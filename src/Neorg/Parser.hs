{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Neorg.Parser where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Arrow (left)
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State
  ( State,
    StateT (runStateT),
    evalStateT,
    get,
    gets,
    modify,
    put,
  )
import Data.Bool (bool)
import Data.Char (isLetter)
import Data.Foldable (foldl')
import Data.Functor (($>), (<&>))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Time (dayOfWeek, defaultTimeLocale, parseTimeM)
import qualified Data.Vector as V
import Data.Vector.Generic.Mutable (clear)
import Data.Void (Void)
import Debug.Trace
import Neorg.Document
import Neorg.Document.Tag (GenerateTagParser, parseTag)
import Neorg.ParsingUtils (embedParser)
import Optics.Core (view, (%~), (&), (.~), (<&>), (?~), (^.))
import Optics.TH (makeLenses)
import Text.Megaparsec (parseErrorPretty)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Text.Megaparsec.Internal (ParsecT (..))

data ParserState = ParserState
  { _parserHeadingLevel :: IndentationLevel,
    _parserMeta :: DocumentMeta
  }
  deriving (Show)

data InlineState = InlineState {_modifierInline :: ModifierInline, _delimitedActive :: Bool} deriving (Show)

data ModifierInline = NoModifier Inline | OpenModifier Text Inline ModifierInline deriving (Show)

hasModifier :: Text -> ModifierInline -> Bool
hasModifier c (NoModifier _) = False
hasModifier c1 (OpenModifier c2 i b) = c1 == c2 || hasModifier c1 b

type Parser = ParsecT Void Text (State ParserState)

makeLenses ''ParserState

makeLenses ''InlineState

makeLenses ''ModifierInline

initialInlineState :: InlineState
initialInlineState = InlineState (NoModifier (ConcatInline V.empty)) False

defaultParserState :: ParserState
defaultParserState = ParserState I0 emptyDocumentMeta

parse :: GenerateTagParser tags => Text -> Text -> Either Text (Document tags)
parse fileName fileContent =
  left (pack . P.errorBundlePretty) $
    runIdentity $
      flip evalStateT defaultParserState $
        P.runParserT document (unpack fileName) fileContent

document :: GenerateTagParser tags => Parser (Document tags)
document =
  Document <$> blocks

blocks :: GenerateTagParser tags => Parser (Blocks tags)
blocks = do
  blocks <- P.many singleBlock
  pure $ V.fromList $ mconcat blocks

singleBlock :: GenerateTagParser tags => Parser [Block tags]
singleBlock = do
  consumingTry $ do
    clearBlankSpace
    P.notFollowedBy P.eof
  P.lookAhead anyChar >>= \case
    -- ' ' -> clearBlankSpace >> singleBlock
    c -> do
      s <- isSpecialLineStart
      if s
        then specialLineStart c
        else pure . Paragraph <$> paragraph

specialLineStart :: GenerateTagParser tags => Char -> Parser [Block tags]
specialLineStart = \case
  '*' -> headingWithDelimiter
  '-' -> pure . List . TaskList <$> taskList I0 <|> pure . List . UnorderedList <$> unorderedList I0 <|> weakDelimiter $> [Delimiter WeakDelimiter]
  '=' -> strongDelimiter $> [Delimiter StrongDelimiter]
  '>' -> pure . Quote <$> quote
  '~' -> pure . List . OrderedList <$> orderedList I0
  '_' -> horizonalLine $> pure (Delimiter HorizonalLine)
  '|' -> pure . Marker <$> marker
  '@' -> maybeToList . fmap Tag <$> tag
  _ -> fail "Not one of: Heading, Delimiter, Quote, List, Horizontal Line, Tag, Marker"

isSpecialLineStart :: P.MonadParsec e Text p => p Bool
isSpecialLineStart = test $> False <|> pure True
  where
    test =
      lookChar >>= \case
        '*' -> P.notFollowedBy (repeatingLevel '*' >> singleSpace)
        '-' ->
          P.notFollowedBy $
            repeating '-' >>= \n ->
              if
                  | n < 3 -> singleSpace
                  | n < 7 -> singleSpace <|> P.hspace >> void newline
                  | otherwise -> P.hspace >> void newline
        '=' -> P.notFollowedBy (repeating '=' >> P.hspace >> newline)
        '>' -> P.notFollowedBy (repeatingLevel '>' >> singleSpace)
        '~' -> P.notFollowedBy (repeatingLevel '~' >> singleSpace)
        '$' -> P.notFollowedBy (anyChar >> singleSpace)
        '_' -> P.notFollowedBy (repeating '_' >>= guard . (> 2) >> P.hspace >> newline)
        '|' -> P.notFollowedBy (P.char '|' >> singleSpace >> P.hspace >> textWord)
        '@' -> P.notFollowedBy (void (P.string "@end") <|> (anyChar >> anyChar >>= guard . isLetter))
        _ -> pure ()

tag :: forall tags. GenerateTagParser tags => Parser (Maybe (SomeTag tags))
tag = do
  tagName <- P.try $ do
    t <- P.char '@' >> P.takeWhileP (Just "Tag description") (\c -> isLetter c || c == '.')
    guard (t /= "end")
    pure t
  P.hspace
  P.choice
    [ P.eof >> pure Nothing,
      do
        let textContent =
              P.takeWhileP (Just "Tag content") (/= '@') >>= \t ->
                (P.try (P.string "@end") >> pure t) <|> (P.char '@' >> fmap ((t <> T.pack ['@']) <>) textContent)
        content <- textContent
        case parseTag @tags tagName of
          Nothing -> pure Nothing
          Just tagParser -> Just <$> embedParser tagParser content
    ]

horizonalLine :: Parser ()
horizonalLine = P.try $ repeating '_' >>= guard . (> 2) >> P.hspace >> lNewline

unorderedList :: IndentationLevel -> Parser UnorderedList
unorderedList l = makeListParser l '-' singleSpace $ \l v -> UnorderedListCons {_uListLevel = l, _uListItems = fmap snd v}

orderedList :: IndentationLevel -> Parser OrderedList
orderedList l = makeListParser l '~' singleSpace $ \l v -> OrderedListCons {_oListLevel = l, _oListItems = fmap snd v}

taskList :: IndentationLevel -> Parser TaskList
taskList l = makeListParser l '-' (singleSpace >> parseTask) $ \l v -> TaskListCons {_tListLevel = l, _tListItems = v}
  where
    parseTask = do
      P.char '['
      status <- P.char ' ' $> TaskUndone <|> P.char 'x' $> TaskDone <|> P.char '*' $> TaskPending
      P.char ']'
      P.char ' '
      pure status

makeListParser :: IndentationLevel -> Char -> Parser a -> (IndentationLevel -> V.Vector (a, V.Vector ListBlock) -> l) -> Parser l
makeListParser minLevel c p f = do
  (level, a) <- P.try $ repeatingLevel c >>= \l -> guard (minLevel <= l) >> p <&> (l,)
  items1 <- P.hspace >> listBlock level
  itemsN <- many $ listItem level
  pure $ f level $ V.fromList ((a, items1) : itemsN)
  where
    listItem level = do
      (_, a) <- P.try $ P.hspace >> repeatingLevel c >>= \l -> guard (level == l) >> p <&> (l,)

      items <- listBlock level
      pure (a, items)
    listBlock :: IndentationLevel -> Parser (V.Vector ListBlock)
    listBlock currentLevel = do
      par <- singleLineParagraph
      subLists <-
        many $
          P.try $
            P.hspace >> lookChar >>= \case
              '~' -> SubList . OrderedList <$> orderedList (succ currentLevel)
              '-' -> SubList <$> (UnorderedList <$> unorderedList (succ currentLevel) <|> TaskList <$> taskList (succ currentLevel))
              _ -> fail "no sublist left"
      pure $ V.fromList $ ListParagraph par : subLists

weakDelimiter :: Parser ()
weakDelimiter = do
  P.try (repeating '-' >> P.hspace >> newline)
  lift $ modify $ parserHeadingLevel %~ pred

strongDelimiter :: Parser ()
strongDelimiter = do
  P.try (repeating '=' >> P.hspace >> newline)
  lift $ modify $ parserHeadingLevel .~ I0

quote :: Parser Quote
quote =
  P.try (repeatingLevel '>' >-> singleSpace) >>= \l ->
    singleLineParagraph <&> \c -> QuoteCons {_quoteLevel = l, _quoteContent = c}

marker :: Parser Marker
marker = do
  first <- P.try $ P.char '|' >> singleSpace >> P.hspace >> textWord
  rest <- P.many $ P.hspace >> textWord
  let markerId = foldl' (\acc r -> acc <> "-" <> T.toLower r) (T.toLower first) rest
  let markerText = foldl' (\acc r -> acc <> " " <> T.toLower r) first rest
  pure $ MarkerCons {_markerId = markerId, _markerText = markerText}

localState :: (ParserState -> ParserState) -> Parser a -> Parser a
localState f p =
  lift (get >-> modify f) >>= \s ->
    p >-> lift (put s)

clearBlankSpace :: P.MonadParsec e Text p => p ()
clearBlankSpace = void $ P.takeWhileP (Just "Clearing whitespace") (<= ' ')

headingWithDelimiter :: GenerateTagParser tags => Parser [Block tags]
headingWithDelimiter = do
  currentLevel <- lift $ gets (view parserHeadingLevel)
  h@(HeadingCons t headingLevel) <- heading
  let delimiters = case (currentLevel, headingLevel) of
        (I0, I0) -> []
        (I1, I0) -> [Delimiter WeakDelimiter]
        (_, I0) -> [Delimiter StrongDelimiter]
        -- _ -> replicate (fromEnum currentLevel - fromEnum headingLevel) (Delimiter WeakDelimiter)
        _ -> [Delimiter WeakDelimiter | currentLevel > headingLevel]
  pure $ delimiters ++ [Heading h]

heading :: Parser Heading
heading = do
  (level, text) <- headingText
  lift $ modify $ parserHeadingLevel .~ succ level
  pure $ HeadingCons text level
  where
    headingText = do
      level <- P.try $ do
        repeatingLevel '*' >-> singleSpace
      headingText <- singleLineParagraph
      pure (level, headingText)

paragraph :: (MonadFail p, P.MonadParsec Void Text p) => p Inline
paragraph = runInline $ do
  modify $ delimitedActive .~ False
  paragraph' $
    doubleNewline
      <|> P.try
        ( do
            gets (view delimitedActive) >>= guard
            isSpecialLineStart >>= guard
        )

singleLineParagraph :: (MonadFail p, P.MonadParsec Void Text p) => p Inline
singleLineParagraph = runInline $ do
  modify $ delimitedActive .~ False
  paragraph' (void newline)

runInline :: P.MonadParsec e Text p => StateT InlineState p () -> p Inline
runInline p = fmap canonalizeInline $ do
  (_, inlineState) <- runStateT p initialInlineState
  pure $ reduceModifierInline $ inlineState ^. modifierInline
  where
    reduceModifierInline :: ModifierInline -> Inline
    reduceModifierInline (NoModifier i) = i
    reduceModifierInline (OpenModifier c i b) = ConcatInline $ V.fromList [reduceModifierInline b, Text c, i]

paragraph' :: forall p. (MonadFail p, P.MonadParsec Void Text p) => StateT InlineState p () -> StateT InlineState p ()
paragraph' end' =
  (end >> pure ())
    <|> (lookChar >>= \c -> intersectingModifier c <|> attachedOpenings c <|> word c)
  where
    end =
      end' <|> P.eof
    appendInlineToStack :: Inline -> StateT InlineState p ()
    appendInlineToStack t =
      modify $
        modifierInline %~ \case
          NoModifier i -> NoModifier $ i <> t
          OpenModifier c i b -> OpenModifier c (i <> t) b
    popStack :: Text -> (Inline -> Inline) -> StateT InlineState p ()
    popStack c f = do
      s <- gets (view modifierInline)
      new <- close s
      modify $ modifierInline .~ new
      where
        close (NoModifier b) = fail "No closing"
        close (OpenModifier cm i b) =
          case b of
            (OpenModifier cd id bd) -> if c == cm then pure (OpenModifier cd (id <> f i) bd) else close (OpenModifier cd (id <> Text cm <> i) bd)
            (NoModifier id) -> if c == cm then pure (NoModifier (id <> f i)) else fail "No closing"
    pushStack :: Text -> StateT InlineState p ()
    pushStack c = do
      s <- gets (view modifierInline)
      new <- case s of
        NoModifier i -> pure $ OpenModifier c mempty (NoModifier i)
        stack@(OpenModifier cm i b) ->
          if not $ hasModifier c stack
            then pure $ OpenModifier c mempty stack
            else fail "No open modifier"
      modify $ modifierInline .~ new

    parseTextModifier :: StateT InlineState p () -> Text -> (Text -> Inline) -> StateT InlineState p ()
    parseTextModifier follow char f = P.string char >> P.try (go "") <|> word (T.head char)
      where
        go :: Text -> StateT InlineState p ()
        go previousText = do
          text <- P.takeWhileP (Just "Inline Text modifier") (\c -> c /= T.last char && c /= '\n' && c /= '\r')
          let fullText = previousText <> text
          ( do
              P.string $ T.reverse char
              followedBy follow
              appendInlineToStack (f fullText)
              modify (delimitedActive .~ False)
              withNextChar $ \c -> parWhitespace c <|> attachedClosings c <|> attachedOpenings c <|> word c
            )
            <|> (newline >> P.hspace >> newline >> fail "No Text modifier")
            <|> (newline >> P.hspace >> go fullText)

    attachedOpenings :: Char -> StateT InlineState p ()
    attachedOpenings = \case
      '*' -> parseOpening "*"
      '/' -> parseOpening "/"
      '_' -> parseOpening "_"
      '-' -> parseOpening "-"
      ',' -> parseOpening ","
      '|' -> parseOpening "|"
      '^' -> parseOpening "^"
      '`' -> parseTextModifier follow "`" Verbatim
      '$' -> parseTextModifier follow "$" Math
      -- '=' -> parseOpening TODO: Behavior unclear
      _ -> fail "No attachedOpenings"
      where
        follow =
          singleSpace <|> lNewline
            <|> withNextChar
              (guard . flip S.member (punctuationSymbols <> attachedModifierSymbols))
        parseOpening c = do
          P.try $ do
            anyChar >> withNextChar (\c -> guard $ isLetter c || S.member c specialSymbols)
            pushStack c
          modify (delimitedActive .~ False)
          withNextChar (\c -> P.choice [attachedOpenings c, word c])

    attachedClosings :: Char -> StateT InlineState p ()
    attachedClosings = \case
      '*' -> parseClosing "*" Bold
      '/' -> parseClosing "/" Italic
      '_' -> parseClosing "_" Underline
      '-' -> parseClosing "-" Strikethrough
      '^' -> parseClosing "^" Superscript
      ',' -> parseClosing "," Subscript
      '|' -> parseClosing "|" Spoiler
      _ -> fail "No attached closings"
      where
        parseClosing c f = do
          P.try $ do
            P.string c
              >> followedBy
                ( singleSpace <|> lNewline
                    <|> withNextChar
                      (guard . flip S.member (punctuationSymbols <> attachedModifierSymbols))
                )
            popStack c f
          modify (delimitedActive .~ False)
          withNextChar $ \c -> parWhitespace c <|> attachedClosings c <|> attachedOpenings c <|> word c

    word :: Char -> StateT InlineState p ()
    word c = do
      modify (delimitedActive .~ False)
      if S.member c (punctuationSymbols <> attachedModifierSymbols)
        then
          ( P.try
              ( do
                  guard (c == '\\')
                  a <- anyChar >> anyChar
                  guard (a > ' ')
                  pure a
              )
              >>= \x -> do
                appendInlineToStack
                  (Text $ pack [x])
                withNextChar $
                  \c -> parWhitespace c <|> attachedClosings c <|> word c
          )
            <|> ( do
                    p <- lift anyChar <&> pack . (: [])
                    appendInlineToStack (Text p)
                    withNextChar $ \c -> parWhitespace c <|> attachedClosings c <|> attachedOpenings c <|> word c
                )
        else
          ( do
              w <- P.takeWhile1P (Just "Word") (\c -> c > ' ' && S.notMember c (punctuationSymbols <> attachedModifierSymbols))
              appendInlineToStack (Text w)
              withNextChar $ \c -> parWhitespace c <|> attachedClosings c <|> word c
          )

    punctuationSymbols = S.fromList "?!:;,.<>()[]{}'\"/#%&$£€-*\\~"
    attachedModifierSymbols = S.fromList "*/_-^,|`$="

    specialSymbols = attachedModifierSymbols <> punctuationSymbols

    withNextChar :: (Char -> StateT InlineState p ()) -> StateT InlineState p ()
    withNextChar f = end <|> (lookChar >>= f)

    intersectingModifier :: Char -> StateT InlineState p ()
    intersectingModifier c1 = do
      c2 <- followedBy $ anyChar >> anyChar
      case c1 : [c2] of
        ":*" -> intersectingOpen ":*"
        ":/" -> intersectingOpen ":/"
        ":_" -> intersectingOpen ":_"
        ":-" -> intersectingOpen ":-"
        ":^" -> intersectingOpen ":^"
        ":," -> intersectingOpen ":,"
        ":|" -> intersectingOpen ":|"
        ":`" -> parseTextModifier (pure ()) ":`" Verbatim
        ":$" -> parseTextModifier (pure ()) ":$" Math
        "*:" -> intersectingClosed ":*" Bold
        "/:" -> intersectingClosed ":/" Italic
        "_:" -> intersectingClosed ":_" Underline
        "-:" -> intersectingClosed ":-" Strikethrough
        "^:" -> intersectingClosed ":^" Superscript
        ",:" -> intersectingClosed ":," Subscript
        "|:" -> intersectingClosed ":|" Spoiler
        s -> fail "No intersecting modifier"
      where
        intersectingClosed mod f = do
          P.string $ T.reverse mod
          popStack mod f
          next
        intersectingOpen mod = do
          P.string mod
          pushStack mod
          next
        next = do
          modify (delimitedActive .~ True)
          withNextChar $ \c -> parWhitespace c <|> attachedClosings c <|> attachedOpenings c <|> word c

    parWhitespace :: Char -> StateT InlineState p ()
    parWhitespace c =
      intersectingModifier c <|> case c of
        ' ' -> do
          appendInlineToStack Space
          next
        '~' -> do
          P.try (anyChar >> P.hspace >> newline)
          next
        '\n' -> do
          lNewline
          modify (delimitedActive .~ True)
          next
        '\r' -> do
          lNewline
          modify (delimitedActive .~ True)
          next
        c -> fail "No newline or space"
      where
        next = do
          P.hspace
          withNextChar (\c -> parWhitespace c <|> attachedOpenings c <|> word c)
    punctuationOrModifier c =
      if S.member c (S.fromList "?!:;,.<>()[]{}'\"/#%&$£€-*\\~" <> S.fromList "*/_-^,|`$=")
        then do
          modify (delimitedActive .~ False)
          anyChar >> withNextChar (\c -> parWhitespace c <|> attachedClosings c <|> attachedOpenings c <|> word c)
        else fail ""

many1 :: (Alternative f) => f a -> f [a]
many1 p = (:) <$> p <*> many p

manyV :: Alternative f => f a -> f (V.Vector a)
manyV = fmap V.fromList . many

lNewline :: P.MonadParsec e Text p => p ()
lNewline = (newline >> P.hspace) <|> P.eof

newline :: P.MonadParsec e Text p => p ()
newline = void P.newline <|> void P.crlf

followedBy :: P.MonadParsec e s p => p a -> p a
followedBy = P.try . P.lookAhead

singleWhitespace :: P.MonadParsec e Text p => p ()
singleWhitespace = P.satisfy (< ' ') $> ()

singleSpace :: P.MonadParsec e Text p => p ()
singleSpace = P.char ' ' $> ()

repeatingLevel :: P.MonadParsec e Text p => Char -> p IndentationLevel
repeatingLevel char = P.try $ P.takeWhile1P (Just $ "repeating " ++ [char]) (== char) <&> toEnum . pred . T.length

repeating :: P.MonadParsec e Text p => Char -> p Int
repeating char = P.try $ P.takeWhile1P (Just $ "repeating " ++ [char]) (== char) <&> T.length

(>->) :: Monad m => m a -> m b -> m a
(>->) ma mb = ma >>= (<$ mb)

infixl 1 >->

lookChar :: P.MonadParsec e Text p => p Char
lookChar = followedBy anyChar

anyChar :: P.MonadParsec e Text p => p Char
anyChar = P.satisfy (const True)

doubleNewline :: P.MonadParsec e Text p => p ()
doubleNewline = P.try $ newline >> P.hspace >> void newline

-- newline = newline <|> crlf

consumingTry :: ParsecT e s m a -> ParsecT e s m a
consumingTry p = ParsecT $ \s cok _ eok eerr ->
  let eerr' err s' = eerr err s'
   in unParser p s cok eerr' eok eerr'
{-# INLINE consumingTry #-}

viewChar :: P.MonadParsec e Text p => p ()
viewChar = do
  !c <- traceShowId <$> lookChar
  pure ()

textWord :: P.MonadParsec e Text p => p Text
textWord = P.takeWhile1P (Just "Text word") (\c -> c /= ' ' && c /= '\n' && c /= '\r')

instance ParseTagContent "code" where
  parseTagContent _ args = P.takeWhileP (Just "Code block") (const True)

instance ParseTagContent "math" where
  parseTagContent _ args = P.takeWhileP (Just "Math block") (const True)

instance ParseTagContent "comment" where
  parseTagContent _ args = P.takeWhileP (Just "Comment block") (const True)

instance ParseTagContent "embed" where
  parseTagContent _ args = case args of
    "image" -> P.hspace >> P.takeWhile1P (Just "Url string") (> ' ')
    a -> fail $ "I do not recognize the embed format " ++ show a

instance ParseTagContent "document.meta" where
  parseTagContent _ _ = do
    clearBlankSpace
    foldl makeDocumentMeta emptyDocumentMeta . catMaybes <$> P.many metaItem
    where
      makeDocumentMeta meta (field, value) = case field of
        "title" -> meta & documentTitle ?~ value
        "description" -> meta & documentDescription ?~ value
        "author" -> meta & documentAuthor ?~ value
        "categories" -> meta & documentCategories .~ V.fromList (T.words value)
        "created" -> meta & documentCreated .~ parseTimeM True defaultTimeLocale "%Y-%-m-%-d" (unpack value)
        "version" -> meta & documentVersion ?~ value
        _ -> meta
      metaItem = do
        field <- P.takeWhileP (Just "meta item") (\c -> c /= ':' && c /= '\n' && c /= '\n')
        P.char ':'
        value <- T.strip <$> P.takeWhileP (Just "meta field") (\c -> c /= '\n' && c /= '\r')
        clearBlankSpace
        case value of
          "" -> pure Nothing
          _ -> pure $ Just (field, value)

instance ParseTagContent "table" where
  parseTagContent _ _ = do
    rows <- manyV row
    pure $ Table rows
    where
      cell = P.notFollowedBy (P.eof <|> void newline) >> cellParagraph >-> P.hspace
      row =
        let inlines = do
              P.hspace
              cells <- manyV cell
              newline
              pure $ TableRowInlines cells
            delimiter = do
              P.char '-'
              P.hspace
              newline
              pure TableRowDelimiter
         in P.try delimiter <|> inlines
      cellParagraph = runInline $ do
        modify $ delimitedActive .~ False
        paragraph' $
          void (P.string " | ")
            <|> P.try
              (P.string " |" >> P.lookAhead (void newline <|> P.eof))
            <|> void (P.lookAhead newline)
