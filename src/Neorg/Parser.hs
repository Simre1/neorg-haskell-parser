{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Neorg.Parser where

import Control.Applicative
import Control.Arrow (left)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Bool (bool)
import Data.Char (isLetter)
import Data.Foldable (foldl')
import Data.Functor
import Data.Functor.Identity
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Time (dayOfWeek, defaultTimeLocale, parseTimeM)
import qualified Data.Vector as V
import Data.Vector.Generic.Mutable (clear)
import Data.Void
import Debug.Trace
import Neorg.Document
import Neorg.Document.Tag (GenerateTagParser, parseTag)
import Neorg.ParsingUtils (embedParser)
import Optics.Core
import Optics.TH
import Text.Megaparsec (parseErrorPretty)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Text.Megaparsec.Internal

data ParserState = ParserState
  { _parserHeadingLevel :: IndentationLevel,
    _parserMeta :: DocumentMeta
  }
  deriving (Show)

data InlineState = InlineState {_modifierInline :: ModifierInline, _delimitedActive :: Bool} deriving (Show)

data ModifierInline = NoModifier Inline | OpenModifier String Inline ModifierInline deriving (Show)

hasModifier c (NoModifier _) = False
hasModifier c1 (OpenModifier c2 i b) = c1 == c2 || hasModifier c1 b

type Parser = ParsecT Void Text (State ParserState)

makeLenses ''ParserState

makeLenses ''InlineState

makeLenses ''ModifierInline

initialInlineState = InlineState (NoModifier (ConcatInline V.empty)) False

defaultParserState = ParserState I0 emptyDocumentMeta

parse :: GenerateTagParser tags => Text -> Text -> Either Text (Document tags)
parse fileName fileContent =
  left (pack . P.errorBundlePretty) $
    runIdentity $
      flip evalStateT defaultParserState $
        P.runParserT document (unpack fileName) fileContent

document :: GenerateTagParser tags => Parser (Document tags)
document = do
  blocks <- blocks
  pure $ Document blocks

blocks :: GenerateTagParser tags => Parser (Blocks tags)
blocks = do
  blocks <- P.many $ singleBlock
  pure $ V.fromList $ catMaybes blocks

singleBlock :: GenerateTagParser tags => Parser (Maybe (Block tags))
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
        else Just . Paragraph <$> paragraph

specialLineStart :: GenerateTagParser tags => Char -> Parser (Maybe (Block tags))
specialLineStart = \case
  '*' -> pure . Heading <$> heading
  '-' -> pure . List . TaskList <$> taskList I0 <|> pure . List . UnorderedList <$> unorderedList I0 <|> weakDelimiter $> Nothing
  '=' -> strongDelimiter $> Nothing
  '>' -> pure . Quote <$> quote
  '~' -> pure . List . OrderedList <$> orderedList I0
  '_' -> horizonalLine $> pure HorizonalLine
  '|' -> pure . Marker <$> marker
  '@' -> fmap Tag <$> tag
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
                  | n < 7 -> singleSpace <|> P.hspace >> void P.newline
                  | otherwise -> P.hspace >> void P.newline
        '=' -> P.notFollowedBy (repeating '=' >> P.hspace >> P.newline)
        '>' -> P.notFollowedBy (repeatingLevel '>' >> singleSpace)
        '~' -> P.notFollowedBy (repeatingLevel '~' >> singleSpace)
        '$' -> P.notFollowedBy (anyChar >> singleSpace)
        '_' -> P.notFollowedBy (repeating '_' >>= guard . (> 2) >> P.hspace >> P.newline)
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
horizonalLine = P.try $ repeating '_' >>= guard . (> 2) >> P.hspace >> newline

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
  level <- lift $ gets (view parserHeadingLevel)
  if level == I0
    then void $ P.try (repeating '-' >> P.hspace >> P.newline)
    else consumingTry $ P.try (repeating '-' >> P.hspace >> P.newline) >> lift (modify $ parserHeadingLevel %~ pred) >> fail "End current heading scope"

strongDelimiter :: Parser ()
strongDelimiter = do
  level <- lift $ gets (view parserHeadingLevel)
  if level == I0
    then void $ P.try (repeating '=' >> P.hspace >> P.newline)
    else followedBy (repeating '=' >> P.hspace >> P.newline >> lift (modify $ parserHeadingLevel %~ pred)) >> fail "End current heading scope"

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

heading :: GenerateTagParser tags => Parser (Heading tags)
heading = do
  (level, text) <- headingText
  content <- localState (parserHeadingLevel %~ succ) blocks
  pure $ HeadingCons text level content
  where
    headingText = do
      level <- P.try $ do
        level <- repeatingLevel '*' >-> singleSpace
        lift (gets $ view parserHeadingLevel) >>= guard . (level >=)
        pure level
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
  paragraph' (void P.newline)

runInline :: P.MonadParsec e Text p => StateT InlineState p () -> p Inline
runInline p = fmap canonalizeInline $ do
  (_, inlineState) <- runStateT p initialInlineState
  pure $ reduceModifierInline $ inlineState ^. modifierInline
  where
    reduceModifierInline :: ModifierInline -> Inline
    reduceModifierInline (NoModifier i) = i
    reduceModifierInline (OpenModifier c i b) = ConcatInline $ V.fromList [reduceModifierInline b, Text $ pack c, i]

paragraph' :: forall p. (MonadFail p, P.MonadParsec Void Text p) => StateT InlineState p () -> StateT InlineState p ()
paragraph' end' = do
  (end >> pure ())
    <|> (lookChar >>= \c -> openings c <|> word c)
  where
    end = do
      end' <|> P.eof
    appendInlineToStack :: Inline -> StateT InlineState p ()
    appendInlineToStack t =
      modify $
        modifierInline %~ \case
          NoModifier i -> NoModifier $ i <> t
          OpenModifier c i b -> OpenModifier c (i <> t) b

    openings :: Char -> StateT InlineState p ()
    openings = \case
      '*' -> parseOpening "*"
      '/' -> parseOpening "/"
      '_' -> parseOpening "_"
      '-' -> parseOpening "-"
      '^' -> parseOpening "^"
      ',' -> parseOpening ","
      '|' -> parseOpening "|"
      '`' -> parseTextModifier "`" Verbatim
      '$' -> parseTextModifier "$" Math
      -- '=' -> parseOpening TODO: Behavior unclear
      _ -> fail "No openings"
      where
        parseTextModifier :: Text -> (Text -> Inline) -> StateT InlineState p ()
        parseTextModifier char f = P.try (go "") <|> word (T.head char)
          where
            go :: Text -> StateT InlineState p ()
            go previousText = do
              P.string char
              text <- P.takeWhileP (Just "Inline Text modifier") (\c -> c /= T.head char && c /= '\n')
              let fullText = previousText <> text
              (P.string char >> appendInlineToStack (f fullText)) <|> (P.newline >> P.hspace >> P.newline >> fail "No Text modifier") <|> (P.newline >> P.hspace >> go fullText)
        pushStack c = do
          s <- gets (view modifierInline)
          new <- case s of
            NoModifier i -> pure $ OpenModifier c mempty (NoModifier i)
            stack@(OpenModifier cm i b) ->
              if not $ hasModifier c stack
                then pure $ OpenModifier c mempty stack
                else fail "No open modifier"
          modify $ modifierInline .~ new
        parseOpening c = do
          P.try $ do
            anyChar >> withNextChar (\c -> guard $ isLetter c || S.member c specialSymbols)
            pushStack c
          modify (delimitedActive .~ False)
          withNextChar (\c -> P.choice [parNewline c, openings c, word c])

    space :: Char -> StateT InlineState p ()
    space = \case
      ' ' -> do
        P.hspace >> appendInlineToStack Space
        withNextChar $ \c -> parNewline c <|> openings c <|> word c
      _ -> fail "No space"

    closings :: Char -> StateT InlineState p ()
    closings = \case
      '*' -> parseClosing "*" Bold
      '/' -> parseClosing "/" Italic
      '_' -> parseClosing "_" Underline
      '-' -> parseClosing "-" Strikethrough
      '^' -> parseClosing "^" Superscript
      ',' -> parseClosing "," Subscript
      '|' -> parseClosing "|" Spoiler
      _ -> fail "No closings"
      where
        parseClosing c f = do
          P.try $ do
            anyChar >> followedBy (singleSpace <|> newline <|> withNextChar punctuationOrModifier)
            popStack c f
          modify (delimitedActive .~ False)
          withNextChar $ \c -> parNewline c <|> closings c <|> openings c <|> space c <|> punctuationOrModifier c

        popStack :: String -> (Inline -> Inline) -> StateT InlineState p ()
        popStack c f = do
          s <- gets (view modifierInline)
          new <- close s
          modify $ modifierInline .~ new
          where
            close (NoModifier b) = fail "No closing"
            close (OpenModifier cm i b) =
              case b of
                (OpenModifier cd id bd) -> if c == cm then pure (OpenModifier cd (id <> f i) bd) else close (OpenModifier cd (id <> Text (pack cm) <> i) bd)
                (NoModifier id) -> if c == cm then pure (NoModifier (id <> f i)) else fail "No closing"

    word :: Char -> StateT InlineState p ()
    word c = do
      modify (delimitedActive .~ False)
      if S.member c (punctuationSymbols <> attachedModifierSymbols)
        then
          ( do
              p <- lift anyChar <&> pack . (: [])
              appendInlineToStack (Text p)
              withNextChar $ \c -> space c <|> parNewline c <|> closings c <|> openings c <|> word c
          )
        else
          ( do
              w <- P.takeWhile1P (Just "Word") (\c -> c > ' ' && S.notMember c (punctuationSymbols <> attachedModifierSymbols))
              appendInlineToStack (Text w)
              withNextChar $ \c -> space c <|> parNewline c <|> closings c <|> word c
          )
    punctuationSymbols = S.fromList "?!:;,.<>()[]{}'\"/#%&$£€-*\\~"
    attachedModifierSymbols = S.fromList "*/_-^,|`$="

    specialSymbols = attachedModifierSymbols <> punctuationSymbols

    withNextChar :: (Char -> StateT InlineState p ()) -> StateT InlineState p ()
    withNextChar f = end <|> (lookChar >>= f)

    parNewline :: Char -> StateT InlineState p ()
    parNewline = \case
      '~' -> do
        P.try (anyChar >> P.hspace >> P.newline)
        next
      '\n' -> do
        newline
        modify (delimitedActive .~ True)
        next
      _ -> fail "No newline"
      where
        next = do
          P.hspace
          withNextChar (\c -> openings c <|> word c)
    punctuationOrModifier c = do
      if S.member c (S.fromList "?!:;,.<>()[]{}'\"/#%&$£€-*\\~" <> S.fromList "*/_-^,|`$=")
        then do
          modify (delimitedActive .~ False)
          anyChar >> withNextChar (\c -> space c <|> parNewline c <|> closings c <|> openings c <|> word c)
        else fail ""

many1 p = p >>= \a -> (a :) <$> many p

manyV :: Alternative f => f a -> f (V.Vector a)
manyV = fmap V.fromList . many

newline :: P.MonadParsec e Text p => p ()
newline = (P.newline >> P.hspace) <|> P.eof

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
doubleNewline = P.try $ P.newline >> P.hspace >> void P.newline

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
        field <- P.takeWhileP (Just "meta item") (\c -> c /= ':' && c /= '\n')
        P.char ':'
        value <- T.strip <$> P.takeWhileP (Just "meta field") (/= '\n')
        clearBlankSpace
        case value of
          "" -> pure Nothing
          _ -> pure $ Just (field, value)

instance ParseTagContent "table" where
  parseTagContent _ _ = do
    rows <- manyV row
    pure $ Table rows
    where
      cell = P.notFollowedBy (P.eof <|> void P.newline) >> cellParagraph >-> P.hspace
      row =
        let inlines = do
              P.hspace
              cells <- manyV cell
              P.newline
              pure $ TableRowInlines cells
            delimiter = do
              P.char '-'
              P.hspace
              P.newline
              pure TableRowDelimiter
         in P.try delimiter <|> inlines
      cellParagraph = runInline $ do
        modify $ delimitedActive .~ False
        paragraph' $ void (P.string " | ") <|> (P.try $ P.string " |" >> P.lookAhead (void P.newline <|> P.eof)) <|> void (P.lookAhead P.newline)
