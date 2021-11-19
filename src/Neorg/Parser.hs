{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Neorg.Parser where

import Control.Applicative
import Control.Arrow (left)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char (isLetter)
import Data.Foldable (foldl')
import Data.Functor
import Data.Functor.Identity
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Void
import Debug.Trace
import Neorg.Document
import Neorg.Document.Tag (GenerateTagParser, parseTag)
import Optics.Core
import Optics.TH
import Text.Megaparsec (parseErrorPretty)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Text.Megaparsec.Internal
import Data.Maybe (catMaybes)
import Data.Time (parseTimeM, defaultTimeLocale)

data ParserState = ParserState
  { _parserHeadingLevel :: IndentationLevel,
    _parserMeta :: DocumentMeta
  }
  deriving (Show)

data InlineState = InlineState {_modifierInline :: ModifierInline, _singleLine :: Bool} deriving (Show)

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
  clearBlankSpace
  blocks <- P.many singleBlock
  pure $ V.fromList blocks

singleBlock :: GenerateTagParser tags => Parser (Block tags)
singleBlock = specialBlock <|> Paragraph <$> paragraph >-> clearBlankSpace
  where
    specialBlock = do
          P.lookAhead anyChar >>= \case
            ' ' -> P.hspace >> singleBlock
            c -> specialLineStart c

specialLineStart :: GenerateTagParser tags => Char -> Parser (Block tags)
specialLineStart = \case
  '*' -> Heading <$> heading
  '-' -> List . TaskList <$> taskList I0 <|> List . UnorderedList <$> unorderedList I0 <|> weakDelimiter
  '=' -> strongDelimiter
  '>' -> Quote <$> quote
  '~' -> List . OrderedList <$> orderedList I0
  '_' -> horizonalLine $> HorizonalLine
  '|' -> Marker <$> marker
  '@' -> tag >>= maybe (clearBlankSpace >> singleBlock) (pure . Tag)
  _ -> fail "Not one of: Heading, Delimiter, Quote, List, Horizontal Line, Tag, Marker"

failOnSpecialLineStart :: P.MonadParsec e Text p => p ()
failOnSpecialLineStart =
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
    [ P.eof >> pure Nothing, do
        let textContent =
              P.takeWhileP (Just "Tag content") (/= '@') >>= \t ->
                (P.try (P.string "@end") >> pure t) <|> (P.char '@' >> fmap ((t <> T.pack ['@']) <>) textContent)
        content <- textContent
        case parseTag @tags tagName of
          Nothing -> pure Nothing
          Just tagParser -> either (fail . P.errorBundlePretty) (pure . Just) $ P.runParser tagParser "Tag" content
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
  items1 <- manyV $ P.hspace >> listBlock level
  itemsN <- many $ listItem level
  pure $ f level $ V.fromList ((a, items1) : itemsN)
  where
    listItem level = do
      (_, a) <- P.try $ P.hspace >> repeatingLevel c >>= \l -> guard (level == l) >> p <&> (l,)
      items <- many $ P.hspace >> listBlock level
      pure (a, V.fromList items)
    listBlock :: IndentationLevel -> Parser ListBlock
    listBlock currentLevel =
      ( P.lookAhead anyChar >>= \case
          '~' -> SubList . OrderedList <$> orderedList (succ currentLevel)
          '-' -> SubList <$> (UnorderedList <$> unorderedList (succ currentLevel) <|> TaskList <$> taskList (succ currentLevel))
          _ -> ListParagraph <$> singleLineParagraph
      )
        <|> ListParagraph
        <$> singleLineParagraph

weakDelimiter :: GenerateTagParser tags => Parser (Block tags)
weakDelimiter = do
  level <- lift $ gets (view parserHeadingLevel)
  if level == I0
    then P.try (repeating '-' >> P.hspace >> P.newline) >> lookChar >>= specialLineStart
    else consumingTry $ P.try (repeating '-' >> P.hspace >> P.newline) >> lift (modify $ parserHeadingLevel %~ pred) >> fail "End current heading scope"

strongDelimiter :: GenerateTagParser tags => Parser (Block tags)
strongDelimiter = do
  level <- lift $ gets (view parserHeadingLevel)
  if level == I0
    then P.try (repeating '=' >> P.hspace >> P.newline) >> lookChar >>= specialLineStart
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

clearBlankSpace ::P.MonadParsec e Text p => p ()
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

paragraph :: Parser Inline
paragraph = do
  fmap canonalizeInline . runInline $ do
    modify $ singleLine .~ False
    paragraph'

singleLineParagraph :: Parser Inline
singleLineParagraph = do
  fmap canonalizeInline . runInline $ do
    modify $ singleLine .~ True
    paragraph'

runInline :: StateT InlineState Parser () -> Parser Inline
runInline p = do
  (_, inlineState) <- runStateT p initialInlineState
  pure $ reduceModifierInline $ inlineState ^. modifierInline
  where
    reduceModifierInline :: ModifierInline -> Inline
    reduceModifierInline (NoModifier i) = i
    reduceModifierInline (OpenModifier c i b) = ConcatInline $ V.fromList [reduceModifierInline b, Text $ pack c, i]

paragraph' :: StateT InlineState Parser ()
paragraph' = do
  failOnSpecialLineStart
  withNextChar $ \c -> openings c <|> word c
  where
    appendInlineToStack :: Inline -> StateT InlineState Parser ()
    appendInlineToStack t =
      modify $
        modifierInline %~ \case
          NoModifier i -> NoModifier $ i <> t
          OpenModifier c i b -> OpenModifier c (i <> t) b

    openings :: Char -> StateT InlineState Parser ()
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
        parseTextModifier :: Text -> (Text -> Inline) -> StateT InlineState Parser ()
        parseTextModifier char f = P.try (go "") <|> word (T.head char)
          where
            go :: Text -> StateT InlineState Parser ()
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
                else fail "No open modifier" -- OpenModifier cm (i <> Text (pack [c])) b
          modify $ modifierInline .~ new
        parseOpening c = do
          P.try $ do
            anyChar >> withNextChar (\c -> guard $ isLetter c || S.member c specialSymbols)
            pushStack c
          withNextChar (\c -> P.choice [parNewline c, openings c, word c])

    space :: Char -> StateT InlineState Parser ()
    space = \case
      ' ' -> do
        P.hspace >> appendInlineToStack Space
        withNextChar $ \c -> parNewline c <|> openings c <|> word c
      _ -> fail "No space"

    closings :: Char -> StateT InlineState Parser ()
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
          withNextChar $ \c -> parNewline c <|> closings c <|> openings c <|> space c <|> punctuationOrModifier c

        popStack :: String -> (Inline -> Inline) -> StateT InlineState Parser ()
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

    word :: Char -> StateT InlineState Parser ()
    word = \case
      c ->
        if
            | S.member c (punctuationSymbols <> attachedModifierSymbols) -> do
              p <- lift anyChar <&> pack . (: [])
              appendInlineToStack (Text p)
              withNextChar $ \c -> space c <|> parNewline c <|> closings c <|> openings c <|> word c
            | otherwise -> do
              w <- P.takeWhile1P (Just "Word") (\c -> c > ' ' && S.notMember c (punctuationSymbols <> attachedModifierSymbols))
              appendInlineToStack (Text w)
              withNextChar $ \c -> space c <|> parNewline c <|> closings c <|> word c
    punctuationSymbols = S.fromList "?!:;,.<>()[]{}'\"/#%&$£€-*\\~"
    attachedModifierSymbols = S.fromList "*/_-^,|`$="

    specialSymbols = attachedModifierSymbols <> punctuationSymbols

    withNextChar :: (Char -> StateT InlineState Parser ()) -> StateT InlineState Parser ()
    withNextChar f = P.eof <|> (lookChar >>= f)

    parNewline :: Char -> StateT InlineState Parser ()
    parNewline = \case
      '~' -> do
        P.try (anyChar >> P.hspace >> P.newline)
        next
      '\n' -> do
        newline
        isSingleLine <- gets (view singleLine)
        unless isSingleLine $ (failOnSpecialLineStart >> next) <|> pure ()
      _ -> fail "No newline"
      where
        next = do
          P.hspace
          let break c = guard (c == '\n' || c == '\r')
          withNextChar (\c -> break c <|> openings c <|> word c)
    punctuationOrModifier c = do
      if S.member c (S.fromList "?!:;,.<>()[]{}'\"/#%&$£€-*\\~" <> S.fromList "*/_-^,|`$=")
        then anyChar >> withNextChar (\c -> space c <|> parNewline c <|> closings c <|> openings c <|> word c)
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

consumingTry :: ParsecT e s m a -> ParsecT e s m a
consumingTry p = ParsecT $ \s cok _ eok eerr ->
  let eerr' err s' = eerr err s'
   in unParser p s cok eerr' eok eerr'
{-# INLINE consumingTry #-}

textWord :: P.MonadParsec e Text p => p Text
textWord = P.takeWhile1P (Just "Text word") (\c -> c /= ' ' && c /= '\n' && c /= '\r')

-- mapParserState :: (s1 -> s2) -> Parser s1 a -> Parser s2 a
-- mapParserState f p1 = do
--   parserState <- getParserState
--   state <- lift get
--   let (s,r) = runIdentity $ runStateT (runParserT' p1 parserState) state
--   a <- ParsecT $ \_ _ handleError _ _ -> either handleError pure r
--   pure a

instance ParseTagContent "code" where
  parseTagContent _ args = P.takeWhileP (Just "Code block") (const True)

instance ParseTagContent "math" where
  parseTagContent _ args = P.takeWhileP (Just "Math block") (const True)

instance ParseTagContent "comment" where
  parseTagContent _ args = P.takeWhileP (Just "Comment block") (const True)

instance ParseTagContent "embed" where
  parseTagContent _ args = case args of
    "image" -> P.hspace >> P.takeWhile1P (Just "Url string") (>' ')
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

