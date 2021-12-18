module Neorg.Parser.Block where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (get, gets, modify, put)
import Data.Char (isLetter)
import Data.Foldable (Foldable (foldl'))
import Data.Functor (($>))
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Neorg.Document
import Neorg.Document.Tag
import Neorg.Parser.Paragraph
import Neorg.Parser.Types
import Neorg.Parser.Utils
import Optics.Core (view, (%~), (.~), (<&>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

-- block :: GenerateTagParser tags => Parser [Block tags]
-- block = do
--   consumingTry $ do
--     clearBlankSpace
--     P.notFollowedBy P.eof
--   P.lookAhead anyChar >>= \case
--     c -> do
--       s <- isSpecialLineStart
--       if s
--         then specialLineStart c
--         else pure . Paragraph <$> paragraph
--

pureBlockWithoutParagraph :: GenerateTagParser tags => Parser (Maybe (PureBlock tags))
pureBlockWithoutParagraph =
  lookChar >>= \case
    '-' -> pure . List . TaskList <$> taskList I0 <|> pure . List . UnorderedList <$> unorderedList I0
    '>' -> pure . Quote <$> quote
    '~' -> pure . List . OrderedList <$> orderedList I0
    '@' -> fmap Tag <$> tag
    _ -> fail "Not a pure block"

pureBlock :: GenerateTagParser tags => Parser (Maybe (PureBlock tags))
pureBlock = pureBlockWithoutParagraph <|> pure . Paragraph <$> paragraph

blocks :: GenerateTagParser tags => Parser (Blocks tags)
blocks = do
  blocks <- P.many $ do
    consumingTry $ do
      clearBlankSpace
      P.notFollowedBy P.eof
    block
  pure $ V.fromList $ mconcat blocks

block :: GenerateTagParser tags => Parser [Block tags]
block = do
  isMarkup <- isMarkupElement
  if isMarkup
    then impureBlock <|> maybeToList . fmap PureBlock <$> pureBlockWithoutParagraph
    else pure . PureBlock . Paragraph <$> paragraph
  where
    impureBlock =
      lookChar >>= \case
        '-' -> weakDelimiter $> [Delimiter WeakDelimiter]
        '=' -> strongDelimiter $> [Delimiter StrongDelimiter]
        '_' -> horizonalLine $> pure (Delimiter HorizonalLine)
        '|' -> pure . Marker <$> marker
        '*' -> headingWithDelimiter
        _ -> fail "Not a delimiter and not a heading"

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
