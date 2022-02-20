module Neorg.Parser.Block where

import Cleff
import Cleff.Reader
import Cleff.State
import Control.Applicative (Alternative (many, (<|>)))
import Control.Monad (guard, void)
import Control.Monad.Trans.Class
import Data.Char (isLetter)
import Data.Foldable (Foldable (fold, foldl'))
import Data.Functor (($>), (<&>))
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.Text as T
import qualified Data.Vector as V
import Effect.Logging
import Neorg.Document
import Neorg.Parser.Paragraph
import Neorg.Parser.Tags.Classes
import Neorg.Parser.Types
import Neorg.Parser.Utils
import qualified Text.Builder as TB
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Debug.Trace

newtype CurrentListLevel = CurrentListLevel IndentationLevel deriving newtype (Show, Eq, Ord, Enum)

newtype CurrentHeadingLevel = CurrentHeadingLevel IndentationLevel deriving newtype (Show, Eq, Ord, Enum)

pureBlockWithoutParagraph :: (Logging :> es, Reader CurrentListLevel :> es, GenerateTagParser tags) => Parser es (Maybe (PureBlock tags))
pureBlockWithoutParagraph = do
  lookChar >>= \case
    '-' -> pure . List . TaskList <$> taskList <|> pure . List . UnorderedList <$> unorderedList
    '>' -> pure . Quote <$> quote
    '~' -> pure . List . OrderedList <$> orderedList
    '@' -> fmap Tag <$> tag
    _ -> fail "Not a pure block"

pureBlock :: (GenerateTagParser tags, Logging :> es, Reader CurrentListLevel :> es) => Parser es (Maybe (PureBlock tags))
pureBlock = do
  markupElement <- isMarkupElement
  if markupElement
    then pureBlockWithoutParagraph
    else pure . Paragraph <$> paragraph

blocks :: (GenerateTagParser tags, Logging :> es, State CurrentHeadingLevel :> es) => Parser es (Blocks tags)
blocks = do
  blocks' <- P.many $ do
    P.try $ do
      clearBlankSpace
      P.notFollowedBy P.eof
    block
  pure $ V.fromList $ mconcat blocks'

block :: (GenerateTagParser tags, Logging :> es, State CurrentHeadingLevel :> es) => Parser es [Block tags]
block = do
  isMarkup <- isMarkupElement
  if isMarkup
    then impureBlock <|> maybeToList . fmap PureBlock <$> runParserReader (CurrentListLevel I0) pureBlockWithoutParagraph
    else pure . PureBlock . Paragraph <$> paragraph
  where
    impureBlock =
      lookChar >>= \case
        '-' -> weakDelimiter $> [Delimiter WeakDelimiter]
        '=' -> strongDelimiter $> [Delimiter StrongDelimiter]
        '_' -> horizonalLine $> pure (Delimiter HorizonalLine)
        '|' -> pure . Marker <$> marker
        '*' -> headingWithDelimiter
        '$' -> pure . Definition <$> definition
        _ -> fail "Not a delimiter and not a heading"

tag :: forall tags es. (Logging :> es, GenerateTagParser tags) => Parser es (Maybe (SomeTag tags))
tag = do
  (tagName, startIndent) <- P.try $ do
    startIndent <- P.indentLevel
    t <- P.char '@' >> P.takeWhileP (Just "Tag description") (\c -> isLetter c || c == '.')
    guard (t /= "end")
    pure (t, startIndent)
  P.hspace
  lines <- tagContentLines
  let args = snd $ head lines
      content = tail $ init lines
      minContentIndent = if null content then P.pos1 else minimum $ fst <$> content
      (endIndent, end) = last lines
  if T.null end || endIndent < startIndent || minContentIndent < startIndent -- end == "" when no @end tag was provided and thus the end is at eof
    then pure Nothing
    else
      let processedContent = TB.run $ TB.text args <> TB.char '\n' <>
            foldMap
              ( \(indent, line) ->
                  fold (replicate (P.unPos indent - P.unPos minContentIndent) (TB.char ' ')) <> TB.text line <> TB.char '\n'
              )
              content
       in case parseTag @tags tagName of
            Nothing -> pure Nothing
            Just tagParser -> Just <$> embedParserT tagParser processedContent
  where
    -- P.choice
    --   [ P.eof >> pure Nothing,
    --     do
    --       let textContent =
    --             P.takeWhileP (Just "Tag content") (/= '@') >>= \t ->
    --               (P.try (P.string "@end") >> pure t) <|> (P.char '@' >> fmap ((t <> T.pack ['@']) <>) textContent)
    --       content <- textContent
    --       case parseTag @tags tagName of
    --         Nothing -> pure Nothing
    --         Just tagParser -> Just <$> embedParserT tagParser content
    --   ]

    tagContent :: Parser es ()
    tagContent = do
      args <- P.takeWhileP (Just "Tag arguments") (not . isNewline)

      pure ()
    lineWithIndent :: Parser es (P.Pos, T.Text)
    lineWithIndent = do
      P.hspace
      level <- P.indentLevel
      line <- P.takeWhileP Nothing (not . isNewline)
      newline <|> P.eof
      pure (level, T.strip line)
    tagContentLines :: Parser es [(P.Pos, T.Text)]
    tagContentLines = do
      (indent, line) <- lineWithIndent
      eof <- isEof
      if eof || line == "@end"
        then pure [(indent, line)]
        else ((indent, line) :) <$> tagContentLines

horizonalLine :: Parser p ()
horizonalLine = P.try $ repeating '_' >>= guard . (> 2) >> P.hspace >> lNewline

unorderedList :: (GenerateTagParser tags, Logging :> es, Reader CurrentListLevel :> es) => Parser es (UnorderedList tags)
unorderedList = makeListParser '-' (pure ()) $ \l' v -> UnorderedListCons {_uListLevel = l', _uListItems = fmap snd v}

orderedList :: (GenerateTagParser tags, Logging :> es, Reader CurrentListLevel :> es) => Parser es (OrderedList tags)
orderedList = makeListParser '~' (pure ()) $ \l' v -> OrderedListCons {_oListLevel = l', _oListItems = fmap snd v}

taskList :: (GenerateTagParser tags, Logging :> es, Reader CurrentListLevel :> es) => Parser es (TaskList tags)
taskList = makeListParser '-' parseTask $ \l' v -> TaskListCons {_tListLevel = l', _tListItems = v}
  where
    parseTask = do
      _ <- P.char '['
      status <- P.char ' ' $> TaskUndone <|> P.char 'x' $> TaskDone <|> P.char '*' $> TaskPending
      _ <- P.char ']'
      _ <- P.char ' '
      pure status

makeListParser :: (Reader CurrentListLevel :> es, Logging :> es, GenerateTagParser tags) => Char -> Parser es a -> (IndentationLevel -> V.Vector (a, V.Vector (PureBlock tags)) -> l) -> Parser es l
makeListParser c p f = do
  CurrentListLevel minLevel <- lift ask
  (level, a) <- P.try $ repeatingLevel c >>= \l -> guard (l >= minLevel) >> singleSpace >> p <&> (l,)
  items1 <- P.hspace >> listBlock (CurrentListLevel level)
  itemsN <- many $ listItem level
  pure $ f level $ V.fromList ((a, items1) : itemsN)
  where
    listItem level = do
      (_, a) <- P.try $ P.hspace >> repeatingLevel c >>= \l -> guard (level == l) >> singleSpace >> p <&> (l,)
      items <- listBlock $ CurrentListLevel level
      pure (a, items)
    listBlock :: (GenerateTagParser tags, Logging :> es) => CurrentListLevel -> Parser es (V.Vector (PureBlock tags))
    listBlock currentLevel = do
      pureBlocks <- catMaybes <$> manyOrEnd P.hspace (P.try $ clearBlankSpace >> runParserReader (succ currentLevel) pureBlock) doubleNewline
      pure $ V.fromList pureBlocks

weakDelimiter :: State CurrentHeadingLevel :> es => Parser es ()
weakDelimiter = do
  P.try (repeating '-' >> P.hspace >> newline)
  lift $ modify @CurrentHeadingLevel pred

strongDelimiter :: State CurrentHeadingLevel :> es => Parser es ()
strongDelimiter = do
  P.try (repeating '=' >> P.hspace >> newline)
  lift $ put $ CurrentHeadingLevel I0

quote :: Parser p Quote
quote =
  P.try (repeatingLevel '>' >-> singleSpace) >>= \l ->
    singleLineParagraph <&> \c -> QuoteCons {_quoteLevel = l, _quoteContent = c}

marker :: Parser p Marker
marker = do
  first <- P.try $ P.char '|' >> singleSpace >> P.hspace >> textWord
  rest <- P.many $ P.hspace >> textWord
  let markerId' = foldl' (\acc r -> acc <> "-" <> T.toLower r) (T.toLower first) rest
  let markerText' = foldl' (\acc r -> acc <> " " <> T.toLower r) first rest
  pure $ MarkerCons {_markerId = markerId', _markerText = markerText'}

headingWithDelimiter :: (State CurrentHeadingLevel :> es, GenerateTagParser tags) => Parser es [Block tags]
headingWithDelimiter = do
  CurrentHeadingLevel currentLevel <- lift get
  h@(HeadingCons _ headingLevel') <- heading
  let delimiters = case (currentLevel, headingLevel') of
        (I0, I0) -> []
        (I1, I0) -> [Delimiter WeakDelimiter]
        (_, I0) -> [Delimiter StrongDelimiter]
        -- _ -> replicate (fromEnum currentLevel - fromEnum headingLevel) (Delimiter WeakDelimiter)
        _ -> [Delimiter WeakDelimiter | currentLevel > headingLevel']
  pure $ delimiters ++ [Heading h]

heading :: State CurrentHeadingLevel :> es => Parser es Heading
heading = do
  (level, text) <- headingText'
  lift $ put $ CurrentHeadingLevel $ succ level
  pure $ HeadingCons text level
  where
    headingText' = do
      level <-
        P.try $
          repeatingLevel '*' >-> singleSpace
      headingInline <- singleLineParagraph
      pure (level, headingInline)

definition :: (GenerateTagParser tags, Logging :> es) => Parser es (Definition tags)
definition =
  singleLineDefinition <|> multiLineDefinition
  where
    singleLineDefinition = do
      _ <- P.try $ P.string "$ "
      definitionObject' <- singleLineParagraph
      P.hspace >> newline
      DefinitionCons definitionObject' . V.fromList . maybeToList <$> runParserReader (CurrentListLevel I0) pureBlock
    multiLineDefinition = do
      _ <- P.try $ P.string "$$ "
      definitionObject' <- singleLineParagraph
      pureBlocks <- catMaybes <$> manyOrEnd clearBlankSpace (runParserReader (CurrentListLevel I0) pureBlock) (void (P.string "$$") <|> P.eof)
      pure $ DefinitionCons definitionObject' $ V.fromList pureBlocks
