module Neorg.Parser.Block where

import Control.Monad
import Neorg.Document hiding (taskStatus)
import Neorg.Parser.Base
import Neorg.Parser.Combinators
import Neorg.Parser.Paragraph (paragraph, paragraphSegment)
import Text.Megaparsec

blocks :: Parser Blocks
blocks = blocks' 0

blocks' :: Int -> Parser Blocks
blocks' envHeadingLevel = Blocks <$> many block
  where
    block =
      choice
        [ Heading <$> heading envHeadingLevel,
          PureBlock <$> pureBlock (PureBlockEnv 0 0)
        ]

data PureBlockEnv = PureBlockEnv
  { listLevel :: Int,
    quoteLevel :: Int
  }

pureBlock :: PureBlockEnv -> Parser PureBlock
pureBlock pureBlockEnv = choice [List <$> list pureBlockEnv, Quote <$> quote pureBlockEnv, Paragraph <$> paragraph]

pureBlocks :: PureBlockEnv -> Parser PureBlocks
pureBlocks pureBlockEnv = PureBlocks <$> takeUntil pureBlockBreak (pureBlock pureBlockEnv)

pureBlockBreak :: Parser ()
pureBlockBreak = choice [eof, linesOfWhitespace >>= guard . (>= 2), atBeginningOfLine >>= guard >> impureDetachedModifierStart]

impureDetachedModifierStart :: Parser ()
impureDetachedModifierStart = lookAhead $ do
  choice $ flip fmap impureDetachedModifierSymbols $ void . detachedModifier

impureDetachedModifierSymbols :: String
impureDetachedModifierSymbols = "*"

quote :: PureBlockEnv -> Parser Quote
quote pureBlockEnv = do
  notFollowedBy pureBlockBreak
  level <- try $ do
    level <- quotePrefix
    guard $ level > quoteLevel pureBlockEnv
    pure level
  status <- taskStatus
  content <- pureBlocks (pureBlockEnv {quoteLevel = level})
  pure $ QuoteCons level status content
  where
    quotePrefix = try $ do
      level <- detachedModifier '>'
      guard $ level > quoteLevel pureBlockEnv
      pure level

list :: PureBlockEnv -> Parser List
list pureBlockEnv = do
  (level, ordering) <- lookAhead listPrefix
  items <- many (listItem level ordering)
  pure $ ListCons level ordering items
  where
    listItem envLevel envOrdering = do
      notFollowedBy pureBlockBreak
      (level, ordering) <- try $ do
        (level, ordering) <- listPrefix
        guard $ ordering == envOrdering
        guard $ level == envLevel
        pure (level, ordering)
      status <- taskStatus
      itemContent <- pureBlocks (pureBlockEnv {listLevel = level})
      pure (status, itemContent)
    listPrefix = try $ do
      prefix@(level, _) <- choice [(,UnorderedList) <$> detachedModifier '-', (,OrderedList) <$> detachedModifier '~']
      guard $ level > listLevel pureBlockEnv
      pure prefix

heading :: Int -> Parser Heading
heading envHeadingLevel = lexeme $ do
  (level, status, title) <- headingLine
  blocks <- blocks' level
  pure $ HeadingCons level status title blocks
  where
    headingLine = lexemeSpaces $ try $ do
      level <- detachedModifier '*'
      status <- taskStatus
      title <- paragraphSegment
      guard $ envHeadingLevel < level
      pure (level, status, title)

taskStatus :: Parser (Maybe TaskStatus)
taskStatus = lexemeSpaces $ fmap join . optional $ try $ do
  char '('
  c <- anyChar
  char ')'
  space
  pure $ charToTaskStatus c
  where
    taskChars = taskStatusChar <$> [minBound .. maxBound]
