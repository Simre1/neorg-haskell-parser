module Neorg.Parser.Block where

import Control.Applicative hiding (many)
import Control.Monad
import Neorg.Document hiding (taskStatus)
import Neorg.Parser.Base
import Neorg.Parser.Combinators
import Neorg.Parser.Delimiter
import Neorg.Parser.Paragraph (paragraph, paragraphSegment)
import Neorg.Parser.Tag
import Text.Megaparsec
import Optics.Core
import GHC.Generics (Generic)

blocks :: Parser Blocks
blocks = blocks' 0

blocks' :: Int -> Parser Blocks
blocks' envHeadingLevel = emptyLines >> (Blocks <$> many block) >-> optional weakDelimiter
  where
    block = do
      notFollowedBy $ weakDelimiter <|> strongDelimiter
      blockInit
      lineNumber <- getLineNumber
      content <-
        choice
          [ Heading <$> heading envHeadingLevel,
            NestableBlock <$> nestableBlock (NestableBlockEnv 0 0),
            HorizontalRule <$ horizontalRule
          ]
      void $ optional $ do
        guard (envHeadingLevel == 0)
        weakDelimiter <|> strongDelimiter

      pure $ Block lineNumber content

data NestableBlockEnv = NestableBlockEnv
  { listLevel :: Int,
    quoteLevel :: Int
  } deriving (Show, Eq, Generic)

nestableBlock :: NestableBlockEnv -> Parser NestableBlock
nestableBlock nestableBlockEnv = do
  notFollowedBy nestableBlockBreak
  choice
    [ List <$> list nestableBlockEnv,
      Quote <$> quote nestableBlockEnv,
      VerbatimRangedTag <$> verbatimRangedTag,
      Paragraph <$> paragraph
    ]

nestableBlocks :: NestableBlockEnv -> Parser NestableBlocks
nestableBlocks nestableBlockEnv = NestableBlocks <$> takeUntil nestableBlockBreak (nestableBlock nestableBlockEnv)

nestableBlockBreak :: Parser ()
nestableBlockBreak =
  choice
    [ eof,
      linesOfWhitespace >>= guard . (>= 2),
      atBeginningOfLine >>= guard >> topLevelDetachedModifierStart,
      delimiterBreak
    ]

topLevelDetachedModifierStart :: Parser ()
topLevelDetachedModifierStart = lookAhead $ do
  choice $ flip fmap topLevelDetachedModifierSymbols $ void . detachedModifier

topLevelDetachedModifierSymbols :: String
topLevelDetachedModifierSymbols = "*"

quote :: NestableBlockEnv -> Parser Quote
quote nestableBlockEnv = do
  notFollowedBy nestableBlockBreak
  level <- try $ do
    level <- quotePrefix
    guard $ level > nestableBlockEnv ^. #quoteLevel
    pure level
  status <- taskStatus
  content <- nestableBlocks (nestableBlockEnv & #quoteLevel .~ level)
  pure $ QuoteCons level status content
  where
    quotePrefix = try $ do
      level <- detachedModifier '>'
      guard $ level > nestableBlockEnv ^. #quoteLevel
      pure level

list :: NestableBlockEnv -> Parser List
list nestableBlockEnv = do
  (level, ordering) <- lookAhead listPrefix
  items <- many (listItem level ordering)
  pure $ ListCons level ordering items
  where
    listItem envLevel envOrdering = do
      notFollowedBy nestableBlockBreak
      level <- try $ do
        (level, ordering) <- listPrefix
        guard $ ordering == envOrdering
        guard $ level == envLevel
        pure level
      status <- taskStatus
      itemContent <- nestableBlocks (nestableBlockEnv & #listLevel .~ level)
      pure (status, itemContent)
    listPrefix = try $ do
      prefix@(level, _) <- choice [(,UnorderedList) <$> detachedModifier '-', (,OrderedList) <$> detachedModifier '~']
      guard $ level > nestableBlockEnv ^. #listLevel
      pure prefix

heading :: Int -> Parser Heading
heading envHeadingLevel = lexeme $ do
  (level, status, title) <- headingLine
  headingBlocks <- blocks' level
  pure $ HeadingCons level status title headingBlocks
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
