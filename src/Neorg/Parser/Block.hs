module Neorg.Parser.Block where

import Control.Monad
import Neorg.Document hiding (taskStatus)
import Neorg.Parser.Combinators
import Neorg.Parser.Paragraph (paragraph, paragraphSegment)
import Neorg.Parser.Type
import Text.Megaparsec
import Text.Megaparsec.Char (char)

blocks :: Parser Blocks
blocks = blocks' 0

blocks' :: Int -> Parser Blocks
blocks' envHeadingLevel = Blocks <$> many block
  where
    block =
      choice
        [ Heading <$> heading envHeadingLevel,
          PureBlock <$> pureBlock
        ]

pureBlock :: Parser PureBlock
pureBlock = choice [Paragraph <$> paragraph]

heading :: Int -> Parser Heading
heading envHeadingLevel = do
  (level, status, title) <- headingLine
  blocks <- blocks' level
  pure $ HeadingCons level status title blocks
  where
    headingLine = lexeme $ try $ do
      level <- repeating '*'
      space
      status <- taskStatus
      title <- paragraphSegment
      guard $ envHeadingLevel < level
      pure (level, status, title)

taskStatus :: Parser (Maybe TaskStatus)
taskStatus = lexeme $ fmap join . optional $ try $ do
  char '['
  c <- anyChar
  char ']'
  space
  pure $ charToTaskStatus c
  where
    taskChars = taskStatusChar <$> [minBound .. maxBound]

