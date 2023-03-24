module Neorg.Parser where

import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Text
import Neorg.Document (Blocks, Document, Paragraph)
import Neorg.Parser.Base
import Neorg.Parser.Block (blocks)
import Neorg.Parser.Document (document)
import Neorg.Parser.Paragraph
import Text.Megaparsec

parseTextAnySource :: Show a => Parser a -> Text -> Either Text a
parseTextAnySource = parseText "any source"

parseText :: String -> Parser a -> Text -> Either Text a
parseText sourceName p t = first (pack . errorBundlePretty) $ runParser (evalStateT p $ ParserState 0 True 1) sourceName t

parseDocument :: String -> Text -> Either Text Document
parseDocument name = parseText name document

parseBlocks :: Text -> Either Text Blocks
parseBlocks = parseTextAnySource blocks

parseParagraph :: Text -> Either Text Paragraph
parseParagraph = parseTextAnySource paragraph

parseParagraphSegment :: Text -> Either Text Paragraph
parseParagraphSegment = parseTextAnySource paragraphSegment
