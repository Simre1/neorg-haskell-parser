{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Neorg.Parser.Main where

import Control.Arrow (ArrowChoice (left))
import Control.Monad.Trans.State
import Data.Functor.Identity (Identity (runIdentity))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Neorg.Document
import Neorg.Document.Tag
import Neorg.Parser.Block
import Neorg.Parser.Paragraph
import Neorg.Parser.Types
import Neorg.Parser.Tags
import qualified Text.Megaparsec as P

parse :: GenerateTagParser tags => Text -> Text -> Either Text (Document tags)
parse fileName fileContent =
  left (T.pack . P.errorBundlePretty) $
    runIdentity $
      flip evalStateT defaultParserState $
        P.runParserT document (T.unpack fileName) fileContent

document :: GenerateTagParser tags => Parser (Document tags)
document =
  Document <$> blocks

blocks :: GenerateTagParser tags => Parser (Blocks tags)
blocks = do
  blocks <- P.many singleBlock
  pure $ V.fromList $ mconcat blocks
