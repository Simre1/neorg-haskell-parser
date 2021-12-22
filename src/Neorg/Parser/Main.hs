{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Neorg.Parser.Main
  ( parse,
    document,
    module Neorg.Parser.Paragraph,
    module Neorg.Parser.Block,
    module Neorg.Parser.Utils,
    module Neorg.Parser.Types,
  )
where

import Control.Arrow (left)
import Control.Monad.Trans.State (evalStateT)
import Data.Text (Text)
import qualified Data.Text as T
import Neorg.Document
import Neorg.Document.Tag
import Neorg.Parser.Block
import Neorg.Parser.Paragraph
import Neorg.Parser.Tags ()
import Neorg.Parser.Types
import Neorg.Parser.Utils
import qualified Text.Megaparsec as P
import Data.Void (Void)

parse :: GenerateTagParser tags => Text -> Text -> Either Text (Document tags)
parse fileName fileContent =
  left (T.pack . P.errorBundlePretty) $
    P.runParser @Void document (T.unpack fileName) fileContent

document :: GenerateTagParser tags => Parser p (Document tags)
document = flip evalStateT (CurrentHeadingLevel I0) $
  Document <$> blocks
