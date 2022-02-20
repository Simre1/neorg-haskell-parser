module Neorg.Parser.Main
  ( parse,
    document,
    module Neorg.Parser.Paragraph,
    module Neorg.Parser.Block,
    module Neorg.Parser.Utils,
    module Neorg.Parser.Types,
  )
where

import Cleff
import Control.Arrow (left)
import Data.Text (Text)
import qualified Data.Text as T
import Neorg.Document
import Neorg.Document.Tag ()
import Neorg.Parser.Block
import Neorg.Parser.Paragraph
import Neorg.Parser.Tags
import Neorg.Parser.Types
import Neorg.Parser.Utils
import qualified Text.Megaparsec as P
import Effect.Logging

parse :: (Logging :> es, GenerateTagParser tags) => Text -> Text -> Eff es (Either Text (Document tags))
parse fileName fileContent =
  left (T.pack . P.errorBundlePretty) <$> P.runParserT document (T.unpack fileName) fileContent

document :: (GenerateTagParser tags, Logging :> es) => Parser es (Document tags)
document =
  runParserState (CurrentHeadingLevel I0) $
    Document <$> blocks
