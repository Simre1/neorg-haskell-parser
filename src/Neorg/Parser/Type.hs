module Neorg.Parser.Type where

import Data.Bifunctor
import Data.Text
import Text.Megaparsec

type Parser = Parsec Error Text

parseTextAnySource :: Show a => Parser a -> Text -> Either Text a
parseTextAnySource = parseText "any source"

parseText :: String -> Parser a -> Text -> Either Text a
parseText sourceName p t = first (pack . errorBundlePretty) $ runParser p sourceName t

data Error = Error deriving (Eq, Show, Ord)

instance ShowErrorComponent Error where
  showErrorComponent = show
