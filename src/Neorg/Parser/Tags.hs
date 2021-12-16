module Neorg.Parser.Tags where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Trans.State (modify)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time (parseTimeM)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import qualified Data.Vector as V
import Neorg.Document
import Neorg.Parser.Paragraph
import Neorg.Parser.Utils
import Optics.Core ((&), (.~), (?~))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

instance ParseTagContent "code" where
  parseTagContent _ args = P.takeWhileP (Just "Code block") (const True)

instance ParseTagContent "math" where
  parseTagContent _ args = P.takeWhileP (Just "Math block") (const True)

instance ParseTagContent "comment" where
  parseTagContent _ args = P.takeWhileP (Just "Comment block") (const True)

instance ParseTagContent "embed" where
  parseTagContent _ args = case args of
    "image" -> P.hspace >> P.takeWhile1P (Just "Url string") (> ' ')
    a -> fail $ "I do not recognize the embed format " ++ show a

instance ParseTagContent "document.meta" where
  parseTagContent _ _ = do
    clearBlankSpace
    foldl makeDocumentMeta emptyDocumentMeta . catMaybes <$> P.many metaItem
    where
      makeDocumentMeta meta (field, value) = case field of
        "title" -> meta & documentTitle ?~ value
        "description" -> meta & documentDescription ?~ value
        "author" -> meta & documentAuthor ?~ value
        "categories" -> meta & documentCategories .~ V.fromList (T.words value)
        "created" -> meta & documentCreated .~ parseTimeM True defaultTimeLocale "%Y-%-m-%-d" (T.unpack value)
        "version" -> meta & documentVersion ?~ value
        _ -> meta
      metaItem = do
        field <- P.takeWhileP (Just "meta item") (\c -> c /= ':' && c /= '\n' && c /= '\n')
        P.char ':'
        value <- T.strip <$> P.takeWhileP (Just "meta field") (\c -> c /= '\n' && c /= '\r')
        clearBlankSpace
        case value of
          "" -> pure Nothing
          _ -> pure $ Just (field, value)

instance ParseTagContent "table" where
  parseTagContent _ _ = do
    rows <- manyV row
    pure $ Table rows
    where
      cell = P.notFollowedBy (P.eof <|> void newline) >> cellParagraph >-> P.hspace
      row =
        let inlines = do
              P.hspace
              cells <- manyV cell
              newline
              pure $ TableRowInlines cells
            delimiter = do
              P.char '-'
              P.hspace
              newline
              pure TableRowDelimiter
         in P.try delimiter <|> inlines
      cellParagraph = runInline $ do
        modify $ delimitedActive .~ False
        paragraph' $
          void (P.string " | ")
            <|> P.try
              (P.string " |" >> P.lookAhead (void newline <|> P.eof))
            <|> void (P.lookAhead newline)
