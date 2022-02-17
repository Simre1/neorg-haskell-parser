module Neorg.Parser.Tags (module Neorg.Parser.Tags.Classes) where

import Cleff.State
import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time (parseTimeM)
import Data.Time.Format (defaultTimeLocale)
import qualified Data.Vector as V
import Neorg.Document
import Neorg.Parser.Paragraph
import Neorg.Parser.Tags.Classes
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
        _ <- P.char ':'
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
      cell = do
        P.notFollowedBy (P.eof <|> void newline) >> cellParagraph
      row =
        let inlines = do
              cells <- manyV cell
              newline <|> P.eof
              pure $ TableRowInlines cells
            delimiter = do
              _ <- P.char '-'
              P.hspace
              newline
              pure TableRowDelimiter
         in P.try (P.lookAhead P.hspace >> noParse P.eof) >> P.try delimiter <|> inlines
      cellParagraph =
        runInline $ do
          lift $ modify $ delimitedActive .~ False
          paragraph' end
      end =
        P.try
          ( do
              P.hspace1
              _ <- P.char '|'
              P.lookAhead (void $ P.char ' ') <|> P.lookAhead newline <|> P.eof
          )
          <|> P.try
            ( do
                P.hspace
                P.lookAhead $ void newline <|> P.eof
            )
