module Main where

import Control.Monad (forM_)
import Data.Aeson
import Data.Text (unpack)
import Data.Text.IO qualified as T
import Neorg.Pandoc (convertDocument)
import Neorg.Parser (parseDocument)
import System.Directory
import System.FilePath
import System.Process.Typed
import Test.HUnit ((@=?))
import Test.Hspec

main :: IO ()
main = do
  setCurrentDirectory "test/norg-markdown-equal/test_files"
  tests <- listDirectory "."
  hspec $ describe "Norg-Markdown equivalence tests" $ do
    forM_ tests $ \testFilePath ->
      it (takeFileName testFilePath) $ do
        markdownJSON <- runMarkdown testFilePath
        norgJSON <- runNorg testFilePath
        markdownJSON @=? norgJSON

runMarkdown :: FilePath -> IO Value
runMarkdown filePath = do
  (_, bytes, err) <- readProcess $ proc "pandoc" ["-w", "json", filePath <> "/file.md"]
  if err /= ""
    then fail $ show err
    else maybe (fail "Cannot decode pandoc output") pure $ decode bytes

runNorg :: FilePath -> IO Value
runNorg filePath = do
  content <- T.readFile $ filePath <> "/file.norg"
  document <- either (fail . unpack) pure $ parseDocument "" content
  pandoc <- convertDocument document
  pure $ toJSON pandoc
