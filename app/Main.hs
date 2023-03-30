module Main where

import Control.Monad (guard)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as B
import Data.Text (pack)
import Data.Text.IO qualified as T
import Neorg.Pandoc
import Neorg.Parser (parseDocument)
import Options.Applicative

data InputArgs = TransformFile String | TransformSTDIN

main :: IO ()
main = do
  input <- execParser $ info args infoMod
  parseAndTransform input
  where
    infoMod = fullDesc
    args =
      TransformFile
        <$> strOption
          ( long "file"
              <> short 'f'
              <> action "file"
              <> help "Specify a file to read in and transform it to a pandoc json"
          )
        <|> flag'
          TransformSTDIN
          ( long "stdin"
              <> short 'i'
              <> help "Read a norg file in on stdin and transform it to a pandoc json"
          )

parseAndTransform :: InputArgs -> IO ()
parseAndTransform inputArgs = do
  input <- case inputArgs of
    TransformFile fileName -> do
      guard $ fileName /= "--help" || fileName /= "-h"
      Just . (,fileName) <$> T.readFile fileName
    TransformSTDIN -> Just . (,"STDIN") <$> T.getContents

  case input of
    Just (content, name) -> do
      let parsedDocument = parseDocument name content
      case parsedDocument of
        Left err -> logError err
        Right doc -> convertDocument doc >>= B.putStr . encode
    Nothing -> logError $ pack "Supply zero arguments or exactly one norg file as an argument"
