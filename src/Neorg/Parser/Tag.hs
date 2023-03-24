module Neorg.Parser.Tag where

import Control.Applicative (liftA2, liftA3)
import Control.Monad
import Data.List (intersperse)
import Data.Text (Text, pack)
import Neorg.Document
import Neorg.Parser.Base
import Neorg.Parser.Combinators
import Text.Megaparsec

tagBreak :: Parser ()
tagBreak = try $ lookAhead $ do
  atBeginningOfLine >>= guard
  char '@'
  void tagName

verbatimRangedTag :: Parser VerbatimRangedTag
verbatimRangedTag = do
  (whitespaceToSkip, name, parameters) <- try $ do
    char '@'
    liftA3
      (,,)
      (pred . pred . unPos . sourceColumn <$> getSourcePos)
      tagName
      tagParameters
  guard $ name /= "end"
  newline
  !content <- withinTag '@' whitespaceToSkip takeLine

  pure $ VerbatimRangedTagCons name parameters $ mconcat $ intersperse "\n" $ filter (/= "") content

withinTag :: Char -> Int -> Parser a -> Parser [a]
withinTag tagChar tagOffset lineParser = lexeme content
  where
    content = do
      lineWhitespace <|> (spaces >> followedBy newline)
      choice
        [ [] <$ (char tagChar >> text "end"),
          liftA2 (:) (lineParser >-> newline) content
        ]

    lineWhitespace = text $ pack $ replicate tagOffset ' '

tagName :: Parser Text
tagName = lexemeSpaces $ takeWhile1Chars (Just "Tag name") (`notElem` (" \n" :: String))

tagParameters :: Parser [Text]
tagParameters = many $ lexemeSpaces $ takeWhile1Chars (Just "Tag name") (`notElem` (" \n" :: String))
