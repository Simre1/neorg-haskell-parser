module Neorg.Parser.Delimiter where

import Neorg.Parser.Base
import Control.Monad
import Text.Megaparsec
import Neorg.Parser.Combinators

delimitingModifier :: Char -> Parser ()
delimitingModifier c = lexeme $ try $ do
  atBeginningOfLine >>= guard
  times <- repeating c
  guard $ times >= 3
  spaces
  followedBy (newline <|> eof)


delimiterBreak :: Parser ()
delimiterBreak = followedBy $ weakDelimiter <|> strongDelimiter <|> horizontalRule

weakDelimiter :: Parser ()
weakDelimiter = delimitingModifier '-'

strongDelimiter :: Parser ()
strongDelimiter = delimitingModifier '='

horizontalRule :: Parser ()
horizontalRule = delimitingModifier '_'
