module Neorg.Parser.Base where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Functor.Identity
import Data.Text
import Text.Megaparsec
import Text.Megaparsec qualified as Mega
import Text.Megaparsec.Char qualified as Mega

data ParserState = ParserState Int Bool deriving (Eq, Show)

type Parser = StateT ParserState (ParsecT Error Text Identity)

parseTextAnySource :: Show a => Parser a -> Text -> Either Text a
parseTextAnySource = parseText "any source"

parseText :: String -> Parser a -> Text -> Either Text a
parseText sourceName p t = first (pack . errorBundlePretty) $ runParser (evalStateT p $ ParserState 0 True) sourceName t

data Error = Error deriving (Eq, Show, Ord)

instance ShowErrorComponent Error where
  showErrorComponent = show

emptyLines :: Parser ()
emptyLines = do
  spaces
  (newline >> emptyLines) <|> pure ()

emptyLines1 :: Parser ()
emptyLines1 = do
  space <|> newline
  spaces
  (newline >> emptyLines) <|> pure ()

space :: Parser ()
space = void $ Mega.char ' '

spaces :: Parser ()
spaces = void $ takeWhileP (Just "Spaces") (\c -> c <= ' ' && (c /= '\n' && c /= '\r'))

spaces1 :: Parser ()
spaces1 = void $ takeWhile1P (Just "1 space or more") (\c -> c <= ' ' && (c /= '\n' && c /= '\r'))

anyChar :: Parser Char
anyChar = do
  c <- Mega.satisfy (const True)
  when (c > ' ') $ put $ ParserState 0 False
  pure c

newline :: Parser ()
newline = Mega.eol >> modify (\(ParserState lines beginning) -> ParserState (succ lines) True)

char :: Char -> Parser ()
char c = do
  Mega.char c
  when (c > ' ') $ put $ ParserState 0 False

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- Mega.satisfy f
  when (c > ' ') $ put $ ParserState 0 False
  pure c

takeWhileChars :: Maybe String -> (Char -> Bool) -> Parser Text
takeWhileChars label f = do
  t <- Mega.takeWhile1P label f
  put $ ParserState 0 False
  pure t

takeWhile1Chars :: Maybe String -> (Char -> Bool) -> Parser Text
takeWhile1Chars label f = do
  t <- Mega.takeWhile1P label f
  put $ ParserState 0 False
  pure t

linesOfWhitespace :: Parser Int
linesOfWhitespace = do
  ParserState lines _ <- get
  pure lines

atBeginningOfLine :: Parser Bool
atBeginningOfLine = do
  ParserState _ beginning <- get
  pure beginning

