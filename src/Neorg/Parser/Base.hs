module Neorg.Parser.Base where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Char (isNumber)
import Data.Functor.Identity
import Data.Text
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec qualified as Mega
import Text.Megaparsec.Char qualified as Mega

data ParserState = ParserState
  { whitespaceLines :: Int,
    atBeginning :: Bool,
    lineNumber :: Int
  }
  deriving (Eq, Show)

type Parser = StateT ParserState (ParsecT Error Text Identity)

parseTextAnySource :: Show a => Parser a -> Text -> Either Text a
parseTextAnySource = parseText "any source"

parseText :: String -> Parser a -> Text -> Either Text a
parseText sourceName p t = first (pack . errorBundlePretty) $ runParser (evalStateT p $ ParserState 0 True 1) sourceName t

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

putWithoutChangingLineNumber :: (Int -> ParserState) -> Parser ()
putWithoutChangingLineNumber f = modify $ \(ParserState _ _ lineNumber) -> f lineNumber

anyChar :: Parser Char
anyChar = do
  c <- Mega.satisfy (const True)
  when (c > ' ') $ putWithoutChangingLineNumber $ ParserState 0 False
  pure c

text :: Text -> Parser ()
text t = do
  Mega.string t
  when (T.any (> ' ') t) $ putWithoutChangingLineNumber $ ParserState 0 False

takeLine :: Parser Text
takeLine = do
  line <- Mega.takeWhileP (Just "line") (\c -> c /= '\n' && c /= '\r')
  when (T.any (> ' ') line) $  putWithoutChangingLineNumber$ ParserState 0 False
  pure line

newline :: Parser ()
newline = Mega.eol >> modify (\(ParserState whitespaceLines beginning lineNumber) -> ParserState (succ whitespaceLines) True (succ lineNumber))

char :: Char -> Parser ()
char c = do
  Mega.char c
  when (c > ' ') $ putWithoutChangingLineNumber $ ParserState 0 False

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- Mega.satisfy f
  when (c > ' ') $ putWithoutChangingLineNumber $ ParserState 0 False
  pure c

takeWhileChars :: Maybe String -> (Char -> Bool) -> Parser Text
takeWhileChars label f = do
  t <- Mega.takeWhile1P label f
  putWithoutChangingLineNumber $ ParserState 0 False
  pure t

takeWhile1Chars :: Maybe String -> (Char -> Bool) -> Parser Text
takeWhile1Chars label f = do
  t <- Mega.takeWhile1P label f
  putWithoutChangingLineNumber $ ParserState 0 False
  pure t

naturalNumber :: Parser Int
naturalNumber = read . unpack <$> takeWhile1Chars (Just "Number") isNumber

linesOfWhitespace :: Parser Int
linesOfWhitespace = do
  ParserState lines _ _ <- get
  pure lines

atBeginningOfLine :: Parser Bool
atBeginningOfLine = do
  ParserState _ beginning _ <- get
  pure beginning

getLineNumber :: Parser Int
getLineNumber = do
  ParserState _ _ lineNumber <- get
  pure lineNumber

blockInit :: Parser ()
blockInit = putWithoutChangingLineNumber $ ParserState 0 True
