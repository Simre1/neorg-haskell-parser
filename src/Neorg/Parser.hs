{-# LANGUAGE BangPatterns #-}

module Neorg.Parser where

import Control.Applicative
import Control.Monad (guard, mzero, void, when)
import Data.Char
import Data.Maybe
import Data.Text as T (Text, concat, length, strip, unpack, words)
import Data.Time (defaultTimeLocale, parseTimeM)
import Data.Time.Calendar
import qualified Data.Vector as V
import Data.Void (Void)
import Debug.Trace
import Neorg.Document hiding (documentMeta)
import Text.Megaparsec as P
import Text.Megaparsec.Char (hspace1)
import Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

type Parser = P.Parsec Void Text

data Some f where
  Some :: f e -> Some f

handleSome :: (forall a. f a -> x) -> Some f -> x
handleSome f (Some x) = f x

document :: Parser Document
document = do
  meta <- documentMeta <|> pure emptyDocumentMeta
  blocks <- blocks (fail "no end")
  pure $ Document blocks meta

documentMeta :: Parser DocumentMeta
documentMeta = do
  lexeme $ P.string "@document.meta"
  lNewline
  meta <- foldl makeDocumentMeta emptyDocumentMeta . catMaybes <$> P.many metaItem
  P.string "@end"
  lNewline
  pure meta
  where
    makeDocumentMeta meta (field, value) = case field of
      "title" -> meta {documentTitle = Just value}
      "description" -> meta {documentDescription = Just value}
      "author" -> meta {documentAuthor = Just value}
      "categories" -> meta {documentCategories = V.fromList $ T.words value}
      "created" -> meta {documentCreated = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" (unpack value)}
      "version" -> meta {documentVersion = Just value}
      _ -> meta
    metaItem = do
      notFollowedBy $ P.string "@end"
      field <- P.takeWhileP (Just "meta item") (\c -> c /= ':' && c /= '\n')
      P.char ':'
      value <- T.strip <$> P.takeWhile1P (Just "meta field") (/= '\n')
      lNewline
      case value of
        "" -> pure Nothing
        _ -> pure $ Just (field, value)

quote :: Parser Quote
quote = do
  level <- repeatingLevel '>'
  singleSpace
  blocks <-
    blocks $
      lNewline
        <|> (repeatingLevel '>' >>= \l -> singleSpace >> guard (level < l))
  pure $ QuoteCons level blocks

blocks :: Parser end -> Parser Blocks
blocks blockEnd = do
  blocks <-
    catMaybes
      <$> P.manyTill
        singleBlock
        ( void blockEnd
            <|> (repeating '=' >>= guard . (> 2))
            <|> eof
        )
  pure $ V.fromList blocks
  where
    singleBlock = P.choice [Nothing <$ lNewline, Just . Heading <$> heading, list (Just . List), Just . Quote <$> quote, Just . Paragraph <$> inline]

repeatingLevel :: Char -> Parser IndentationLevel
repeatingLevel char = try $ takeWhile1P (Just $ "repeating " ++ [char]) (== char) >>= toIndentation . T.length
  where
    toIndentation = \case
      1 -> pure I0
      2 -> pure I1
      3 -> pure I2
      4 -> pure I3
      5 -> pure I4
      6 -> pure I5
      _ -> mzero

repeating :: Char -> Parser Int
repeating char = T.length <$> takeWhile1P (Just $ "repeating " ++ [char]) (== char)

singleSpace :: Parser ()
singleSpace = lexeme $ () <$ char ' '

heading :: Parser Heading
heading = do
  (level, text) <- try headingText
  blocks <- blocks (headingEnd level)
  pure $ HeadingCons text level blocks
  where
    headingText = do
      indentation <- repeatingLevel '*'
      singleSpace
      headingText <- wordsInLine
      lNewline
      pure (indentation, headingText)
    headingEnd parentLevel =
      try $
        lookAhead (repeatingLevel '*' >>= guard . (parentLevel >=) >> singleSpace)
          <|> (repeating '-' >>= guard . (> 2) >> lNewline)

lNewline :: Parser ()
lNewline = void $ lexeme newline

list :: (forall order. List order -> x) -> Parser x
list f = list' I0 f
  where
    detectListBeginning :: Char -> OrderS o -> (IndentationLevel -> OrderS o -> Parser x) -> Parser x
    detectListBeginning s o f = do
      level <- lookAhead . try $ do
        l <- repeatingLevel s
        singleSpace
        pure l
      f level o

    list' :: IndentationLevel -> (forall order. List order -> x) -> Parser x
    list' level f = do
      detectListBeginning '-' UnorderedS (makeList level f) <|> detectListBeginning '~' OrderedS (makeList level f)

    listItem :: IndentationLevel -> OrderS order -> Parser (V.Vector ListBlock)
    listItem listLevel order = do
      try $ do
        listItemLevel <- case order of
          UnorderedS -> repeatingLevel '-'
          OrderedS -> repeatingLevel '~'
        singleSpace
        guard $ listLevel == listItemLevel
      fmap V.fromList . P.many $ listBlock listLevel

    makeList :: IndentationLevel -> (forall order. List order -> x) -> IndentationLevel -> OrderS order -> Parser x
    makeList minimumLevel f level order = do
      guard $ minimumLevel >= level
      items <- P.many (listItem level order)
      pure $ f $ ListCons level order $ V.fromList items

    listBlock :: IndentationLevel -> Parser ListBlock
    listBlock level = do
      notFollowedBy (listBlockEnd level)
      P.choice [list' (succ level) SubList, ListParagraph <$> inline]

    listBlockEnd :: IndentationLevel -> Parser ()
    listBlockEnd parentLevel = do
      lNewline
        <|> ((repeatingLevel '-' <|> repeatingLevel '~') >>= \l -> singleSpace >> guard (parentLevel >= l))
        <|> (repeatingLevel '*' >> singleSpace)
      pure ()

peekLine :: Parser ()
peekLine = do
  !line <- fmap traceShowId . lookAhead $ P.takeWhileP Nothing (/= '\n')
  pure ()

inline :: Parser Inline
inline = inline'
  where
    inline' = do
      paragraph <- P.many $ notFollowedBy inlineEnd >> wordsInLine
      lNewline
      pure $ ConcatInline . V.fromList . fmap (\i -> ConcatInline . V.fromList $ [i, Text " "]) $ paragraph
    inlineEnd :: Parser ()
    inlineEnd = do
      lNewline <|> delimitingModifiers <|> eof
    delimitingModifiers = do
      repeatingLevel '*' <|> repeatingLevel '-' <|> repeatingLevel '~'
      singleSpace

lexeme :: Parser a -> Parser a
lexeme = P.lexeme P.hspace

word :: (Char -> Bool) -> Parser Text
word end = lexeme $ P.takeWhile1P (Just "Word") (not . end)

wordsInLine :: Parser Inline
wordsInLine = do
  inlineHelper (\c -> c == ' ' || c == '\n')

inlineHelper :: (Char -> Bool) -> Parser Inline
inlineHelper end = lexeme $ do
  try (lookAhead (P.satisfy end) >> pure (Text "")) <|> 
    foldl1 (<|>) (uncurry modifier <$> modifiers) <|> text <|> pure (Text "")
  where 
    modifier char cons = try $ do 
      P.char char
      inline <- inlineHelper (\c -> c==char || end c)
      peekLine
      P.char char
      pure $ cons inline
    text = do
      w <- word end
      r <- inlineHelper end
      pure $ ConcatInline $ V.fromList [Text w, r]
    modifiers = [('*', Bold), ('/', Italic)]

