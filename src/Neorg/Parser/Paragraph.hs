{-# LANGUAGE LambdaCase #-}

module Neorg.Parser.Paragraph where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Bool
import Data.Set qualified as S
import Data.Text
import Data.Text qualified as T
import Data.These
import Neorg.Document
import Neorg.Parser.Combinators
import Neorg.Parser.Type
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol, newline, string)

type ParagraphParser a = StateT ParagraphState Parser a

data ParagraphState = ParagraphState
  { currentLineHasContent :: Bool,
    previousElement :: ParagraphElement
  }
  deriving (Show, Eq)

paragraphSegment :: Parser Paragraph
paragraphSegment = paragraphWithEnd (True <$ (void eol <|> eof))

paragraph :: Parser Paragraph
paragraph = paragraphWithEnd (True <$ paragraphBreak)

paragraphWithEnd :: Parser Bool -> Parser Paragraph
paragraphWithEnd =
  flip evalStateT (ParagraphState False Space) . paragraphWithEnd' . lift

paragraphWithEnd' :: ParagraphParser Bool -> ParagraphParser Paragraph
paragraphWithEnd' end = do
  result <- collect1 end' $ do
    element <-
      choice
        [ either Punctuation Word <$> escapeChar,
          Link <$> link,
          uncurry StyledParagraph <$> styleModifier,
          uncurry VerbatimParagraph <$> verbatimModifier,
          Word <$> lift word,
          Punctuation <$> lift punctuation,
          Space <$ lift spaces1,
          Space <$ ((lift spaces >> void eol) >> modify (\state -> state {currentLineHasContent = False}))
        ]
    modify (\state -> ParagraphState (currentLineHasContent state || element /= Space) element)
    pure element

  maybe (fail "Not a valid paragraph") pure result
  where
    end' :: ParagraphParser ([ParagraphElement] -> Maybe Paragraph)
    end' = do
      isValid <- end
      if isValid
        then pure $ pure . Paragraph
        else pure $ const Nothing

escapeChar :: ParagraphParser (Either Char Text)
escapeChar = try $ do
  char '\\'
  c <- satisfy (> ' ')
  pure $
    if S.member c punctuationCharacters
      then Left c
      else Right $ pack [c]

styleModifier :: ParagraphParser (ParagraphStyle, Paragraph)
styleModifier = try $ do
  style <- styleStart
  paragraph <- paragraphWithEnd' (True <$ styleEnd style <|> False <$ lift paragraphBreak)
  pure (style, paragraph)
  where
    styleStart :: ParagraphParser ParagraphStyle
    styleStart = choice $ do
      style <- [minBound .. maxBound]
      pure $ style <$ attachedModifierStart (styleToChar style)

    styleEnd :: ParagraphStyle -> ParagraphParser ()
    styleEnd style = attachedModifierEnd (styleToChar style)

verbatimModifier :: ParagraphParser (VerbatimType, Text)
verbatimModifier = try $ do
  verbatimType <- verbatimStart
  text <- lift (verbatim (verbatimToChar verbatimType) verbatimEnd)
  pure (verbatimType, text)
  where
    verbatimStart :: ParagraphParser VerbatimType
    verbatimStart = choice $ do
      verbatimType <- [minBound .. maxBound]
      pure $ verbatimType <$ attachedModifierStart (verbatimToChar verbatimType)

    verbatimEnd :: Char -> Text -> Parser ()
    verbatimEnd c line =
      evalStateT
        (attachedModifierEnd c)
        (ParagraphState True $ if T.last line == ' ' then Space else Word (pack [T.last line]))

link :: ParagraphParser (These LinkLocation Paragraph)
link = try $ do
  location <- optional linkLocation
  label <- optional linkLabel
  case (location, label) of
    (Just loc, Just lab) -> pure $ These loc lab
    (Nothing, Just lab) -> pure $ That lab
    (Just loc, Nothing) -> pure $ This loc
    _ -> fail "Not a link"
  where
    linkLocation = try $ do
      char '{'
      notFollowedBy (void eol <|> eof)
      url <- lift $ verbatim '}' (\c line -> guard (T.length (T.strip line) > 0) >> void (char c))
      pure $ Url url
    linkLabel = try $ do
      char '['
      notFollowedBy (void eol <|> eof)
      paragraphWithEnd' $ do
        hasContent <- currentLineHasContent <$> get
        ( hasContent <$ char ']' ) <|> ( False <$ lift paragraphBreak )

verbatim :: Char -> (Char -> Text -> Parser ()) -> Parser Text
verbatim endChar endParser = do
  line <- takeWhile1P (Just "verbatim") (\c -> c /= '\n' && c /= '\r' && c /= endChar)
  guard $ T.length line > 0
  choice
    [ line <$ endParser endChar line,
      notFollowedBy paragraphBreak >> eol >> ((line <>) <$> verbatim endChar endParser)
    ]

attachedModifierStart :: Char -> ParagraphParser ()
attachedModifierStart c = try $ do
  get >>= (guard . \case
      Space -> True
      (Punctuation _) -> True
      _ -> False) . previousElement
  char c
  lift $ notFollowedBy (space <|> void eol <|> void eof)
  pure ()

attachedModifierEnd :: Char -> ParagraphParser ()
attachedModifierEnd c = try $ do
  get >>= (guard . \case
      Space -> False
      _ -> True) . previousElement
  char c
  lift $ followedBy $ space <|> void punctuation <|> void eol <|> void eof

styleToChar :: ParagraphStyle -> Char
styleToChar Bold = '*'
styleToChar Italic = '/'
styleToChar Underline = '_'
styleToChar Spoiler = '!'
styleToChar Superscript = '^'
styleToChar Subscript = ','
styleToChar StrikeThrough = '-'

verbatimToChar :: VerbatimType -> Char
verbatimToChar Math = '$'
verbatimToChar Code = '`'

word :: Parser Text
word = takeWhile1P (Just "Word") $ \c -> c > ' ' && not (S.member c punctuationCharacters)

punctuation :: Parser Char
punctuation = satisfy $ \c -> S.member c punctuationCharacters

punctuationCharacters :: S.Set Char
punctuationCharacters = S.fromList "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

paragraphBreak :: Parser ()
paragraphBreak = eof <|> try (eol >> spaces >> void eol)
