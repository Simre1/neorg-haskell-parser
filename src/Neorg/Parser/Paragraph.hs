module Neorg.Parser.Paragraph where

import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Bool
import Data.Set qualified as S
import Data.Text hiding (dropWhile, init, last, reverse)
import Data.Text qualified as T hiding (dropWhile, reverse)
import Neorg.Document
import Neorg.Parser.Base
import Neorg.Parser.Combinators
import Neorg.Parser.Delimiter (delimiterBreak)
import Neorg.Parser.Tag
import Text.Megaparsec hiding (satisfy)

type ParagraphParser a = StateT ParagraphState Parser a

newtype ParagraphState = ParagraphState
  { previousElement :: ParagraphElement
  }
  deriving (Show, Eq)

paragraphSegment :: Parser Paragraph
paragraphSegment = lexeme $ paragraphWithEnd (True <$ paragraphSegmentBreak)

paragraph :: Parser Paragraph
paragraph = lexeme $ paragraphWithEnd (True <$ paragraphBreak)

paragraphWithEnd :: Parser Bool -> Parser Paragraph
paragraphWithEnd =
  flip evalStateT (ParagraphState Space) . paragraphWithEnd' . lift

paragraphWithEnd' :: ParagraphParser Bool -> ParagraphParser Paragraph
paragraphWithEnd' end = do
  notFollowedBy $ lift detachedModifierStart
  result <- collect1 end' $ do
    element <-
      choice
        [ either Punctuation Word <$> escapeChar,
          uncurry Link <$> lift link,
          uncurry StyledParagraph <$> styleModifier,
          uncurry VerbatimParagraph <$> verbatimModifier,
          Word <$> lift word,
          Punctuation <$> lift punctuation,
          Space <$ lift emptyLines1
        ]
    modify (\state -> state {previousElement = element})
    pure element

  maybe (fail "Not a valid paragraph") pure result
  where
    end' :: ParagraphParser ([ParagraphElement] -> Maybe Paragraph)
    end' = do
      isValid <- end
      if isValid
        then pure $ pure . ParagraphCons . removeTrailingWhitespace
        else pure $ const Nothing
    removeTrailingWhitespace elements = reverse $ dropWhile (== Space) $ reverse elements

escapeChar :: ParagraphParser (Either Char Text)
escapeChar = try $ do
  lift $ char '\\'
  c <- lift $ satisfy (> ' ')
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
  text <- lift $ verbatim (verbatimToChar verbatimType) (verbatimEnd $ verbatimToChar verbatimType)
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
        (ParagraphState $ if T.last line == ' ' then Space else Word (pack [T.last line]))

link :: Parser (LinkLocation, Maybe Paragraph)
link = try $ do
  location <- linkLocation
  label <- optional linkLabel
  pure (location, label)
  where
    linkLocation = try $ do
      char '{'
      notFollowedBy (void newline <|> eof)
      choice [CurrentFile <$> norgLocation, norgFile, externalFile, urlLocation]
    linkLabel = try $ do
      char '['
      notFollowedBy (void newline <|> eof)
      paragraphWithEnd $ do
        atBeginning <- atBeginningOfLine
        (not atBeginning <$ char ']') <|> (False <$ paragraphBreak)
    urlLocation = do
      url <- linkVerbatim '}'
      pure $ Url url
    norgFile = do
      char ':'
      path <- linkVerbatim ':'
      location <- Just <$> norgLocation <|> Nothing <$ char '}'
      pure $ NorgFile path location
    externalFile = do
      char '/' >> space
      (path, lineNumber) <-
        choice
          [ (\a b -> (a, Just b)) <$> try (linkVerbatim ':') <*> naturalNumber >-> char '}',
            (,Nothing) <$> linkVerbatim '}'
          ]
      pure $ ExternalFile path lineNumber
    norgLocation = do
      choice
        [ LineNumberLocation <$> naturalNumber >-> char '}',
          MagicLocation <$> (char '#' >> space >> linkParagraph),
          uncurry HeadingLocation <$> liftA2 (,) (repeating '*' >-> space) linkParagraph
        ]
    linkVerbatim c = verbatim c (\line -> guard (T.length (T.strip line) > 0) >> void (char c))
    linkParagraph = paragraphWithEnd $ True <$ char '}' <|> False <$ (linesOfWhitespace >>= guard . (>= 2)) <|> False <$ eof

verbatim :: Char -> (Text -> Parser ()) -> Parser Text
verbatim endChar endParser = do
  line <- takeWhile1Chars (Just "verbatim") (\c -> c /= '\n' && c /= '\r' && c /= endChar)
  guard $ T.length line > 0
  choice
    [ line <$ endParser line,
      newline >> notFollowedBy paragraphBreak >> ((line <>) <$> verbatim endChar endParser)
    ]

attachedModifierStart :: Char -> ParagraphParser ()
attachedModifierStart c = try $ do
  get
    >>= ( guard . \case
            Space -> True
            (Punctuation _) -> True
            _ -> False
        )
      . previousElement
  lift $ char c
  lift $ notFollowedBy (space <|> void newline <|> void eof)
  pure ()

attachedModifierEnd :: Char -> ParagraphParser ()
attachedModifierEnd c = try $ do
  get
    >>= ( guard . \case
            Space -> False
            _ -> True
        )
      . previousElement
  lift $ char c
  lift $ followedBy $ space <|> void punctuation <|> void newline <|> void eof

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
word = takeWhile1Chars (Just "Word") $ \c -> c > ' ' && not (S.member c punctuationCharacters)

punctuation :: Parser Char
punctuation = satisfy $ \c -> S.member c punctuationCharacters

punctuationCharacters :: S.Set Char
punctuationCharacters = S.fromList "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

paragraphBreak :: Parser ()
paragraphBreak =
  choice
    [ eof,
      linesOfWhitespace >>= guard . (>= 2),
      atBeginningOfLine >>= guard >> detachedModifierStart,
      delimiterBreak,
      tagBreak
    ]

paragraphSegmentBreak :: Parser ()
paragraphSegmentBreak = choice [eof, linesOfWhitespace >>= guard . (>= 1)]

detachedModifierStart :: Parser ()
detachedModifierStart = lookAhead $ do
  choice $ flip fmap detachedModifierSymbols $ void . detachedModifier

detachedModifierSymbols :: String
detachedModifierSymbols = "*-~>"
