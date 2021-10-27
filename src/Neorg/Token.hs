{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Neorg.Token where

import Control.Applicative
import Control.Monad (forM_, guard)
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char (isDigit, isLetter)
import Data.Functor
import Data.Functor.Identity
import Data.STRef
import Data.Text
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Void
import Debug.Trace (traceShow, traceShowId)
import Neorg.Consumer
import Neorg.Document
import Text.Megaparsec (errorBundlePretty)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Text.Read (readMaybe)

data AttachedTokenType
  = TBold
  | TItalic
  | TUnderline
  | TStrikethrough
  | TSuperscript
  | TSubscript
  | TSpoiler
  | TVerbatim
  | TMath
  | TVariable
  deriving (Show, Eq)

data OpenClosed = Open | Closed deriving (Show, Eq)

data AttachedToken = AttachedT OpenClosed AttachedTokenType deriving (Show, Eq)

data DetachedToken
  = THeading IndentationLevel
  | TUnorderedList IndentationLevel
  | TOrderedList IndentationLevel
  | TTaskList TaskStatus IndentationLevel
  | TMarker
  | TQuote IndentationLevel
  | TInsertion
  deriving (Show, Eq)

data DelimitingToken = TWeakDelimiter | TStrongDelimiter | THorizontalDelimiter deriving (Show, Eq)

data LinkToken = LinkStart | LinkEnd Text deriving (Show, Eq)

data DefinitionToken = TSingleDefinition | TMultiDefinition deriving (Show, Eq)

newtype IntersectingToken = TIntersecting AttachedToken deriving (Show, Eq)

data TaskStatus = TaskDone | TaskPending | TaskUndone deriving (Show, Eq)

data Token (tags :: [*])
  = Word Text
  | Break
  | AttachedToken AttachedToken
  | DetachedToken DetachedToken
  | DelimitingToken DelimitingToken
  | LinkToken LinkToken
  | DefinitionToken DefinitionToken
  | IntersectionToken IntersectingToken
  | End
  deriving (Show, Eq)

type TokenConsumer t = Consumer (Token t)

data ScannerState = ScannerState
  { stateInSingleParagraph :: Bool,
    stateInSubscript :: Bool
  }

type Scanner = P.ParsecT Void Text (StateT ScannerState Identity)

anyChar = P.satisfy (const True)

listTokens :: TokenConsumer tags [Token tags]
listTokens = ($ []) <$> go id
  where
    go !f =
      Next $ \case
        End -> Done (f . (End :))
        t -> go (f . (t :))

data NextToken tags = NextToken (Token tags) (Scanner (NextToken tags))

yieldToken :: Scanner (NextToken tags) -> Token tags -> NextToken tags
yieldToken = flip NextToken

makeScanner :: (Char -> Scanner (NextToken tags)) -> Scanner (NextToken tags)
makeScanner scanner = (P.eof $> NextToken End (fail "File end reached")) <|> (P.lookAhead anyChar >>= scanner)

openingAttachedTokens :: Char -> Scanner (NextToken tags)
openingAttachedTokens c =
  let token = case c of
        '*' -> scanAttachedToken $> mkToken TBold
        '/' -> scanAttachedToken $> mkToken TItalic
        '_' -> scanAttachedToken $> mkToken TUnderline
        '-' -> scanAttachedToken $> mkToken TStrikethrough
        '^' -> scanAttachedToken $> mkToken TSuperscript
        ',' -> scanAttachedToken >> lift (modify $ \s -> s {stateInSubscript = True}) $> mkToken TSubscript
        '|' -> scanAttachedToken $> mkToken TSpoiler
        '`' -> scanAttachedToken $> mkToken TVerbatim
        '$' -> scanAttachedToken $> mkToken TMath
        '=' -> scanAttachedToken $> mkToken TVariable
        _ -> fail "No attached tokens matched"
   in token <&> yieldToken (makeScanner $ \c -> openingAttachedTokens c <|> word c)
  where
    scanAttachedToken = P.try $ anyChar >> followedBy (P.satisfy (\c -> isLetter c || isDigit c) $> () <|> (P.notFollowedBy (P.char c) >> attachedModifier))
    mkToken = AttachedToken . AttachedT Open

closingAttachedTokens :: Char -> Scanner (NextToken tags)
closingAttachedTokens c =
  let token = case c of
        '*' -> scanAttachedToken $> mkToken TBold
        '/' -> scanAttachedToken $> mkToken TItalic
        '_' -> scanAttachedToken $> mkToken TUnderline
        '-' -> scanAttachedToken $> mkToken TStrikethrough
        '^' -> scanAttachedToken $> mkToken TSuperscript
        ',' ->
          lift (gets stateInSubscript) >>= guard
            >> scanAttachedToken
            >> lift (modify $ \s -> s {stateInSubscript = True})
            $> mkToken TSubscript
        '|' -> scanAttachedToken $> mkToken TSpoiler
        '`' -> scanAttachedToken $> mkToken TVerbatim
        '$' -> scanAttachedToken $> mkToken TMath
        '=' -> scanAttachedToken $> mkToken TVariable
        _ -> fail "No closing attached tokens"
   in token <&> yieldToken (makeScanner $ \c -> closingAttachedTokens c <|> (P.hspace >> inline c))
  where
    scanAttachedToken = P.try $ anyChar >> followedBy (singleSpace <|> attachedModifier <|> punctuation <|> P.eof <|> newline c $> ())
    mkToken = AttachedToken . AttachedT Closed

escapedChar :: Char -> Scanner (NextToken tags)
escapedChar = 
  P.try . \case
    '\\' -> do
      anyChar
      c <- P.lookAhead anyChar
      token <- case c of
        '*' -> fmap Word $ anyChar >> withSpace (pack [c])
        '-' -> fmap Word $ anyChar >> withSpace (pack [c])
        '_' -> fmap Word $ anyChar >> withSpace (pack [c])
        '/' -> fmap Word $ anyChar >> withSpace (pack [c])
        ',' -> fmap Word $ anyChar >> withSpace (pack [c])
        '^' -> fmap Word $ anyChar >> withSpace (pack [c])
        '|' -> fmap Word $ anyChar >> withSpace (pack [c])
        '`' -> fmap Word $ anyChar >> withSpace (pack [c])
        '=' -> fmap Word $ anyChar >> withSpace (pack [c])
        _ -> fail "No escaped char"
      pure $ yieldToken (makeScanner inline) token
    _ -> fail "No backslash"
  where
    withSpace t = P.char ' ' $> (t <> " ") <|> pure t
-- 
-- TODO: Optimise
word :: Char -> Scanner (NextToken tags)
word c = normalWord <|> controlTokenAsWord c
  where
    normalWord = do
      text <- P.takeWhile1P (Just "Word") (\c -> c > ' ' && c `notElem` ("?!:;,.<>()[]{}'\"\\/#%&$£€-*=^`_|" :: String))
      yieldToken (P.hspace >> makeScanner inline) <$> (Word <$> (P.char ' ' $> text <> " "))
        <|> pure (yieldToken (makeScanner $ \c -> closingAttachedTokens c <|> (P.hspace >> makeScanner inline)) $ Word text)
    controlTokenAsWord c
      | c `elem` ("?!:;,.<>()[]{}'\"\\/#%&$£€-*=^`_|" :: String) = anyChar >>= (\c -> P.char ' ' $> pack [c] <> " " <|> pure (pack [c])) <&> yieldToken (P.hspace >> makeScanner inline) . Word
      | otherwise = fail "No control token"

atLineStart :: Scanner (NextToken tags)
atLineStart =
  P.hspace
    >> makeScanner
      ( \c -> do
          lineStartTokens c <|> escapedChar c <|> openingAttachedTokens c <|> word c
      )

newline :: Char -> Scanner (NextToken tags)
newline c = do
  inSingleParagraph <- lift $ gets stateInSingleParagraph
  if inSingleParagraph
    then newlineSingleParagraph
    else normalNewline
  where
    scanNewline :: Scanner (NextToken tags)
    scanNewline = do
      P.newline
      P.try
        ( P.hspace >> P.newline $> yieldToken atLineStart Break
            >-> many (P.try $ P.hspace >> P.newline)
        )
        <|> atLineStart
    newlineSingleParagraph :: Scanner (NextToken tags)
    newlineSingleParagraph = case c of
      '\n' -> P.newline >> lift (modify $ \s -> s {stateInSingleParagraph = False}) $> yieldToken atLineStart Break
      '\r' -> P.newline >> lift (modify $ \s -> s {stateInSingleParagraph = False}) $> yieldToken atLineStart Break
      '~' -> (P.try (anyChar >> P.hspace >> P.newline) >> P.hspace >> makeScanner inline) <|> word c
      _ -> fail "No newline"
    normalNewline :: Scanner (NextToken tags)
    normalNewline = case c of
      '~' -> (P.try (anyChar >> P.hspace >> P.newline) >> P.hspace >> makeScanner inline) <|> word c
      '\r' -> scanNewline
      '\n' -> scanNewline
      _ -> fail "No newline"

inline :: Char -> Scanner (NextToken tags)
inline c = escapedChar c <|> openingAttachedTokens c <|> newline c <|> word c

singleLine :: forall tags. Char -> Scanner (NextToken tags)
singleLine c = lift (modify $ \s -> s {stateInSingleParagraph = True}) >> inline c

lineStartTokens :: Char -> Scanner (NextToken tags)
lineStartTokens c = P.try $ case c of
  '*' -> repeatingLevel '*' >-> singleSpace <&> yieldToken (P.hspace >> makeScanner inline) . DetachedToken . THeading
  '-' -> weakDelimiter <|> list
  '~' -> repeatingLevel '~' >-> singleSpace <&> yieldToken (P.hspace >> makeScanner inline) . DetachedToken . TOrderedList
  '|' -> anyChar >> singleSpace $> yieldToken (P.hspace >> makeScanner singleLine) (DetachedToken TMarker)
  '>' -> repeatingLevel '>' >-> singleSpace <&> yieldToken (P.hspace >> makeScanner singleLine) . DetachedToken . TQuote
  ':' -> anyChar >> singleSpace $> yieldToken (P.hspace >> makeScanner singleLine) (DefinitionToken TSingleDefinition)
  '=' ->
    repeating '=' >>= \case
      1 -> singleSpace $> yieldToken (P.hspace >> makeScanner singleLine) (DetachedToken TInsertion)
      2 -> fail "== is not a line start token"
      _ -> P.hspace >> (P.newline $> () <|> P.eof) $> yieldToken (P.hspace >> atLineStart) (DelimitingToken TStrongDelimiter)
  '_' ->
    repeating '_' >>= \case
      1 -> fail "_ is not a line start token"
      2 -> fail "__ is not a line start token"
      _ -> P.hspace >> (P.newline $> () <|> P.eof) $> yieldToken (P.hspace >> atLineStart) (DelimitingToken THorizontalDelimiter)
  _ -> fail "No line start tokens"
  where
    list =
      P.try $
        repeatingLevel '-' >-> singleSpace >>= \level ->
          yieldToken (P.hspace >> makeScanner inline) . DetachedToken
            <$> P.choice
              [ P.string "[ ]" $> TTaskList TaskUndone level,
                P.string "[*]" $> TTaskList TaskPending level,
                P.string "[x]" $> TTaskList TaskDone level,
                pure (TUnorderedList level)
              ]
    weakDelimiter =
      P.try $
        repeating '-' >>= \case
          1 -> fail "Not a delimiter"
          2 -> fail "Not a delimiter"
          _ -> P.try (P.hspace >> (P.newline $> () <|> P.eof)) $> yieldToken (P.hspace >> atLineStart) (DelimitingToken TWeakDelimiter)

singleWhitespace :: Scanner ()
singleWhitespace = P.satisfy (< ' ') $> ()

followedBy :: Scanner a -> Scanner a
followedBy = P.try . P.lookAhead

-- TODO: Optimize!
punctuation :: Scanner ()
punctuation = P.oneOf ("?!:;,.<>()[]{}\"/#%&$£€-" :: String) $> ()

attachedModifier :: Scanner ()
attachedModifier = P.oneOf ("-*=^`_|,/" :: String) $> ()

asTokens :: Text -> TokenConsumer tags a -> a
asTokens input consumer = go consumer (initialState "test" input) initialScannerState atLineStart
  where
    initialScannerState = ScannerState False False
    go :: TokenConsumer tags a -> P.State Text Void -> ScannerState -> Scanner (NextToken tags) -> a
    go consumer parserState scannerState next = case consumer of
      Done a -> a
      (Next f) -> case flip runStateT scannerState $ P.runParserT' next parserState of
        Identity ((_, Left err), nextScannerState) -> error $ errorBundlePretty err
        Identity ((nextParserState, Right (NextToken token newNext)), nextScannerState) -> go (f token) nextParserState nextScannerState newNext

initialState :: String -> s -> P.State s e
initialState name s =
  P.State
    { P.stateInput = s,
      P.stateOffset = 0,
      P.statePosState =
        P.PosState
          { P.pstateInput = s,
            P.pstateOffset = 0,
            P.pstateSourcePos = P.initialPos name,
            P.pstateTabWidth = P.defaultTabWidth,
            P.pstateLinePrefix = ""
          },
      P.stateParseErrors = []
    }

singleSpace :: Scanner ()
singleSpace = P.char ' ' $> ()

repeatingLevel :: Char -> Scanner IndentationLevel
repeatingLevel char = P.try $ P.takeWhile1P (Just $ "repeating " ++ [char]) (== char) <&> toEnum . pred . T.length

repeating :: Char -> Scanner Int
repeating char = P.try $ P.takeWhile1P (Just $ "repeating " ++ [char]) (== char) <&> T.length

(>->) :: Monad m => m a -> m b -> m a
(>->) ma mb = ma >>= (\a -> mb >> pure a)

infixl 1 >->
