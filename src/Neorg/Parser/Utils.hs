{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Neorg.Parser.Utils where

import Cleff
import Cleff.Reader
import Cleff.State
import Control.Applicative (Alternative (many, (<|>)))
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Data.Char (isLetter)
import Data.Functor (void, ($>), (<&>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Void (Void)
import Debug.Trace (traceShow, traceShowId)
import Neorg.Document
import Neorg.Parser.Types
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Internal as P
import Unsafe.Coerce (unsafeCoerce)
import Effect.Logging

embedParser :: (MonadFail m, P.ShowErrorComponent e) => P.Parsec e Text a -> Text -> m a
embedParser p t = do
  let a = P.runParser p "Embed" t
  either (fail . P.errorBundlePretty) pure a

embedParserT :: (es :>> moreEs) => Parser es a -> Text -> Parser moreEs a
embedParserT p t = do
  a <- lift $ liftMore $ P.runParserT p "Embed" t
  either (fail . P.errorBundlePretty) pure a
  where
    liftMore :: (es :>> moreEs) => Eff es a -> Eff moreEs a
    liftMore = unsafeCoerce

many1 :: (Alternative f) => f a -> f [a]
many1 p = (:) <$> p <*> many p

manyV :: Alternative f => f a -> f (V.Vector a)
manyV = fmap V.fromList . many

lNewline :: P.MonadParsec e Text p => p ()
lNewline = (newline >> P.hspace) <|> P.eof

newline :: P.MonadParsec e Text p => p ()
newline = void P.newline <|> void P.crlf

followedBy :: P.MonadParsec e s p => p a -> p a
followedBy = P.try . P.lookAhead

singleWhitespace :: P.MonadParsec e Text p => p ()
singleWhitespace = P.satisfy (< ' ') $> ()

singleSpace :: P.MonadParsec e Text p => p ()
singleSpace = P.char ' ' $> ()

repeatingLevel :: P.MonadParsec e Text p => Char -> p IndentationLevel
repeatingLevel char = P.try $ P.takeWhile1P (Just $ "repeating " ++ [char]) (== char) <&> toEnum . pred . T.length

repeating :: P.MonadParsec e Text p => Char -> p Int
repeating char = P.try $ P.takeWhile1P (Just $ "repeating " ++ [char]) (== char) <&> T.length

(>->) :: Monad m => m a -> m b -> m a
(>->) ma mb = ma >>= (<$ mb)

infixl 1 >->

lookChar :: P.MonadParsec e Text p => p Char
lookChar = followedBy anyChar

anyChar :: P.MonadParsec e Text p => p Char
anyChar = P.satisfy (const True)

doubleNewline :: P.MonadParsec e Text p => p ()
doubleNewline = P.try $ newline >> P.hspace >> void newline

consumingTry :: P.ParsecT e s m a -> P.ParsecT e s m a
consumingTry p = P.ParsecT $ \s cok _ eok eerr ->
  let eerr' err s' = eerr err s'
   in P.unParser p s cok eerr' eok eerr'
{-# INLINE consumingTry #-}

viewChar :: Logging :> es => Parser es ()
viewChar = lookChar >>= lift . logInfo . T.pack . pure

viewN ::  Logging :> es => Int -> Parser es ()
viewN i = P.lookAhead (takeN i) >>= lift . logInfo
  where takeN n = P.takeP Nothing n <|> takeN (pred n)

textWord :: P.MonadParsec e Text p => p Text
textWord = P.takeWhile1P (Just "Text word") (\c -> c /= ' ' && c /= '\n' && c /= '\r')

isMarkupElement :: P.MonadParsec e Text p => p Bool
isMarkupElement = test $> False <|> pure True
  where
    test =
      lookChar >>= \case
        '*' -> P.notFollowedBy (repeatingLevel '*' >> singleSpace)
        '-' ->
          P.notFollowedBy $
            repeating '-' >>= \n ->
              if
                  | n < 3 -> singleSpace
                  | n < 7 -> singleSpace <|> P.hspace >> void newline
                  | otherwise -> P.hspace >> void newline
        '=' -> P.notFollowedBy (repeating '=' >> P.hspace >> newline)
        '>' -> P.notFollowedBy (repeatingLevel '>' >> singleSpace)
        '~' -> P.notFollowedBy (repeatingLevel '~' >> singleSpace)
        '$' ->
          P.notFollowedBy
            ( void (P.string "$ ")
                <|> void (P.string "$$" >> (singleSpace <|> newline <|> P.eof))
            )
        '_' -> P.notFollowedBy (repeating '_' >>= guard . (> 2) >> P.hspace >> newline)
        '|' -> P.notFollowedBy (P.char '|' >> singleSpace >> P.hspace >> textWord)
        '@' -> P.notFollowedBy (void (P.string "@end") <|> (anyChar >> anyChar >>= guard . isLetter))
        _ -> pure ()

clearBlankSpace :: P.MonadParsec e Text p => p ()
clearBlankSpace = void $ P.takeWhileP (Just "Clearing whitespace") (<= ' ')

manyOrEnd :: (Monad f, Alternative f) => f () -> f a -> f end -> f [a]
manyOrEnd s p e = do
  s
  e $> [] <|> (:) <$> p <*> manyOrEnd s p e <|> pure []

noParse :: (P.MonadParsec e Text p) => p a -> p ()
noParse p = ((P.try p $> False) <|> pure True) >>= guard

debugParse :: Show a => P.Parsec Void Text a -> Text -> IO ()
debugParse p s = print $ P.parse p "debug" s

mapParserMonad :: forall f g e s o. (forall x. f x -> g x) -> (forall x. g x -> f x) -> P.ParsecT e s f o -> P.ParsecT e s g o
mapParserMonad to from p = P.ParsecT $ \s cok cerr eok eerr ->
  to $ P.unParser p s (f3 cok) (f2 cerr) (f3 eok) (f2 eerr)
  where
    f2 :: forall a b x. (a -> b -> g x) -> a -> b -> f x
    f2 f a b = from $ f a b
    f3 :: forall a b c x. (a -> b -> c -> g x) -> a -> b -> c -> f x
    f3 f a b c = from $ f a b c

runParserEffect :: (Eff (e ': es) ~> Eff es) -> P.ParsecT err s (Eff (e ': es)) ~> P.ParsecT err s (Eff es)
runParserEffect runHandler = mapParserMonad runHandler raise

runParserState :: s -> Parser (State s ': es) ~> Parser es
runParserState s = runParserEffect (fmap fst . runState s)

runParserReader :: r -> Parser (Reader r ': es) ~> Parser es
runParserReader r = runParserEffect (runReader r)

readOnlyState :: forall s es. Parser (Reader s ': es) ~> Parser (State s ': es)
readOnlyState = mapParserMonad to from
  where
    to p = do
      s <- get
      raise $ runReader s p
    from p = do
      s <- ask
      fmap fst . raise $ runState s p

isNewline :: Char -> Bool
isNewline c = c == '\n' || c == '\r'

isEof :: Parser es Bool
isEof = P.eof $> True <|> pure False

-- data Embed (s :: *) a = Embed {embedState :: s, embedInfo :: a s}
--
-- newtype WithEnd s = UntilMatch (P.Parsec Void s ())
--
-- instance (Ord (P.Token s), Ord (P.Tokens s), P.Stream s) => P.Stream (Embed s WithEnd) where
--   type Token (Embed s _) = P.Token s
--   type Tokens (Embed s _) = P.Tokens s
--   tokensToChunk _ = P.tokensToChunk (Proxy @s)
--   chunkToTokens _ = P.chunkToTokens (Proxy @s)
--   chunkLength _ = P.chunkLength (Proxy @s)
--   take1_ (Embed s i) = second (`Embed` i) <$> P.take1_ s
--   takeN_ n (Embed s i) = second (`Embed` i) <$> P.takeN_ n s
--   takeWhile_ f (Embed s i) = second (`Embed` i) $ P.takeWhile_ f s
--
-- embed :: forall s embedInfo m a e. P.Token s ~ P.Token (Embed s embedInfo) => embedInfo s -> P.ParsecT e (Embed s embedInfo) m a -> P.ParsecT e s m a
-- embed embedInfo p = P.ParsecT $ \s a b c d ->
--   P.unParser
--     p
--     (mapStateInto s)
--     (\x s -> a x (mapStateBack s))
--     (\pE sE -> b (mapError pE) (mapStateBack sE))
--     (\x s -> c x (mapStateBack s))
--     (\pE sE -> d (mapError pE) (mapStateBack sE))
--   where
--     mapStateInto :: P.State s x -> P.State (Embed s embedInfo) x
--     mapStateInto s =
--       let ps = P.statePosState s
--        in P.State
--             { P.stateInput = Embed (P.stateInput s) embedInfo,
--               P.stateOffset = P.stateOffset s,
--               P.statePosState =
--                 P.PosState
--                   { P.pstateInput = Embed (P.pstateInput ps) embedInfo,
--                     P.pstateOffset = P.pstateOffset ps,
--                     P.pstateSourcePos = P.pstateSourcePos ps,
--                     P.pstateTabWidth = P.pstateTabWidth ps,
--                     P.pstateLinePrefix = P.pstateLinePrefix ps
--                   },
--               P.stateParseErrors = mapError <$> P.stateParseErrors s
--             }
--     mapStateBack :: P.State (Embed s embedInfo) e -> P.State s e
--     mapStateBack s =
--       let ps = P.statePosState s
--        in P.State
--             { P.stateInput = embedState (P.stateInput s),
--               P.stateOffset = P.stateOffset s,
--               P.statePosState =
--                 P.PosState
--                   { P.pstateInput = embedState (P.pstateInput ps),
--                     P.pstateOffset = P.pstateOffset ps,
--                     P.pstateSourcePos = P.pstateSourcePos ps,
--                     P.pstateTabWidth = P.pstateTabWidth ps,
--                     P.pstateLinePrefix = P.pstateLinePrefix ps
--                   },
--               P.stateParseErrors = mapError <$> P.stateParseErrors s
--             }
--     mapError :: P.Token s1 ~ P.Token s2 => P.ParseError s1 x -> P.ParseError s2 x
--     mapError = \case
--       P.TrivialError x1 x2 x3 -> P.TrivialError x1 x2 x3
--       P.FancyError x1 x2 -> P.FancyError x1 x2
--
--
-- takeUntilEnd :: Ord e => (Char -> Bool) -> P.ParsecT e Text m () -> P.ParsecT e Text m Text
-- takeUntilEnd f end = go ""
--   where
--     go previous = do
--       text <- P.takeWhileP (Just "Text") f
--       (P.try end >> P.takeP (Maybe String) Int (previous <> text)) <|> pure (previous <> text)
--
