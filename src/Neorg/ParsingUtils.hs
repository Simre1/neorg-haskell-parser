{-# LANGUAGE UndecidableInstances #-}

module Neorg.ParsingUtils where

import Control.Applicative
import Control.Arrow
import Control.Monad.Trans.Class (lift)
import Data.Coerce
import Data.Functor
import Data.Proxy
import Data.Text (Text)
import Data.Void
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Internal as P

embedParser :: (MonadFail m, P.ShowErrorComponent e) => P.Parsec e Text a -> Text -> m a
embedParser p t = do
  let a = P.runParser p "Embed" t
  either (fail . P.errorBundlePretty) pure a

embedParserT :: (Monad m, MonadFail m, P.Stream s, P.ShowErrorComponent e) => P.ParsecT e Text m a -> Text -> P.ParsecT e s m a
embedParserT p t = do
  a <- lift $ P.runParserT p "Embed" t
  either (fail . P.errorBundlePretty) pure a



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
