{-# LANGUAGE TypeFamilies #-}

module Neorg.Parser.Combinators where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Sequence qualified as S
import Neorg.Parser.Base
import Text.Megaparsec
import Text.Megaparsec.Debug
import Data.Text (Text)

lexeme :: Parser a -> Parser a
lexeme p = liftA2 const p emptyLines

lexemeSpaces :: Parser a -> Parser a
lexemeSpaces p = liftA2 const p spaces

many1 :: Alternative p => p a -> p [a]
many1 p = (:) <$> p <*> Control.Applicative.many p

takeUntil :: MonadParsec e t p => p () -> p a -> p [a]
takeUntil end p = [] <$ try end <|> (:) <$> p <*> takeUntil end p <|> pure []

collect :: (MonadParsec e t p) => p ([a] -> b) -> p a -> p b
collect end p = go S.empty
  where
    go elems =
      (\f -> f (foldr (:) [] elems)) <$> end
        <|> (p >>= go . (elems S.:|>))

collect1 :: (MonadParsec e t p) => p ([a] -> b) -> p a -> p b
collect1 end p = p >>= go . S.singleton
  where
    go elems =
      (\f -> f (foldr (:) [] elems)) <$> end
        <|> (p >>= go . (elems S.:|>))

followedBy :: (MonadParsec e t p) => p a -> p a
followedBy = lookAhead

detachedModifier :: Char -> Parser Int
detachedModifier c = lexemeSpaces $ try $ do
  modifiers <- many1 (char c)
  space
  pure $ length modifiers

dbgThis :: (MonadParsecDbg e s p, Show a) => p a -> p a
dbgThis = dbg "this" . label "this"
