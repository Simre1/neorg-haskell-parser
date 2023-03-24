{-# LANGUAGE TypeFamilies #-}

module Neorg.Parser.Combinators where

import Control.Applicative
import Data.Sequence qualified as S
import Data.Text qualified as T
import Neorg.Parser.Base
import Text.Megaparsec
import Text.Megaparsec.Debug

lexeme :: Parser a -> Parser a
lexeme p = liftA2 const p emptyLines

lexemeSpaces :: Parser a -> Parser a
lexemeSpaces p = liftA2 const p spaces

detachedModifier :: Char -> Parser Int
detachedModifier c = lexemeSpaces $ try $ do
  modifiers <- repeating c
  space
  pure modifiers

repeating :: Char -> Parser Int
repeating c = try $ do
  chars <- takeWhile1Chars (Just "Repeating") (== c)
  pure $ T.length chars

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

(>->) :: Applicative f => f a -> f b -> f a
(>->) a b = const <$> a <*> b

dbgThis :: (MonadParsecDbg e s p, Show a) => p a -> p a
dbgThis = dbg "this" . label "this"
