{-# LANGUAGE TypeFamilies #-}

module Neorg.Parser.Combinators where

import Control.Applicative
import Control.Monad
import Data.Sequence qualified as S
import Neorg.Parser.Type
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Debug

emptyLines :: (Token t ~ Char, MonadParsec e t p) => p ()
emptyLines = void $ takeWhileP (Just "Empty Lines") (<= ' ')

space :: (Token t ~ Char, MonadParsec e t p) => p ()
space = void $ char ' '

spaces :: Parser ()
spaces = void $ takeWhileP (Just "Spaces") (\c -> c <= ' ' && (c /= '\n' && c /= '\r'))

spaces1 :: Parser ()
spaces1 = void $ takeWhile1P (Just "1 space or more") (\c -> c <= ' ' && (c /= '\n' && c /= '\r'))

anyChar :: (Token t ~ Char, MonadParsec e t p) => p Char
anyChar = satisfy (const True)

lexeme :: (Token t ~ Char, MonadParsec e t p) => p a -> p a
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

repeating :: (MonadParsec e t p, Token t ~ Char) => Char -> p Int
repeating c = length <$> many1 (char c)

dbgThis :: (MonadParsecDbg e s p, Show a) => p a -> p a
dbgThis = dbg "this" . label "this"
