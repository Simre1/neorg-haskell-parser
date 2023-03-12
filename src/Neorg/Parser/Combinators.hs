module Neorg.Parser.Combinators where

import Control.Applicative
import Control.Monad
import Data.Sequence qualified as S
import Neorg.Parser.Type
import Text.Megaparsec
import Text.Megaparsec.Char (char)

emptyLines :: Parser ()
emptyLines = void $ takeWhileP (Just "Empty Lines") (<= ' ')

space :: Parser ()
space = void $ char ' '

spaces :: Parser ()
spaces = void $ takeWhileP (Just "Spaces") (\c -> c <= ' ' && (c /= '\n' && c /= '\r'))

spaces1 :: Parser ()
spaces1 = void $ takeWhile1P (Just "1 space or more") (\c -> c <= ' ' && (c /= '\n' && c /= '\r'))

lexemeEmptyLines :: Parser a -> Parser a
lexemeEmptyLines p = liftA2 const p emptyLines

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
