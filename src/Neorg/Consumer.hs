{-# LANGUAGE DeriveFunctor #-}
module Neorg.Consumer where

data Consumer a b = Next (a -> Consumer a b) | Done b deriving Functor


