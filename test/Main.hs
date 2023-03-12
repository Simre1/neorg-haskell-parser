module Main (main) where

import Test.Hspec

import Layer1


main :: IO ()
main = hspec $ do
  layer1
