module Main where

import Test.Tasty
import Tokenizer

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [tokenizerTests]
