module Main (main) where

import Block 
import Paragraph
import Test.Hspec

main :: IO ()
main = hspec $ do
  paragraphSpec
  blockSpec
