module Main (main) where

import Test.Hspec

import Paragraph
import Heading


main :: IO ()
main = hspec $ do
  paragraphSpec
  headingSpec
