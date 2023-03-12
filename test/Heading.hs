module Heading where

import Data.Text
import Neorg.Document
import Neorg.Parser.Block (blocks)
import Neorg.Parser.Type (parseTextAnySource)
import Test.HUnit
import Test.Hspec

parseBlocks :: Text -> IO Blocks
parseBlocks text = case parseTextAnySource blocks text of
  Left error -> assertFailure (unpack error)
  Right a -> pure a

headingSpec :: Spec
headingSpec = describe "Heading" $ do
  it "Heading and content" $ do
    let input = "* Heading\nBody"
        expectation = Blocks [Heading $ HeadingCons 1 Nothing (ParagraphCons [Word "Heading"]) (Blocks [PureBlock $ Paragraph $ ParagraphCons [Word "Body"]])]
    result <- parseBlocks input
    expectation @=? result

  it "Nested headings" $ do
    let input = "* Heading\n** Heading\n*** Heading"
        expectation =
          Blocks
            [ Heading $
                HeadingCons
                  1
                  Nothing
                  (ParagraphCons [Word "Heading"])
                  ( Blocks
                      [ Heading $
                          HeadingCons
                            2
                            Nothing
                            (ParagraphCons [Word "Heading"])
                            (Blocks [Heading $ HeadingCons 3 Nothing (ParagraphCons [Word "Heading"]) (Blocks [])])
                      ]
                  )
            ]
    result <- parseBlocks input
    expectation @=? result

  it "Nested headings with content" $ do
    let input = "* Heading\nSome content\n\n** Heading\n*** Heading"
        expectation =
          Blocks
            [ Heading $
                HeadingCons
                  1
                  Nothing
                  (ParagraphCons [Word "Heading"])
                  ( Blocks
                      [ PureBlock $ Paragraph (ParagraphCons [Word "Some", Space, Word "content"]),
                        Heading $
                          HeadingCons
                            2
                            Nothing
                            (ParagraphCons [Word "Heading"])
                            (Blocks [Heading $ HeadingCons 3 Nothing (ParagraphCons [Word "Heading"]) (Blocks [])])
                      ]
                  )
            ]
    result <- parseBlocks input
    expectation @=? result

  it "Paragraph breaks on Heading" $ do
    let input = "Paragraph\n* Heading"
        expectation =
          Blocks
            [ PureBlock $ Paragraph $ ParagraphCons [Word "Paragraph"],
              Heading $ HeadingCons 1 Nothing (ParagraphCons [Word "Heading"]) (Blocks [])
            ]
    result <- parseBlocks input
    expectation @=? result

  it "Same-level Headings do not nest" $ do
    let input = "* Heading\n* Heading"
        expectation =
          Blocks
            [ Heading $
                HeadingCons
                  1
                  Nothing
                  (ParagraphCons [Word "Heading"])
                  (Blocks []),
              Heading $
                HeadingCons
                  1
                  Nothing
                  (ParagraphCons [Word "Heading"])
                  (Blocks [])
            ]
    result <- parseBlocks input
    expectation @=? result

  it "Heading with task status" $ do
    let input = "* [x] Heading\nBody"
        expectation = Blocks [Heading $ HeadingCons 1 (Just Done) (ParagraphCons [Word "Heading"]) (Blocks [PureBlock $ Paragraph $ ParagraphCons [Word "Body"]])]
    result <- parseBlocks input
    expectation @=? result
