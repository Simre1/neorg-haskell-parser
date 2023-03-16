module Block where

import Data.Text
import Neorg.Document
import Neorg.Parser.Base (parseTextAnySource, emptyLines)
import Neorg.Parser.Block (blocks)
import Test.HUnit
import Test.Hspec

parseBlocks :: Text -> IO Blocks
parseBlocks text = case parseTextAnySource (emptyLines >> blocks) text of
  Left error -> assertFailure (unpack error)
  Right a -> pure a

parseBlocksShouldFail :: Text -> IO ()
parseBlocksShouldFail text = case parseTextAnySource (emptyLines >> blocks) text of
  Left error -> pure ()
  Right a -> assertFailure ("Should have failed, but returned:\n" ++ show a)

blockSpec :: Spec
blockSpec = describe "Block" $ do
  headingSpec
  listSpec
  quoteSpec
  delimiterSpec
  tagSpec

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
    let input = "* (x) Heading\nBody"
        expectation = Blocks [Heading $ HeadingCons 1 (Just Done) (ParagraphCons [Word "Heading"]) (Blocks [PureBlock $ Paragraph $ ParagraphCons [Word "Body"]])]
    result <- parseBlocks input
    expectation @=? result

  it "Heading char not the first one in line" $ do
    let input = "a * Heading"
        expectation = Blocks [PureBlock $ Paragraph $ ParagraphCons [Word "a", Space, Punctuation '*', Space, Word "Heading"]]
    result <- parseBlocks input
    expectation @=? result

  it "Heading with no space" $ do
    let input = "*Heading"
        expectation = Blocks [PureBlock $ Paragraph $ ParagraphCons [Punctuation '*', Word "Heading"]]
    result <- parseBlocks input
    expectation @=? result

listSpec :: Spec
listSpec = describe "List" $ do
  it "Single list item" $ do
    let input = "- list"
        expectation =
          Blocks
            [ PureBlock $
                List $
                  ListCons
                    1
                    UnorderedList
                    [ ( Nothing,
                        PureBlocks
                          [Paragraph $ ParagraphCons [Word "list"]]
                      )
                    ]
            ]
    result <- parseBlocks input
    expectation @=? result

  it "Multiple list items" $ do
    let input = "- list\n- list\n- list"
        item = (Nothing, PureBlocks [Paragraph $ ParagraphCons [Word "list"]])
        expectation =
          Blocks
            [ PureBlock $
                List $
                  ListCons
                    1
                    UnorderedList
                    [item, item, item]
            ]
    result <- parseBlocks input
    expectation @=? result

  it "Nested list items" $ do
    let input = "- list\n-- list\n-- list"
        item = (Nothing, PureBlocks [Paragraph $ ParagraphCons [Word "list"]])
        expectation =
          Blocks
            [ PureBlock $
                List $
                  ListCons
                    1
                    UnorderedList
                    [ ( Nothing,
                        PureBlocks
                          [ Paragraph $
                              ParagraphCons
                                [Word "list"],
                            List $ ListCons 2 UnorderedList [item, item]
                          ]
                      )
                    ]
            ]
    result <- parseBlocks input
    expectation @=? result

  it "Sub list items" $ do
    let input = "- list\n-- list\n- list"
        item = (Nothing, PureBlocks [Paragraph $ ParagraphCons [Word "list"]])
        expectation =
          Blocks
            [ PureBlock $
                List $
                  ListCons
                    1
                    UnorderedList
                    [ ( Nothing,
                        PureBlocks
                          [ Paragraph $
                              ParagraphCons
                                [Word "list"],
                            List $ ListCons 2 UnorderedList [item]
                          ]
                      ),
                      item
                    ]
            ]
    result <- parseBlocks input
    expectation @=? result

  it "Ordered list items" $ do
    let input = "~~ list\n~~ list"
        item = (Nothing, PureBlocks [Paragraph $ ParagraphCons [Word "list"]])
        expectation =
          Blocks
            [ PureBlock $
                List $
                  ListCons
                    2
                    OrderedList
                    [item, item]
            ]
    result <- parseBlocks input
    expectation @=? result

  it "List items with tasks" $ do
    let input = "- ( ) list\n- (x) list\n- (+) list"
        item task = (Just task, PureBlocks [Paragraph $ ParagraphCons [Word "list"]])
        expectation =
          Blocks
            [ PureBlock $
                List $
                  ListCons
                    1
                    UnorderedList
                    [item Undone, item Done, item Recurring]
            ]
    result <- parseBlocks input
    expectation @=? result

  it "List is not first character in line" $ do
    let input = "x - no list"
        expectation =
          Blocks
            [ PureBlock $
                Paragraph $
                  ParagraphCons [Word "x", Space, Punctuation '-', Space, Word "no", Space, Word "list"]
            ]
    result <- parseBlocks input
    expectation @=? result

  it "List with no space afterwads" $ do
    let input = "-no list"
        expectation =
          Blocks
            [ PureBlock $
                Paragraph $
                  ParagraphCons [Punctuation '-', Word "no", Space, Word "list"]
            ]
    result <- parseBlocks input
    expectation @=? result

  it "Lower-level list afterwards" $ do
    let input = "-- list\n- list"
        item = (Nothing, PureBlocks [Paragraph $ ParagraphCons [Word "list"]])
        expectation =
          Blocks
            [ PureBlock $
                List $
                  ListCons
                    2
                    UnorderedList
                    [item],
              PureBlock $
                List $
                  ListCons
                    1
                    UnorderedList
                    [item]
            ]
    result <- parseBlocks input
    expectation @=? result

  it "Paragraph after list" $ do
    let input = "- list\n\nParagraph"
        expectation =
          Blocks
            [ PureBlock $
                List $
                  ListCons
                    1
                    UnorderedList
                    [(Nothing, PureBlocks [Paragraph $ ParagraphCons [Word "list"]])],
              PureBlock $ Paragraph $ ParagraphCons [Word "Paragraph"]
            ]
    result <- parseBlocks input
    expectation @=? result

  it "Sublist after paragraph" $ do
    let input = "- list:\n    some text\n-- list"
        expectation =
          Blocks
            [ PureBlock $
                List $
                  ListCons
                    1
                    UnorderedList
                    [ ( Nothing,
                        PureBlocks
                          [ Paragraph $
                              ParagraphCons
                                [Word "list", Punctuation ':', Space, Word "some", Space, Word "text"],
                            List $ ListCons 2 UnorderedList [(Nothing, PureBlocks [Paragraph $ ParagraphCons [Word "list"]])]
                          ]
                      )
                    ]
            ]
    result <- parseBlocks input
    expectation @=? result

quoteSpec :: Spec
quoteSpec = describe "Quote" $ do
  it "Single quote" $ do
    let input = "> quote"
        expectation =
          Blocks
            [PureBlock $ Quote $ QuoteCons 1 Nothing $ PureBlocks [Paragraph $ ParagraphCons [Word "quote"]]]
    result <- parseBlocks input
    expectation @=? result

  it "Level 2 quote" $ do
    let input = ">> quote"
        expectation =
          Blocks
            [PureBlock $ Quote $ QuoteCons 2 Nothing $ PureBlocks [Paragraph $ ParagraphCons $ [Word "quote"]]]
    result <- parseBlocks input
    expectation @=? result

  it "Quote with no space" $ do
    let input = ">quote"
        expectation =
          Blocks
            [PureBlock $ Paragraph $ ParagraphCons [Punctuation '>', Word "quote"]]
    result <- parseBlocks input
    expectation @=? result

  it "Quote with content in front" $ do
    let input = "x > quote"
        expectation =
          Blocks
            [PureBlock $ Paragraph $ ParagraphCons [Word "x", Space, Punctuation '>', Space, Word "quote"]]
    result <- parseBlocks input
    expectation @=? result

  it "Nested quote" $ do
    let input = "> quote\n>> quote"
        quoteContent = Paragraph $ ParagraphCons $ [Word "quote"]
        expectation =
          Blocks
            [PureBlock $ Quote $ QuoteCons 1 Nothing $ PureBlocks [quoteContent, Quote $ QuoteCons 2 Nothing $ PureBlocks [quoteContent]]]
    result <- parseBlocks input
    expectation @=? result

  it "Quote stops at quote with lower level" $ do
    let input = "> quote\n>> quote\n> quote"
        quoteContent = Paragraph $ ParagraphCons $ [Word "quote"]
        expectation =
          Blocks
            [ PureBlock $
                Quote $
                  QuoteCons 1 Nothing $
                    PureBlocks
                      [quoteContent, Quote $ QuoteCons 2 Nothing $ PureBlocks [quoteContent]],
              PureBlock $
                Quote $
                  QuoteCons 1 Nothing $
                    PureBlocks
                      [quoteContent]
            ]
    result <- parseBlocks input
    expectation @=? result

delimiterSpec :: Spec
delimiterSpec = describe "Quote" $
  do
    it "Horizontal rule" $ do
      let input = "Some text\n___"
          expectation =
            Blocks
              [PureBlock $ Paragraph $ ParagraphCons [Word "Some", Space, Word "text"], HorizontalRule]
      result <- parseBlocks input
      expectation @=? result

    it "Weak delimiter" $ do
      let input = "* heading\n** heading\n---\nSome text"
          expectation =
            Blocks
              [ Heading $
                  HeadingCons 1 Nothing (ParagraphCons [Word "heading"]) $
                    Blocks
                      [ Heading $
                          HeadingCons 2 Nothing (ParagraphCons [Word "heading"]) $
                            Blocks [],
                        PureBlock $ Paragraph $ ParagraphCons [Word "Some", Space, Word "text"]
                      ]
              ]
      result <- parseBlocks input
      expectation @=? result

    it "Weak delimiter with no heading" $ do
      let input = "Some text\n---"
          expectation =
            Blocks
              [PureBlock $ Paragraph $ ParagraphCons [Word "Some", Space, Word "text"]]
      result <- parseBlocks input
      expectation @=? result

    it "Strong delimiter" $ do
      let input = "* heading\n** heading\n===\nSome text"
          expectation =
            Blocks
              [ Heading $
                  HeadingCons 1 Nothing (ParagraphCons [Word "heading"]) $
                    Blocks
                      [ Heading $
                          HeadingCons 2 Nothing (ParagraphCons [Word "heading"]) $
                            Blocks []
                      ],
                PureBlock $ Paragraph $ ParagraphCons [Word "Some", Space, Word "text"]
              ]

      result <- parseBlocks input
      expectation @=? result

    it "Strong delimiter with chars at line end" $ do
      let input = "* heading\n** heading\n=== Some text"
          expectation =
            Blocks
              [ Heading $
                  HeadingCons 1 Nothing (ParagraphCons [Word "heading"]) $
                    Blocks
                      [ Heading $
                          HeadingCons 2 Nothing (ParagraphCons [Word "heading"]) $
                            Blocks
                              [ PureBlock $
                                  Paragraph $
                                    ParagraphCons
                                      [ Punctuation '=',
                                        Punctuation '=',
                                        Punctuation '=',
                                        Space,
                                        Word "Some",
                                        Space,
                                        Word "text"
                                      ]
                              ]
                      ]
              ]

      result <- parseBlocks input
      expectation @=? result

    it "Weak delimiter with chars at line end" $ do
      let input = "* heading\n** heading\n--- Some text"
          expectation =
            Blocks
              [ Heading $
                  HeadingCons 1 Nothing (ParagraphCons [Word "heading"]) $
                    Blocks
                      [ Heading $
                          HeadingCons 2 Nothing (ParagraphCons [Word "heading"]) $
                            Blocks
                              [ PureBlock $
                                  List $
                                    ListCons
                                      3
                                      UnorderedList
                                      [ ( Nothing,
                                          PureBlocks
                                            [ Paragraph $
                                                ParagraphCons
                                                  [ Word "Some",
                                                    Space,
                                                    Word "text"
                                                  ]
                                            ]
                                        )
                                      ]
                              ]
                      ]
              ]

      result <- parseBlocks input
      expectation @=? result

tagSpec :: Spec
tagSpec = describe "Heading" $ do
  it "Verbatim ranged tag" $ do
    let input = "@code\ntest\n@end"
        expectation =
          Blocks [PureBlock $ VerbatimRangedTag $ VerbatimRangedTagCons "code" [] "test"]
    result <- parseBlocks input
    expectation @=? result

  it "Verbatim ranged tag multiple lines" $ do
    let input = "@code\ntest \n  test\n@end"
        expectation =
          Blocks [PureBlock $ VerbatimRangedTag $ VerbatimRangedTagCons "code" [] "test \n  test"]
    result <- parseBlocks input
    expectation @=? result

  it "Verbatim ranged tag with indentation" $ do
    let input = "  @code\n  test\n  test\n  @end"
        expectation =
          Blocks [PureBlock $ VerbatimRangedTag $ VerbatimRangedTagCons "code" [] "test\ntest"]
    result <- parseBlocks input
    expectation @=? result

  it "Verbatim ranged tag with too little space at end" $ do
    let input = "  @code\n  a\n@end"
    parseBlocksShouldFail input

  it "Verbatim ranged tag with too little space for code" $ do
    let input = "  @code\na\n  @end"
    parseBlocksShouldFail input

  it "Verbatim ranged tag with empty lines" $ do
    let input = "  @code\n \n  test\n  test\n  @end"
        expectation =
          Blocks [PureBlock $ VerbatimRangedTag $ VerbatimRangedTagCons "code" [] "test\ntest"]
    result <- parseBlocks input
    expectation @=? result
