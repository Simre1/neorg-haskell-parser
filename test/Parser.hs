module Parser where

import Control.Monad.Trans.State
import Data.Text
import qualified Data.Vector as V
import Neorg.Document
import Neorg.Parser hiding (parse)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Megaparsec as P
import Type.Set (TypeSet(Empty), FromList)
import Data.Data (Proxy(..))

parse :: Parser a -> Text -> a
parse p i =
  let res = evalState (P.runParserT p "test" i) defaultParserState
   in either (error . P.errorBundlePretty) id res

parserTests :: TestTree
parserTests =
  testGroup
    "Parser tests"
    [headingTests, paragraphTests, listTests, horizonalLineTests, markerTests, tagTests]

tagTests :: TestTree
tagTests =   testGroup
    "Tag tests"
    [ testCase "Unknown tag" $ parse (tag @Empty) "@unknown\n@end" @?= Nothing,
      testCase "Code tag" $ parse (tag @(FromList '["code"])) "@code \nhelloworld\n@end" @?= Just (SomeTag (Proxy @"code") Nothing "helloworld\n")
      ]

horizonalLineTests :: TestTree
horizonalLineTests =
  testGroup
    "Horizonal Line tests"
    [ testCase "Horizonal line" $ parse horizonalLine "___" @?= (),
      testCase "Horizonal line" $ parse horizonalLine "________     " @?= ()
      -- TODO: Behavior unclear
      -- testCase "Not a horizonal line" $
      --   parse blocks "___ asda"
      --     @?= V.fromList
      --       [ Paragraph
      --           ( ConcatInline $
      --               V.fromList
      --                 [Text "___", Space, Text "asda"]
      --           )
      --       ]
    ]

paragraphTests :: TestTree
paragraphTests =
  testGroup
    "Paragraph tests"
    [ testCase "Single-Line Bold" $ parse singleLineParagraph "*bold*" @?= Bold (Text "bold"),
      testCase "Single-Line Italic" $ parse singleLineParagraph "/italic/" @?= Italic (Text "italic"),
      testCase "Single-Line Underline" $ parse singleLineParagraph "_underline_" @?= Underline (Text "underline"),
      testCase "Single-Line Strikethrough" $ parse singleLineParagraph "-strike-" @?= Strikethrough (Text "strike"),
      testCase "Single-Line Superscript" $ parse singleLineParagraph "^super^" @?= Superscript (Text "super"),
      testCase "Single-Line Subscript" $ parse singleLineParagraph ",sub," @?= Subscript (Text "sub"),
      testCase "Single-Line Spoiler" $ parse singleLineParagraph "|spoiler|" @?= Spoiler (Text "spoiler"),
      testCase "Single-Line Math" $ parse singleLineParagraph "$math$" @?= Math "math",
      testCase "Single-Line Verbatim" $ parse singleLineParagraph "`verbatim`" @?= Verbatim "verbatim",
      testCase "Single-Line Two Bolds" $ parse singleLineParagraph "*bold1* *bold2*" @?= ConcatInline (V.fromList [Bold (Text "bold1"), Space, Bold (Text "bold2")]),
      testCase "Bold and italic word" $ parse singleLineParagraph "*/bolditalic/*" @?= Bold (Italic (Text "bolditalic")),
      testCase "~ Symbol" $ parse singleLineParagraph "Text~\n* NoHeading" @?= ConcatInline (V.fromList [Text "Text*", Space, Text "NoHeading"]),
      testCase "Paragraphs separated with Break" $
        parse (blocks @Empty) "Text1\n\nText2"
          @?= V.fromList
            [ Paragraph (Text "Text1"),
              Paragraph (Text "Text2")
            ],
      testCase "Two sentences with blank space" $
        parse (blocks @Empty) "Simple sentence.\n   \n   Another sentence.\n\n"
          @?= V.fromList
            [ Paragraph
                ( ConcatInline $
                    V.fromList
                      [Text "Simple", Space, Text "sentence."]
                ),
              Paragraph
                ( ConcatInline $
                    V.fromList [Text "Another", Space, Text "sentence."]
                )
            ]
    ]

markerTests :: TestTree
markerTests =
  testGroup
    "Marker tests"
    [ testCase "Simple Markup" $ parse marker "| Simple marker" @?= MarkerCons "simple-marker" "Simple marker",
      testCase "Block Marker" $ parse (blocks @Empty) "| Simple marker" @?= V.fromList [Marker $ MarkerCons "simple-marker" "Simple marker"],
      testCase "Not a Marker" $ parse (blocks @Empty) "|" @?= V.fromList [Paragraph $ Text "|"]
    ]

listTests :: TestTree
listTests =
  testGroup
    "List tests"
    [ testCase "Single unordered item" $
        parse (unorderedList I0) "- test1"
          @?= UnorderedListCons
            { _uListLevel = I0,
              _uListItems =
                V.singleton
                  . V.singleton
                  $ ListParagraph (Text "test1")
            },
      testCase "Two unordered items" $
        parse (unorderedList I0) "- test1\n- test2"
          @?= UnorderedListCons
            { _uListLevel = I0,
              _uListItems =
                V.fromList
                  [ V.singleton $
                      ListParagraph (Text "test1"),
                    V.singleton $
                      ListParagraph (Text "test2")
                  ]
            },
      testCase "Ordered List" $
        parse (orderedList I0) "~~ test1\n~~ test2"
          @?= OrderedListCons
            { _oListLevel = I1,
              _oListItems =
                V.fromList
                  [ V.singleton $
                      ListParagraph (Text "test1"),
                    V.singleton $
                      ListParagraph (Text "test2")
                  ]
            },
      testCase "Task List" $
        parse (taskList I0) "- [x] Done\n- [*] Pending"
          @?= TaskListCons
            { _tListLevel = I0,
              _tListItems =
                V.fromList
                  [ ( TaskDone,
                      V.singleton $
                        ListParagraph (Text "Done")
                    ),
                    ( TaskPending,
                      V.singleton $
                        ListParagraph (Text "Pending")
                    )
                  ]
            },
      testCase "List with Paragraph" $
        parse (blocks @Empty) "Paragraph\n- List\n\nParagraph"
          @?= V.fromList
            [ Paragraph (Text "Paragraph"),
              List $
                UnorderedList $
                  UnorderedListCons
                    { _uListLevel = I0,
                      _uListItems =
                        V.singleton
                          . V.singleton
                          $ ListParagraph (Text "List")
                    },
              Paragraph (Text "Paragraph")
            ],
      testCase "Sublists" $
        parse (blocks @Empty) "- l1: test\n~~ o1\n~~ o2\n- l2"
          @?= V.fromList
            [ List $
                UnorderedList $
                  UnorderedListCons
                    { _uListLevel = I0,
                      _uListItems =
                        V.fromList
                          [ V.fromList
                              [ ListParagraph (ConcatInline $ V.fromList [Text "l1:", Space, Text "test"]),
                                SubList
                                  ( OrderedList $
                                      OrderedListCons
                                        { _oListLevel = I1,
                                          _oListItems = V.fromList [V.singleton (ListParagraph (Text "o1")), V.singleton (ListParagraph (Text "o2"))]
                                        }
                                  )
                              ],
                            V.singleton $ ListParagraph (Text "l2")
                          ]
                    }
            ]
    ]

headingTests :: TestTree
headingTests =
  testGroup
    "Heading tests"
    [ testCase "Heading" $
        parse (heading @Empty) "* Heading"
          @?= HeadingCons
            { _headingText = Text "Heading",
              _headingLevel = I0,
              _headingContent = V.empty
            },
      testCase "Equal Headings" $
        parse (blocks @Empty) "* Heading1\n* Heading2"
          @?= V.fromList
            [ Heading $
                HeadingCons
                  { _headingText = Text "Heading1",
                    _headingLevel = I0,
                    _headingContent = V.empty
                  },
              Heading $
                HeadingCons
                  { _headingText = Text "Heading2",
                    _headingLevel = I0,
                    _headingContent = V.empty
                  }
            ],
      testCase "No Headings" $
        parse (blocks @Empty) "*Heading1"
          @?= V.singleton (Paragraph $ Text "*Heading1"),
      testCase "Nested Headings" $
        parse (heading @Empty) "* Heading\n** SubHeading"
          @?= HeadingCons
            { _headingText = Text "Heading",
              _headingLevel = I0,
              _headingContent =
                V.singleton
                  ( Heading $
                      HeadingCons
                        { _headingText = Text "SubHeading",
                          _headingLevel = I1,
                          _headingContent = V.empty
                        }
                  )
            },
      testCase "Headings with Paragraph" $
        parse (blocks @Empty) "* Heading1\nExample1\n* Heading2\nExample2"
          @?= V.fromList
            [ Heading $
                HeadingCons
                  { _headingText = Text "Heading1",
                    _headingLevel = I0,
                    _headingContent =
                      V.singleton (Paragraph $ Text "Example1")
                  },
              Heading $
                HeadingCons
                  { _headingText = Text "Heading2",
                    _headingLevel = I0,
                    _headingContent =
                      V.singleton (Paragraph $ Text "Example2")
                  }
            ],
      testCase "Weak delimiter" $
        parse (blocks @Empty) "* Heading1\n---\n** Heading2"
          @?= V.fromList
            [ Heading $
                HeadingCons
                  { _headingText = Text "Heading1",
                    _headingLevel = I0,
                    _headingContent = V.empty
                  },
              Heading $
                HeadingCons
                  { _headingText = Text "Heading2",
                    _headingLevel = I1,
                    _headingContent = V.empty
                  }
            ],
      testCase "Strong delimiter" $
        parse (blocks @Empty) "* Heading1\n===\n** Heading2"
          @?= V.fromList
            [ Heading $
                HeadingCons
                  { _headingText = Text "Heading1",
                    _headingLevel = I0,
                    _headingContent = V.empty
                  },
              Heading $
                HeadingCons
                  { _headingText = Text "Heading2",
                    _headingLevel = I1,
                    _headingContent = V.empty
                  }
            ]
    ]
