module Parser where

import Cleff (Eff, runPure, runIOE, IOE)
import Cleff.State (State)
import Data.Data (Proxy (..))
import Data.Text (Text)
import Data.Time (defaultTimeLocale, parseTimeM)
import qualified Data.Vector as V
import Data.Void (Void)
import Effect.Logging
import Neorg.Document
import Neorg.Parser.Main hiding (parse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), HasCallStack)
import qualified Text.Megaparsec as P
import Type.Set (FromList, TypeSet (Empty))

parse :: P.ParsecT Void Text (Eff '[State CurrentHeadingLevel, Logging, IOE]) a -> Text -> IO a
parse p i = do
  res <- runIOE $ stdErrorLogging $ P.runParserT (runParserState (CurrentHeadingLevel I0) p) "test" i
  pure $ either (error . P.errorBundlePretty) id res

parserTests :: TestTree
parserTests =
  testGroup
    "Parser tests"
    [headingTests, paragraphTests, listTests, horizonalLineTests, markerTests, tagTests, definitionTest, quoteTests]

tagTests :: TestTree
tagTests =
  testGroup
    "Tag tests"
    [ testCase "Unknown tag" $ parse (tag @'Empty) "@unknown\n@end" ?== Nothing,
      testCase "Code tag" $ parse (tag @(FromList '["code"])) "@code \nhelloworld\n@end" ?== Just (SomeTag (Proxy @"code") Nothing "helloworld\n"),
      testCase "Math tag" $ parse (tag @(FromList '["math"])) "@math \nhelloworld\n@end" ?== Just (SomeTag (Proxy @"math") () "helloworld\n"),
      testCase "Comment tag" $ parse (tag @(FromList '["comment"])) "@comment \nhelloworld\n@end" ?== Just (SomeTag (Proxy @"comment") () "helloworld\n"),
      testCase "Embed image tag" $ parse (tag @(FromList '["embed"])) "@embed image \n   image.png\n@end" ?== Just (SomeTag (Proxy @"embed") "image" "image.png"),
      testCase "Document meta" $
        parse (tag @(FromList '["document.meta"])) "@document.meta\n  title: test\n  description:\n   author: simon\n  categories: \n  created: 2021-11-08\n  version: 0.1\n@end"
          ?== Just
            ( SomeTag
                (Proxy @"document.meta")
                ()
                ( DocumentMeta
                    { _documentTitle = Just "test",
                      _documentDescription = Nothing,
                      _documentAuthor = Just "simon",
                      _documentCategories = V.fromList [],
                      _documentCreated = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2021-11-08",
                      _documentVersion = Just "0.1"
                    }
                )
            ),
      testCase "Table" $
        parse
          (tag @(FromList '["table"]))
          "@table\n\
          \This is a row | And another element of that row\n\
          \This is a row on a new column | And another element of that row\n\
          \-\n\
          \The above line marks a delimiter |\n\
          \@end"
          ?== Just
            ( SomeTag
                (Proxy @"table")
                ()
                ( Table $
                    V.fromList
                      [ TableRowInlines $ V.fromList [ConcatInline $ V.fromList [Text "This", Space, Text "is", Space, Text "a", Space, Text "row"], ConcatInline $ V.fromList [Text "And", Space, Text "another", Space, Text "element", Space, Text "of", Space, Text "that", Space, Text "row"]],
                        TableRowInlines $ V.fromList [ConcatInline $ V.fromList [Text "This", Space, Text "is", Space, Text "a", Space, Text "row", Space, Text "on", Space, Text "a", Space, Text "new", Space, Text "column"], ConcatInline $ V.fromList [Text "And", Space, Text "another", Space, Text "element", Space, Text "of", Space, Text "that", Space, Text "row"]],
                        TableRowDelimiter,
                        TableRowInlines $ V.fromList [ConcatInline $ V.fromList [Text "The", Space, Text "above", Space, Text "line", Space, Text "marks", Space, Text "a", Space, Text "delimiter"]]
                      ]
                )
            ),
      testCase "Table with empty cells" $
        parse
          (tag @(FromList '["table"]))
          "@table\n\
          \A | B\n\
          \  | C\n\
          \@end"
          ?== Just
            ( SomeTag
                (Proxy @"table")
                ()
                ( Table $
                    V.fromList
                      [ TableRowInlines $ V.fromList [Text "A", Text "B"],
                        TableRowInlines $ V.fromList [ConcatInline V.empty, Text "C"]
                      ]
                )
            ),
      testCase "Proper tag indentation" $ parse (tag @(FromList '["code"])) "@code \n  nospaceshere\n    twospaces\n@end" ?== Just (SomeTag (Proxy @"code") Nothing "nospaceshere\n  twospaces\n"),
      testCase "Proper tag indentation 2" $ parse (tag @(FromList '["code"])) "@code \nnospaceshere\n  twospaces\n@end" ?== Just (SomeTag (Proxy @"code") Nothing "nospaceshere\n  twospaces\n"),
      testCase "Wrong tag indentation" $ parse (blocks @(FromList '["code"])) "  @code \n  nospaceshere\n    twospaces\n@end" ?== V.fromList [],
      testCase "Wrong tag indentation 2" $ parse (blocks @(FromList '["code"])) "  @code \n nospaceshere\n    twospaces\n  @end" ?== V.fromList []
    ]

horizonalLineTests :: TestTree
horizonalLineTests =
  testGroup
    "Horizonal Line tests"
    [ testCase "Horizonal line" $ parse horizonalLine "___" ?== (),
      testCase "Horizonal line" $ parse horizonalLine "________     " ?== ()
      -- TODO: Behavior unclear
      -- testCase "Not a horizonal line" $
      --   parse blocks "___ asda"
      --     ?== V.fromList
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
    [ testCase "Single-Line Bold" $ parse singleLineParagraph "*bold*" ?== Bold (Text "bold"),
      testCase "Single-Line Italic" $ parse singleLineParagraph "/italic/" ?== Italic (Text "italic"),
      testCase "Single-Line Underline" $ parse singleLineParagraph "_underline_" ?== Underline (Text "underline"),
      testCase "Single-Line Strikethrough" $ parse singleLineParagraph "-strike-" ?== Strikethrough (Text "strike"),
      testCase "Single-Line Superscript" $ parse singleLineParagraph "^super^" ?== Superscript (Text "super"),
      testCase "Single-Line Subscript" $ parse singleLineParagraph ",sub," ?== Subscript (Text "sub"),
      testCase "Single-Line Spoiler" $ parse singleLineParagraph "|spoiler|" ?== Spoiler (Text "spoiler"),
      testCase "Single-Line Math" $ parse singleLineParagraph "$math$" ?== Math "math",
      testCase "Single-Line Verbatim" $ parse singleLineParagraph "`verbatim`" ?== Verbatim "verbatim",
      testCase "Single-Line Two Bolds" $ parse singleLineParagraph "*bold1* *bold2*" ?== ConcatInline (V.fromList [Bold (Text "bold1"), Space, Bold (Text "bold2")]),
      testCase "Bold and italic word" $ parse singleLineParagraph "*/bolditalic/*" ?== Bold (Italic (Text "bolditalic")),
      testCase "~ Symbol" $ parse singleLineParagraph "Text~\n* NoHeading" ?== ConcatInline (V.fromList [Text "Text*", Space, Text "NoHeading"]),
      testCase "Paragraphs separated with Break" $
        parse (blocks @'Empty) "Text1\n\nText2"
          ?== V.fromList
            [ PureBlock $ Paragraph (Text "Text1"),
              PureBlock $ Paragraph (Text "Text2")
            ],
      testCase "Two sentences with blank space" $
        parse (blocks @'Empty) "Simple sentence.\n   \n   Another sentence.\n\n"
          ?== V.fromList
            [ PureBlock $
                Paragraph
                  ( ConcatInline $
                      V.fromList
                        [Text "Simple", Space, Text "sentence."]
                  ),
              PureBlock $
                Paragraph
                  ( ConcatInline $
                      V.fromList [Text "Another", Space, Text "sentence."]
                  )
            ],
      testCase "Single-Line intersecting Bold" $ parse singleLineParagraph ":*bold*:" ?== Bold (Text "bold"),
      testCase "Single-Line intersecting Italic" $ parse singleLineParagraph ":/italic/:" ?== Italic (Text "italic"),
      testCase "Single-Line intersecting Underline" $ parse singleLineParagraph ":_underline_:" ?== Underline (Text "underline"),
      testCase "Single-Line intersecting Strikethrough" $ parse singleLineParagraph ":-strike-:" ?== Strikethrough (Text "strike"),
      testCase "Single-Line intersecting Superscript" $ parse singleLineParagraph ":^super^:" ?== Superscript (Text "super"),
      testCase "Single-Line intersecting Subscript" $ parse singleLineParagraph ":,sub,:" ?== Subscript (Text "sub"),
      testCase "Single-Line intersecting Spoiler" $ parse singleLineParagraph ":|spoiler|:" ?== Spoiler (Text "spoiler"),
      testCase "Single-Line intersecting Math" $ parse singleLineParagraph ":$math$:" ?== Math "math",
      testCase "Single-Line intersecting Verbatim" $ parse singleLineParagraph ":`verbatim`:" ?== Verbatim "verbatim",
      testCase "Escape bold character" $ parse singleLineParagraph "\\*test*" ?== Text "*test*",
      testCase "Verbatim ," $ parse singleLineParagraph ",`,`" ?== ConcatInline (V.fromList [Text ",", Verbatim ","]),
      testCase "Verbatim , 2" $ parse singleLineParagraph "\\{`,`,#}" ?== ConcatInline (V.fromList [Text "{", Verbatim ",", Text ",#}"]),
      testCase "Hyperlink" $ parse paragraph "{https://example.com}[example]" ?== Link (LinkCons (LinkTargetUrl "https://example.com") (Just $ Text "example") Nothing),
      testCase "Definition link" $
        parse paragraph "{$     def}"
          ?== Link
            (LinkCons (LinkTargetCurrentDocument $ LinkTargetDefinition $ TargetName $ Text "def") Nothing Nothing),
      testCase
        "Heading link"
        $ parse paragraph "{** Heading2}[Go to Heading]" ?== Link (LinkCons (LinkTargetCurrentDocument $ LinkTargetHeading I1 $ TargetName $ Text "Heading2") (Just (ConcatInline $ V.fromList [Text "Go", Space, Text "to", Space, Text "Heading"])) Nothing),
      testCase
        "Absolute file link"
        $ parse paragraph "{@   /absolute/file}" ?== Link (LinkCons (LinkTargetFile $ Absolute "absolute/file") Nothing Nothing),
      testCase
        "Relative file link"
        $ parse paragraph "{@ file}" ?== Link (LinkCons (LinkTargetFile $ Relative "file") Nothing Nothing),
      testCase
        "Current workspace file link"
        $ parse paragraph "{@ $/file}" ?== Link (LinkCons (LinkTargetFile $ CurrentWorkspace "file") Nothing Nothing),
      testCase
        "Workspace file link"
        $ parse paragraph "{@ $home/file}" ?== Link (LinkCons (LinkTargetFile $ Workspace "home" "file") Nothing Nothing),
      testCase
        "Norg link"
        $ parse paragraph "{:file:}" ?== Link (LinkCons (LinkTargetNorgFile (Relative "file") Nothing) Nothing Nothing),
      testCase
        "Norg link with target"
        $ parse paragraph "{:file:* Heading}" ?== Link (LinkCons (LinkTargetNorgFile (Relative "file") (Just $ LinkTargetHeading I0 $ TargetName $ Text "Heading")) Nothing Nothing)
    ]

markerTests :: TestTree
markerTests =
  testGroup
    "Marker tests"
    [ testCase "Simple Markup" $ parse marker "| Simple marker" ?== MarkerCons "simple-marker" "Simple marker",
      testCase "Block Marker" $ parse (blocks @'Empty) "| Simple marker" ?== V.fromList [Marker $ MarkerCons "simple-marker" "Simple marker"],
      testCase "Not a Marker" $ parse (blocks @'Empty) "|" ?== V.fromList [PureBlock . Paragraph $ Text "|"]
    ]

listTests :: TestTree
listTests =
  testGroup
    "List tests"
    [ testCase "Single unordered item" $
        parse (runParserReader (CurrentListLevel I0) (unorderedList @'Empty)) "- test1"
          ?== UnorderedListCons
            { _uListLevel = I0,
              _uListItems =
                V.singleton
                  . V.singleton
                  $ Paragraph (Text "test1")
            },
      testCase "Two unordered items" $
        parse (runParserReader (CurrentListLevel I0) (unorderedList @'Empty)) "- test1\n- test2"
          ?== UnorderedListCons
            { _uListLevel = I0,
              _uListItems =
                V.fromList
                  [ V.singleton $
                      Paragraph (Text "test1"),
                    V.singleton $
                      Paragraph (Text "test2")
                  ]
            },
      testCase "Ordered List" $
        parse (runParserReader (CurrentListLevel I0) (orderedList @'Empty)) "~~ test1\n~~ test2"
          ?== OrderedListCons
            { _oListLevel = I1,
              _oListItems =
                V.fromList
                  [ V.singleton $
                      Paragraph (Text "test1"),
                    V.singleton $
                      Paragraph (Text "test2")
                  ]
            },
      testCase "Task List" $
        parse (runParserReader (CurrentListLevel I0) (taskList @'Empty)) "- [x] Done\n- [*] Pending"
          ?== TaskListCons
            { _tListLevel = I0,
              _tListItems =
                V.fromList
                  [ ( TaskDone,
                      V.singleton $
                        Paragraph (Text "Done")
                    ),
                    ( TaskPending,
                      V.singleton $
                        Paragraph (Text "Pending")
                    )
                  ]
            },
      testCase "List with Paragraph" $
        parse (blocks @'Empty) "Paragraph\n- List\n\nParagraph"
          ?== V.fromList
            [ PureBlock $ Paragraph (Text "Paragraph"),
              PureBlock $
                List $
                  UnorderedList $
                    UnorderedListCons
                      { _uListLevel = I0,
                        _uListItems =
                          V.singleton
                            . V.singleton
                            $ Paragraph (Text "List")
                      },
              PureBlock $ Paragraph (Text "Paragraph")
            ],
      testCase "Sublists" $
        parse (blocks @'Empty) "- l1: test\n~~ o1\n~~ o2\n- l2"
          ?== V.fromList
            [ PureBlock $
                List $
                  UnorderedList $
                    UnorderedListCons
                      { _uListLevel = I0,
                        _uListItems =
                          V.fromList
                            [ V.fromList
                                [ Paragraph (ConcatInline $ V.fromList [Text "l1:", Space, Text "test"]),
                                  List
                                    ( OrderedList $
                                        OrderedListCons
                                          { _oListLevel = I1,
                                            _oListItems = V.fromList [V.singleton (Paragraph (Text "o1")), V.singleton (Paragraph (Text "o2"))]
                                          }
                                    )
                                ],
                              V.singleton $ Paragraph (Text "l2")
                            ]
                      }
            ],
      testCase "Escaped list" $
        parse (blocks @'Empty) "\\- test1"
          ?== V.singleton
            ( PureBlock $
                Paragraph
                  ( ConcatInline $ V.fromList [Text "-", Space, Text "test1"]
                  )
            )
    ]

headingTests :: TestTree
headingTests =
  testGroup
    "Heading tests"
    [ testCase "Heading" $
        parse heading "* Heading"
          ?== HeadingCons
            { _headingText = Text "Heading",
              _headingLevel = I0
            },
      testCase "Equal Headings" $
        parse (blocks @'Empty) "* Heading1\n* Heading2"
          ?== V.fromList
            [ Heading $
                HeadingCons
                  { _headingText = Text "Heading1",
                    _headingLevel = I0
                  },
              Delimiter WeakDelimiter,
              Heading $
                HeadingCons
                  { _headingText = Text "Heading2",
                    _headingLevel = I0
                  }
            ],
      testCase "No Headings" $
        parse (blocks @'Empty) "*Heading1"
          ?== V.singleton (PureBlock $ Paragraph $ Text "*Heading1"),
      testCase "Nested Headings" $
        parse (blocks @'Empty) "* Heading\n** SubHeading"
          ?== V.fromList
            [ Heading $
                HeadingCons
                  { _headingText = Text "Heading",
                    _headingLevel = I0
                  },
              Heading $
                HeadingCons
                  { _headingText = Text "SubHeading",
                    _headingLevel = I1
                  }
            ],
      testCase "Headings with Paragraph" $
        parse (blocks @'Empty) "* Heading1\nExample1\n* Heading2\nExample2"
          ?== V.fromList
            [ Heading $
                HeadingCons
                  { _headingText = Text "Heading1",
                    _headingLevel = I0
                  },
              PureBlock $ Paragraph $ Text "Example1",
              Delimiter WeakDelimiter,
              Heading $
                HeadingCons
                  { _headingText = Text "Heading2",
                    _headingLevel = I0
                  },
              PureBlock $ Paragraph $ Text "Example2"
            ],
      testCase "Weak delimiter" $
        parse (blocks @'Empty) "* Heading1\n---\n** Heading2"
          ?== V.fromList
            [ Heading $
                HeadingCons
                  { _headingText = Text "Heading1",
                    _headingLevel = I0
                  },
              Delimiter
                WeakDelimiter,
              Heading $
                HeadingCons
                  { _headingText = Text "Heading2",
                    _headingLevel = I1
                  }
            ],
      testCase "Strong delimiter" $
        parse (blocks @'Empty) "* Heading1\n===\n** Heading2"
          ?== V.fromList
            [ Heading $
                HeadingCons
                  { _headingText = Text "Heading1",
                    _headingLevel = I0
                  },
              Delimiter StrongDelimiter,
              Heading $
                HeadingCons
                  { _headingText = Text "Heading2",
                    _headingLevel = I1
                  }
            ]
    ]

definitionTest :: TestTree
definitionTest =
  testGroup
    "Definition tests"
    [ testCase "Single-Line Definition" $
        parse (blocks @'Empty) "$ Single-Paragraph\nsingle-line"
          ?== V.fromList
            [ Definition $
                DefinitionCons
                  { _definitionObject = Text "Single-Paragraph",
                    _definitionContent = V.singleton (Paragraph (Text "single-line"))
                  }
            ],
      testCase "Multi-Line Definition" $
        parse (blocks @'Empty) "$$ Multi-Line\nline1\n\nline2\n$$"
          ?== V.fromList
            [ Definition $
                DefinitionCons
                  { _definitionObject = Text "Multi-Line",
                    _definitionContent = V.fromList [Paragraph (Text "line1"), Paragraph (Text "line2")]
                  }
            ],
      testCase "Complex Multi-Line Definition" $
        parse (blocks @'Empty) "$$ *Multi*-Line\n- list1\n- list2\n\n> some quote\n$$"
          ?== V.fromList
            [ Definition $
                DefinitionCons
                  { _definitionObject = ConcatInline $ V.fromList [Bold (Text "Multi"), Text "-Line"],
                    _definitionContent =
                      V.fromList
                        [ List $
                            UnorderedList $
                              UnorderedListCons {_uListLevel = I0, _uListItems = V.fromList [V.singleton $ Paragraph $ Text "list1", V.singleton $ Paragraph $ Text "list2"]},
                          Quote (QuoteCons {_quoteLevel = I0, _quoteContent = ConcatInline $ V.fromList [Text "some", Space, Text "quote"]})
                        ]
                  }
            ]
    ]

quoteTests :: TestTree
quoteTests =
  testGroup
    "Quote tests"
    [ testCase "Single-Line quote" $
        parse (blocks @'Empty) "> line1"
          ?== V.fromList
            [ PureBlock $
                Quote $
                  QuoteCons
                    { _quoteLevel = I0,
                      _quoteContent = Text "line1"
                    }
            ],
      testCase
        "Multi-Line quote"
        $ parse (blocks @'Empty) "> line1\n> line2"
          ?== V.fromList
            [ PureBlock $
                Quote $
                  QuoteCons
                    { _quoteLevel = I0,
                      _quoteContent = ConcatInline $ V.fromList [Text "line1", Space, Text "line2"]
                    }
            ],
      testCase
        "Interleaved quote"
        $ parse (blocks @'Empty) "> line1\n>> line2\n> line3"
          ?== V.fromList
            [ PureBlock $
                Quote $
                  QuoteCons
                    { _quoteLevel = I0,
                      _quoteContent = Text "line1"
                    },
              PureBlock $
                Quote $
                  QuoteCons
                    { _quoteLevel = I1,
                      _quoteContent = Text "line2"
                    },
              PureBlock $
                Quote $
                  QuoteCons
                    { _quoteLevel = I0,
                      _quoteContent = Text "line3"
                    }
            ],
      testCase
        "Separate quotes"
        $ parse (blocks @'Empty) "> line1\n\n> line2"
          ?== V.fromList
            [ PureBlock $
                Quote $
                  QuoteCons
                    { _quoteLevel = I0,
                      _quoteContent = Text "line1"
                    },
              PureBlock $
                Quote $
                  QuoteCons
                    { _quoteLevel = I0,
                      _quoteContent = Text "line2"
                    }
            ]
    ]

(?==) :: (Eq a, Show a, HasCallStack) => IO a -> a -> IO ()
(?==) a b = a >>= (@?= b)
