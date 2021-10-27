module Tokenizer where

import Data.Text
import Neorg.Document (IndentationLevel (..))
import Neorg.Token
import Test.Tasty
import Test.Tasty.HUnit

makeTokens :: Text -> [Token tags]
makeTokens = flip asTokens listTokens

tokenizerTests :: TestTree
tokenizerTests =
  testGroup
    "Tokenizer tests"
    [ testCase "EOF" $ makeTokens "" @?= [End],
      attachedTokenTests,
      detachedTokenTests,
      breaks,
      delimitingTokens,
      trailingModifier
    ]

attachedTokenTests :: TestTree
attachedTokenTests =
  testGroup
    "Attached tokens"
    [ testCase "Bold word" $ makeTokens "*bold*" @?= [bold Open, Word "bold", bold Closed, End],
      testCase "Non-attached *" $ makeTokens "*bold *" @?= [bold Open, Word "bold ", Word "*", End],
      testCase "Non-attached *" $ makeTokens "*bold *" @?= [bold Open, Word "bold ", Word "*", End],
      testCase "Italic word" $ makeTokens "/italic/" @?= [italic Open, Word "italic", italic Closed, End],
      testCase "Underline word" $ makeTokens "_underline_" @?= [underline Open, Word "underline", underline Closed, End],
      testCase "Strikethrough word" $ makeTokens "-strikethrough-" @?= [strikethrough Open, Word "strikethrough", strikethrough Closed, End],
      testCase "Superscript word" $ makeTokens "^superscript^" @?= [superscript Open, Word "superscript", superscript Closed, End],
      testCase "Subscript word" $ makeTokens ",subscript," @?= [subscript Open, Word "subscript", subscript Closed, End],
      testCase "Normal comma" $ makeTokens "normal, comma" @?= [Word "normal", Word ", ", Word "comma", End],
      testCase "Spoiler word" $ makeTokens "|spoiler|" @?= [spoiler Open, Word "spoiler", spoiler Closed, End],
      testCase "Verbatim word" $ makeTokens "`verbatim`" @?= [verbatim Open, Word "verbatim", verbatim Closed, End],
      testCase "Math word" $ makeTokens "$math$" @?= [math Open, Word "math", math Closed, End],
      testCase "Variable word" $ makeTokens "=variable=" @?= [variable Open, Word "variable", variable Closed, End],
      testCase "Bold and Italic word" $ makeTokens "*/word/*" @?= [bold Open, italic Open, Word "word", italic Closed, bold Closed, End]
    ]

breaks :: TestTree
breaks =
  testGroup
    "Breaks"
    [ testCase "No Break" $ makeTokens "hello\ntest" @?= [Word "hello", Word "test", End],
      testCase "Single Break" $ makeTokens "hello\n\ntest" @?= [Word "hello", Break, Word "test", End],
      testCase "Multiple lines" $ makeTokens "hello\n\n\n\ntest" @?= [Word "hello", Break, Word "test", End]
    ]

detachedTokenTests :: TestTree
detachedTokenTests =
  testGroup
    "Detached tokens"
    [ testCase "I0 Heading" $ makeTokens "* test" @?= [heading I0, Word "test", End],
      testCase "Sub Heading" $ makeTokens "*   main \n ** sub" @?= [heading I0, Word "main ", heading I1, Word "sub", End],
      testCase "Simple unordered List" $ makeTokens "- list1\n- list2" @?= [uList I0, Word "list1", uList I0, Word "list2", End],
      testCase "Simple unorderd List with Text" $
        makeTokens "- list1: test1\n- list2: test2"
          @?= [uList I0, Word "list1", Word ": ", Word "test1", uList I0, Word "list2", Word ": ", Word "test2", End],
      testCase
        "List with sublist"
        $ makeTokens "- list1: level1 \n -- list2: level2 \n --- list3: level3 \n - list1: level1"
          @?= [ uList I0,
                Word "list1",
                Word ": ",
                Word "level1 ",
                uList I1,
                Word "list2",
                Word ": ",
                Word "level2 ",
                uList I2,
                Word "list3",
                Word ": ",
                Word "level3 ",
                uList I0,
                Word "list1",
                Word ": ",
                Word "level1",
                End
              ],
      testCase "Simple ordered List" $ makeTokens "~ list1\n~ list2" @?= [oList I0, Word "list1", oList I0, Word "list2", End],
      testCase "Simple Task List" $ makeTokens "- [ ] list1\n-- [x] list2\n - [*] list3" @?= [tList TaskUndone I0, Word "list1", tList TaskDone I1, Word "list2", tList TaskPending I0, Word "list3", End],
      testCase "Wrong task list" $ makeTokens "-[ ]" @?= [Word "-", Word "[ ", Word "]", End],
      testCase "Insertion" $ makeTokens "= ToC" @?= [DetachedToken TInsertion, Word "ToC", End],
      testCase "Insertion with text afterwards" $ makeTokens "= ToC\nsome text" @?= [DetachedToken TInsertion, Word "ToC", Break, Word "some ", Word "text", End],
      testCase "Definition" $ makeTokens "= object" @?= [DetachedToken TInsertion, Word "object", End],
      testCase "Definition with text afterwards" $ makeTokens ": object\nsome text" @?= [DefinitionToken TSingleDefinition, Word "object", Break, Word "some ", Word "text", End],
      testCase "Marker" $ makeTokens "| Marker" @?= [DetachedToken TMarker, Word "Marker", End],
      testCase "Marker with text afterwards" $ makeTokens "| marker\nsome text" @?= [DetachedToken TMarker, Word "marker", Break, Word "some ", Word "text", End],
      testCase "Horizonal line" $ makeTokens "___ " @?= [DelimitingToken THorizontalDelimiter, End],
      testCase "Horizonal line with text afterwards" $ makeTokens "______ \nsome text" @?= [DelimitingToken THorizontalDelimiter, Word "some ", Word "text", End]
    ]

delimitingTokens :: TestTree
delimitingTokens =
  testGroup
    "Delimiting tokens"
    [ testCase "Strong delimiter with file end" $ makeTokens "===" @?= [DelimitingToken TStrongDelimiter, End],
      testCase "Strong delimiter with new line" $ makeTokens "======== \n" @?= [DelimitingToken TStrongDelimiter, End],
      testCase "Wrong strong delimiter" $ makeTokens "=== s\n" @?= [Word "=", Word "=", Word "= ", Word "s", End],
      testCase "Weak delimiter with EOF" $ makeTokens "---" @?= [DelimitingToken TWeakDelimiter, End],
      testCase "Weak delimiter with new line" $ makeTokens "----------- \n" @?= [DelimitingToken TWeakDelimiter, End],
      testCase "Wrong weak delimiter" $ makeTokens "--- s\n" @?= [DetachedToken (TUnorderedList I2), Word "s", End]
    ]

trailingModifier :: TestTree
trailingModifier =
  testGroup "TrailingModifier" [
    testCase "List with trailing modifier" $ makeTokens "- list ~\n - not a list" @?= [DetachedToken (TUnorderedList I0), Word "list ", Word "- ", Word "not ", Word "a ", Word "list", End]
  ]

bold oc = AttachedToken (AttachedT oc TBold)

italic oc = AttachedToken (AttachedT oc TItalic)

underline oc = AttachedToken (AttachedT oc TUnderline)

strikethrough oc = AttachedToken (AttachedT oc TStrikethrough)

superscript oc = AttachedToken (AttachedT oc TSuperscript)

subscript oc = AttachedToken (AttachedT oc TSubscript)

spoiler oc = AttachedToken (AttachedT oc TSpoiler)

verbatim oc = AttachedToken (AttachedT oc TVerbatim)

math oc = AttachedToken (AttachedT oc TMath)

variable oc = AttachedToken (AttachedT oc TVariable)

heading level = DetachedToken (THeading level)

uList level = DetachedToken (TUnorderedList level)

oList level = DetachedToken (TOrderedList level)

tList status level = DetachedToken (TTaskList status level)
