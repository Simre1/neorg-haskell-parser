module Paragraph where

import Data.Text
import Neorg.Document
import Neorg.Parser.Paragraph (paragraph, paragraphSegment)
import Neorg.Parser.Base (parseTextAnySource)
import Test.HUnit
import Test.Hspec

parseParagraph :: Text -> IO Paragraph
parseParagraph text = case parseTextAnySource paragraph text of
  Left error -> assertFailure (unpack error)
  Right a -> pure a

parseParagraphShouldFail :: Text -> IO ()
parseParagraphShouldFail text = case parseTextAnySource paragraph text of
  Left error -> pure ()
  Right a -> assertFailure ("Should have failed, but returned:\n" ++ show a)

parseParagraphSegment :: Text -> IO Paragraph
parseParagraphSegment text = case parseTextAnySource paragraphSegment text of
  Left error -> assertFailure (unpack error)
  Right a -> pure a

paragraphSpec :: Spec
paragraphSpec = describe "Paragraph" $ do
  it "Some words" $ do
    let input = "Some simple words."
        expectation = ParagraphCons [Word "Some", Space, Word "simple", Space, Word "words", Punctuation '.']
    result <- parseParagraph input
    expectation @=? result

  it "Sentences" $ do
    let input = "Sentence 1. Sentence 2.\nSentence 3."
        expectation = ParagraphCons [Word "Sentence", Space, Word "1", Punctuation '.', Space, Word "Sentence", Space, Word "2", Punctuation '.', Space, Word "Sentence", Space, Word "3", Punctuation '.']
    result <- parseParagraph input
    expectation @=? result

  it "Bold words" $ do
    let input = "A *bold* word"
        expectation = ParagraphCons [Word "A", Space, StyledParagraph Bold (ParagraphCons [Word "bold"]), Space, Word "word"]
    result <- parseParagraph input
    expectation @=? result

  it "Bold text" $ do
    let input = "*Bold text*"
        expectation = ParagraphCons [StyledParagraph Bold $ ParagraphCons [Word "Bold", Space, Word "text"]]
    result <- parseParagraph input
    expectation @=? result

  it "Bold text with comma" $ do
    let input = "*Bold text*,"
        expectation = ParagraphCons [StyledParagraph Bold $ ParagraphCons [Word "Bold", Space, Word "text"], Punctuation ',']
    result <- parseParagraph input
    expectation @=? result

  it "Bold text with . in front" $ do
    let input = ".*Bold text*"
        expectation = ParagraphCons [Punctuation '.', StyledParagraph Bold $ ParagraphCons [Word "Bold", Space, Word "text"]]
    result <- parseParagraph input
    expectation @=? result

  it "Bold text with newline between" $ do
    let input = "*Bold\ntext*"
        expectation = ParagraphCons [StyledParagraph Bold $ ParagraphCons [Word "Bold", Space, Word "text"]]
    result <- parseParagraph input
    expectation @=? result

  it "Bold and italic text" $ do
    let input = "*/Bold and italic/*"
        expectation =
          ParagraphCons
            [ StyledParagraph Bold $
                ParagraphCons
                  [StyledParagraph Italic $ ParagraphCons [Word "Bold", Space, Word "and", Space, Word "italic"]]
            ]
    result <- parseParagraph input
    expectation @=? result

  it "Bold and partly italic text" $ do
    let input = "*/Bold and italic/ and only bold*"
        expectation =
          ParagraphCons
            [ StyledParagraph Bold $
                ParagraphCons
                  [ StyledParagraph Italic $
                      ParagraphCons
                        [Word "Bold", Space, Word "and", Space, Word "italic"],
                    Space,
                    Word "and",
                    Space,
                    Word "only",
                    Space,
                    Word "bold"
                  ]
            ]
    result <- parseParagraph input
    expectation @=? result

  it "Text with different markup types" $ do
    let input = "Text */with/ _different_ ^markup^ !types!*"
        expectation =
          ParagraphCons
            [ Word "Text",
              Space,
              StyledParagraph Bold $
                ParagraphCons
                  [ StyledParagraph Italic (ParagraphCons [Word "with"]),
                    Space,
                    StyledParagraph Underline (ParagraphCons [Word "different"]),
                    Space,
                    StyledParagraph Superscript (ParagraphCons [Word "markup"]),
                    Space,
                    StyledParagraph Spoiler (ParagraphCons [Word "types"])
                  ]
            ]
    result <- parseParagraph input
    expectation @=? result

  it "Bold with invalid modifiers" $ do
    let input = "* Bold text *"
    parseParagraphShouldFail input

  it "Bold with invalid end modifier" $ do
    let input = "\\*Bold text *"
        expectation = ParagraphCons [Punctuation '*', Word "Bold", Space, Word "text", Space, Punctuation '*']
    result <- parseParagraph input
    expectation @=? result

  it "Bold with invalid start modifier" $ do
    let input = "other text*Bold text*"
        expectation =
          ParagraphCons
            [ Word "other",
              Space,
              Word "text",
              Punctuation '*',
              Word "Bold",
              Space,
              Word "text",
              Punctuation '*'
            ]
    result <- parseParagraph input
    expectation @=? result

  it "Bold with no start modifier" $ do
    let input = "Bold text*"
        expectation =
          ParagraphCons [Word "Bold", Space, Word "text", Punctuation '*']
    result <- parseParagraph input
    expectation @=? result

  it "Bold without end modifier" $ do
    let input = "*Bold text"
        expectation =
          ParagraphCons [Punctuation '*', Word "Bold", Space, Word "text"]
    result <- parseParagraph input
    expectation @=? result

  it "Bold over paragraph bounds" $ do
    let input = "*Bold\n\ntext*"
        expectation = ParagraphCons [Punctuation '*', Word "Bold"]
    result <- parseParagraph input
    expectation @=? result

  it "Closed in wrong order" $ do
    let input = "*/Bold and italic*/"
        expectation = ParagraphCons [Punctuation '*', StyledParagraph Italic $ ParagraphCons [Word "Bold", Space, Word "and", Space, Word "italic", Punctuation '*']]
    result <- parseParagraph input
    expectation @=? result

  it "Inline code" $ do
    let input = "Some `inline code`"
        expectation = ParagraphCons [Word "Some", Space, VerbatimParagraph Code "inline code"]
    result <- parseParagraph input
    expectation @=? result

  it "Inline math" $ do
    let input = "Some $inline math$"
        expectation = ParagraphCons [Word "Some", Space, VerbatimParagraph Math "inline math"]
    result <- parseParagraph input
    expectation @=? result

  it "Inline math with no ending modifier" $ do
    let input = "Some $inline math"
        expectation = ParagraphCons [Word "Some", Space, Punctuation '$', Word "inline", Space, Word "math"]
    result <- parseParagraph input
    expectation @=? result

  it "Zero length inline code" $ do
    let input = "Zero ``"
        expectation = ParagraphCons [Word "Zero", Space, Punctuation '`', Punctuation '`']
    result <- parseParagraph input
    expectation @=? result

  it "Inline code with two newlines" $ do
    let input = "`a\n\na`"
        expectation = ParagraphCons [Punctuation '`', Word "a"]
    result <- parseParagraph input
    expectation @=? result

  it "Inline code with heading next" $ do
    let input = "`a\n* Heading`"
        expectation = ParagraphCons [Punctuation '`', Word "a"]
    result <- parseParagraph input
    expectation @=? result

  it "Multi-line inline code" $ do
    let input = "`sd\ncode`"
        expectation = ParagraphCons [VerbatimParagraph Code "sdcode"]
    result <- parseParagraph input
    expectation @=? result

  it "Zero length bold" $ do
    let input = "Zero **"
        expectation = ParagraphCons [Word "Zero", Space, Punctuation '*', Punctuation '*']
    result <- parseParagraph input
    expectation @=? result

  it "Escape character 1" $ do
    let input = "\\*works*"
        expectation = ParagraphCons [Punctuation '*', Word "works", Punctuation '*']
    result <- parseParagraph input
    expectation @=? result

  it "Escape character 2" $ do
    let input = "\\\\"
        expectation = ParagraphCons [Punctuation '\\']
    result <- parseParagraph input
    expectation @=? result

  it "Link with only a location" $ do
    let input = "{https://github.com/nvim-neorg/neorg}"
        expectation = ParagraphCons [Link (Url "https://github.com/nvim-neorg/neorg") Nothing]
    result <- parseParagraph input
    expectation @=? result

  it "Link with both a location and a description" $ do
    let input = "{https://github.com/nvim-neorg/neorg}[Neorg]"
        expectation = ParagraphCons [Link (Url "https://github.com/nvim-neorg/neorg") (Just $ ParagraphCons [Word "Neorg"])]
    result <- parseParagraph input
    expectation @=? result

  it "Link location separated by paragraph break" $ do
    let input = "{link\n\n}"
        expectation = ParagraphCons [Punctuation '{', Word "link"]
    result <- parseParagraph input
    expectation @=? result

  it "Link description separated by paragraph break" $ do
    let input = "[link\n\n]"
        expectation = ParagraphCons [Punctuation '[', Word "link"]
    result <- parseParagraph input
    expectation @=? result

  it "Link location on line with no content" $ do
    let input = "{link\n}"
        expectation = ParagraphCons [Punctuation '{', Word "link", Space, Punctuation '}']
    result <- parseParagraph input
    expectation @=? result

  it "Link description on line with no content" $ do
    let input = "[link\n]"
        expectation = ParagraphCons [Punctuation '[', Word "link", Space, Punctuation ']']
    result <- parseParagraph input
    expectation @=? result

  it "Link location with direct newline" $ do
    let input = "{\nlink}"
        expectation = ParagraphCons [Punctuation '{', Space, Word "link", Punctuation '}']
    result <- parseParagraph input
    expectation @=? result

  it "Link description with direct newline" $ do
    let input = "[\nlink]"
        expectation = ParagraphCons [Punctuation '[', Space, Word "link", Punctuation ']']
    result <- parseParagraph input
    expectation @=? result

  it "Link with a norg file location" $ do
    let input = "{:path:}"
        expectation = ParagraphCons [Link (NorgFile "path" Nothing) Nothing]
    result <- parseParagraph input
    expectation @=? result
  
  it "Link with a norg file location and line number" $ do
    let input = "{:path:123}"
        expectation = ParagraphCons [Link (NorgFile "path" (Just $ LineNumberLocation 123)) Nothing]
    result <- parseParagraph input
    expectation @=? result

  it "Link with a norg file location a heading" $ do
    let input = "{:path:* heading}"
        expectation = ParagraphCons [Link (NorgFile "path" (Just $ HeadingLocation 1 "heading")) Nothing]
    result <- parseParagraph input
    expectation @=? result

  it "Link with a norg file location a magic location" $ do
    let input = "{:path:# magic}"
        expectation = ParagraphCons [Link (NorgFile "path" (Just $ MagicLocation "magic")) Nothing]
    result <- parseParagraph input
    expectation @=? result

  it "Link with a magic location" $ do
    let input = "{# magic}"
        expectation = ParagraphCons [Link (CurrentFile (MagicLocation "magic")) Nothing]
    result <- parseParagraph input
    expectation @=? result

  it "Link with line number" $ do
    let input = "{123}"
        expectation = ParagraphCons [Link (CurrentFile (LineNumberLocation 123)) Nothing]
    result <- parseParagraph input
    expectation @=? result

  it "Link with line number" $ do
    let input = "{*** heading}"
        expectation = ParagraphCons [Link (CurrentFile (HeadingLocation 3 "heading")) Nothing]
    result <- parseParagraph input
    expectation @=? result

  it "Paragraph segment" $ do
    let input = "Segment1\nSegment2"
        expectation = ParagraphCons [Word "Segment1"]
    result <- parseParagraphSegment input
    expectation @=? result

  it "Paragraph with no trailing spaces" $ do
    let input = "Para1 \n\n  Para2"
        expectation = ParagraphCons [Word "Para1"]
    result <- parseParagraphSegment input
    expectation @=? result
