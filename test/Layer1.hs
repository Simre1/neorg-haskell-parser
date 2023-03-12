module Layer1 where

import Data.Text
import Data.These
import Neorg.Document
import Neorg.Parser.Paragraph (paragraph)
import Neorg.Parser.Type (parseTextAnySource)
import Test.HUnit
import Test.Hspec

parseParagraph :: Text -> IO Paragraph
parseParagraph text = case parseTextAnySource paragraph text of
  Left error -> assertFailure (unpack error)
  Right a -> pure a

layer1 :: Spec
layer1 = describe "Layer 1" $ do
  describe "Paragraph" paragraphSpec

paragraphSpec :: Spec
paragraphSpec = do
  it "Some words" $ do
    let input = "Some simple words."
        expectation = Paragraph [Word "Some", Space, Word "simple", Space, Word "words", Punctuation '.']
    result <- parseParagraph input
    expectation @=? result

  it "Sentences" $ do
    let input = "Sentence 1. Sentence 2.\nSentence 3."
        expectation = Paragraph [Word "Sentence", Space, Word "1", Punctuation '.', Space, Word "Sentence", Space, Word "2", Punctuation '.', Space, Word "Sentence", Space, Word "3", Punctuation '.']
    result <- parseParagraph input
    expectation @=? result

  it "Bold words" $ do
    let input = "A *bold* word"
        expectation = Paragraph [Word "A", Space, StyledParagraph Bold (Paragraph [Word "bold"]), Space, Word "word"]
    result <- parseParagraph input
    expectation @=? result

  it "Bold text" $ do
    let input = "*Bold text*"
        expectation = Paragraph [StyledParagraph Bold $ Paragraph [Word "Bold", Space, Word "text"]]
    result <- parseParagraph input
    expectation @=? result

  it "Bold text with comma" $ do
    let input = "*Bold text*,"
        expectation = Paragraph [StyledParagraph Bold $ Paragraph [Word "Bold", Space, Word "text"], Punctuation ',']
    result <- parseParagraph input
    expectation @=? result

  it "Bold text with . in front" $ do
    let input = ".*Bold text*"
        expectation = Paragraph [Punctuation '.', StyledParagraph Bold $ Paragraph [Word "Bold", Space, Word "text"]]
    result <- parseParagraph input
    expectation @=? result

  it "Bold text with newline between" $ do
    let input = "*Bold\ntext*"
        expectation = Paragraph [StyledParagraph Bold $ Paragraph [Word "Bold", Space, Word "text"]]
    result <- parseParagraph input
    expectation @=? result

  it "Bold and italic text" $ do
    let input = "*/Bold and italic/*"
        expectation =
          Paragraph
            [ StyledParagraph Bold $
                Paragraph
                  [StyledParagraph Italic $ Paragraph [Word "Bold", Space, Word "and", Space, Word "italic"]]
            ]
    result <- parseParagraph input
    expectation @=? result

  it "Bold and partly italic text" $ do
    let input = "*/Bold and italic/ and only bold*"
        expectation =
          Paragraph
            [ StyledParagraph Bold $
                Paragraph
                  [ StyledParagraph Italic $
                      Paragraph
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
          Paragraph
            [ Word "Text",
              Space,
              StyledParagraph Bold $
                Paragraph
                  [ StyledParagraph Italic (Paragraph [Word "with"]),
                    Space,
                    StyledParagraph Underline (Paragraph [Word "different"]),
                    Space,
                    StyledParagraph Superscript (Paragraph [Word "markup"]),
                    Space,
                    StyledParagraph Spoiler (Paragraph [Word "types"])
                  ]
            ]
    result <- parseParagraph input
    expectation @=? result

  it "Bold with invalid modifiers" $ do
    let input = "* Bold text *"
        expectation = Paragraph [Punctuation '*', Space, Word "Bold", Space, Word "text", Space, Punctuation '*']
    result <- parseParagraph input
    expectation @=? result

  it "Bold with invalid end modifier" $ do
    let input = "\\*Bold text *"
        expectation = Paragraph [Punctuation '*', Word "Bold", Space, Word "text", Space, Punctuation '*']
    result <- parseParagraph input
    expectation @=? result

  it "Bold with invalid start modifier" $ do
    let input = "other text*Bold text*"
        expectation =
          Paragraph
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
          Paragraph [Word "Bold", Space, Word "text", Punctuation '*']
    result <- parseParagraph input
    expectation @=? result

  it "Bold without end modifier" $ do
    let input = "*Bold text"
        expectation =
          Paragraph [Punctuation '*', Word "Bold", Space, Word "text"]
    result <- parseParagraph input
    expectation @=? result

  it "Bold over paragraph bounds" $ do
    let input = "*Bold\n\ntext*"
        expectation = Paragraph [Punctuation '*', Word "Bold"]
    result <- parseParagraph input
    expectation @=? result

  it "Closed in wrong order" $ do
    let input = "*/Bold and italic*/"
        expectation = Paragraph [Punctuation '*', StyledParagraph Italic $ Paragraph [Word "Bold", Space, Word "and", Space, Word "italic", Punctuation '*']]
    result <- parseParagraph input
    expectation @=? result

  it "Inline code" $ do
    let input = "Some `inline code`"
        expectation = Paragraph [Word "Some", Space, VerbatimParagraph Code "inline code"]
    result <- parseParagraph input
    expectation @=? result

  it "Inline math" $ do
    let input = "Some $inline math$"
        expectation = Paragraph [Word "Some", Space, VerbatimParagraph Math "inline math"]
    result <- parseParagraph input
    expectation @=? result

  it "Inline math with no ending modifier" $ do
    let input = "Some $inline math"
        expectation = Paragraph [Word "Some", Space, Punctuation '$', Word "inline", Space, Word "math"]
    result <- parseParagraph input
    expectation @=? result

  it "Zero length inline code" $ do
    let input = "Zero ``"
        expectation = Paragraph [Word "Zero", Space,  Punctuation '`', Punctuation '`']
    result <- parseParagraph input
    expectation @=? result

  it "Zero length bold" $ do
    let input = "Zero **"
        expectation = Paragraph [Word "Zero", Space,  Punctuation '*', Punctuation '*']
    result <- parseParagraph input
    expectation @=? result

  it "Escape character 1" $ do
    let input = "\\*works*"
        expectation = Paragraph [Punctuation '*', Word "works", Punctuation '*']
    result <- parseParagraph input
    expectation @=? result

  it "Escape character 2" $ do
    let input = "\\\\"
        expectation = Paragraph [Punctuation '\\']
    result <- parseParagraph input
    expectation @=? result
  
  it "Link with only a location" $ do
    let input = "{https://github.com/nvim-neorg/neorg}"
        expectation = Paragraph [Link $ This (Url "https://github.com/nvim-neorg/neorg")]
    result <- parseParagraph input
    expectation @=? result
  
  it "Link with only a description" $ do
    let input = "[A *description*]"
        expectation = Paragraph [Link $ That (Paragraph $ [Word "A", Space, StyledParagraph Bold $ Paragraph [Word "description"]])]
    result <- parseParagraph input
    expectation @=? result


  it "Link with both a location and a description" $ do
    let input = "{https://github.com/nvim-neorg/neorg}[Neorg]"
        expectation = Paragraph [Link $ These (Url "https://github.com/nvim-neorg/neorg") (Paragraph $ [Word "Neorg"])]
    result <- parseParagraph input
    expectation @=? result

  it "Link location separated by paragraph break" $ do
    let input = "{link\n\n}"
        expectation = Paragraph [Punctuation '{', Word "link"]
    result <- parseParagraph input
    expectation @=? result


  it "Link description separated by paragraph break" $ do
    let input = "[link\n\n]"
        expectation = Paragraph [Punctuation '[', Word "link"]
    result <- parseParagraph input
    expectation @=? result


  it "Link location on line with no content" $ do
    let input = "{link\n}"
        expectation = Paragraph [Punctuation '{', Word "link", Space, Punctuation '}']
    result <- parseParagraph input
    expectation @=? result

  it "Link description on line with no content" $ do
    let input = "[link\n]"
        expectation = Paragraph [Punctuation '[', Word "link", Space, Punctuation ']']
    result <- parseParagraph input
    expectation @=? result

  it "Link location with direct newline" $ do
    let input = "{\nlink}"
        expectation = Paragraph [Punctuation '{', Space, Word "link", Punctuation '}']
    result <- parseParagraph input
    expectation @=? result

  it "Link description with direct newline" $ do
    let input = "[\nlink]"
        expectation = Paragraph [Punctuation '[', Space, Word "link", Punctuation ']']
    result <- parseParagraph input
    expectation @=? result
