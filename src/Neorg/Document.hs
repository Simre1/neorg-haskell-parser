module Neorg.Document where

import Data.Text
import Data.These

data Document

newtype Paragraph = Paragraph [ParagraphElement] deriving (Show, Eq)

data ParagraphElement
  = Word Text
  | Punctuation Char
  | Space
  | StyledParagraph ParagraphStyle Paragraph
  | VerbatimParagraph VerbatimType Text
  | Link (These LinkLocation Paragraph)
  deriving (Show, Eq)

data ParagraphStyle = Bold | Italic | Underline | StrikeThrough | Superscript | Subscript | Spoiler deriving (Show, Eq, Enum, Bounded)

data VerbatimType = Code | Math deriving (Show, Eq, Enum, Bounded)

newtype LinkLocation = Url Text deriving (Show, Eq)
