module Neorg.Document where

import Data.Text
import Data.These

newtype Document = Document Blocks deriving (Show, Eq)

newtype Paragraph = ParagraphCons [ParagraphElement] deriving (Show, Eq)

data ParagraphElement
  = Word Text
  | Punctuation Char
  | Space
  | StyledParagraph ParagraphStyle Paragraph
  | VerbatimParagraph VerbatimType Text
  | Link LinkLocation (Maybe Paragraph)
  deriving (Show, Eq)

data ParagraphStyle = Bold | Italic | Underline | StrikeThrough | Superscript | Subscript | Spoiler deriving (Show, Eq, Enum, Bounded)

data VerbatimType = Code | Math deriving (Show, Eq, Enum, Bounded)

data LinkLocation
  = Url Text
  | NorgFile Text (Maybe NorgLocation)
  | CurrentFile NorgLocation
  deriving (Show, Eq)

data NorgLocation
  = HeadingLocation Int Text
  | LineNumberLocation Int
  | MagicLocation Text
  deriving (Show, Eq)

newtype Blocks = Blocks [Block] deriving (Show, Eq)

data Block
  = PureBlock PureBlock
  | Heading Heading
  | HorizontalRule
  deriving (Show, Eq)

data Heading = HeadingCons
  { level :: Int,
    taskStatus :: Maybe TaskStatus,
    title :: Paragraph,
    content :: Blocks
  }
  deriving (Show, Eq)

newtype PureBlocks = PureBlocks [PureBlock] deriving (Show, Eq)

data PureBlock
  = List List
  | Quote Quote
  | Paragraph Paragraph
  | VerbatimRangedTag VerbatimRangedTag
  deriving (Show, Eq)

data Quote = QuoteCons
  { level :: Int,
    status :: Maybe TaskStatus,
    content :: PureBlocks
  }
  deriving (Show, Eq)

data List = ListCons
  { level :: Int,
    ordering :: ListOrdering,
    items :: [(Maybe TaskStatus, PureBlocks)]
  }
  deriving (Show, Eq)

data ListOrdering = OrderedList | UnorderedList deriving (Show, Eq)

data VerbatimRangedTag = VerbatimRangedTagCons
  { tag :: Text,
    parameters :: [Text],
    content :: Text
  } deriving (Show, Eq)

data TaskStatus = Undone | Done | Unclear | Urgent | Recurring | InProgress | OnHold | Cancelled deriving (Show, Eq, Enum, Bounded)

taskStatusChar :: TaskStatus -> Char
taskStatusChar = \case
  Undone -> ' '
  Done -> 'x'
  Unclear -> '?'
  Urgent -> '!'
  Recurring -> '+'
  InProgress -> '-'
  OnHold -> '='
  Cancelled -> '_'

charToTaskStatus :: Char -> Maybe TaskStatus
charToTaskStatus = \case
  ' ' -> Just Undone
  'x' -> Just Done
  '?' -> Just Unclear
  '!' -> Just Urgent
  '+' -> Just Recurring
  '-' -> Just InProgress
  '=' -> Just OnHold
  '_' -> Just Cancelled
  _ -> Nothing
