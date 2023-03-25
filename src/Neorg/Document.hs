module Neorg.Document where

import Data.Maybe (fromMaybe)
import Data.Text
import GHC.Generics (Generic)

newtype Document = Document Blocks deriving (Show, Eq, Generic)

newtype Paragraph = ParagraphCons [ParagraphElement] deriving (Show, Eq, Generic, Ord)

data ParagraphElement
  = Word Text
  | Punctuation Char
  | Space
  | StyledParagraph ParagraphStyle Paragraph
  | VerbatimParagraph VerbatimType Text
  | Link LinkLocation (Maybe Paragraph)
  deriving (Show, Eq, Generic, Ord)

data ParagraphStyle = Bold | Italic | Underline | StrikeThrough | Superscript | Subscript | Spoiler deriving (Show, Eq, Generic, Enum, Bounded, Ord)

data VerbatimType = Code | Math deriving (Show, Eq, Generic, Enum, Bounded, Ord)

data LinkLocation
  = Url Text
  | NorgFile Text (Maybe NorgLocation)
  | CurrentFile NorgLocation
  | ExternalFile Text (Maybe Int)
  deriving (Show, Eq, Generic, Ord)

data NorgLocation
  = HeadingLocation Int Paragraph
  | LineNumberLocation Int
  | MagicLocation Paragraph
  deriving (Show, Eq, Generic, Ord)

newtype Blocks = Blocks [Block] deriving (Show, Eq, Generic)

data Block = Block
  { lineNumber :: Int,
    content :: BlockContent
  }
  deriving (Show, Eq, Generic)

data BlockContent
  = NestableBlock NestableBlock
  | Heading Heading
  | HorizontalRule
  deriving (Show, Eq, Generic)

data Heading = HeadingCons
  { level :: Int,
    taskStatus :: Maybe TaskStatus,
    title :: Paragraph,
    content :: Blocks
  }
  deriving (Show, Eq, Generic)

newtype NestableBlocks = NestableBlocks [NestableBlock] deriving (Show, Eq, Generic)

data NestableBlock
  = List List
  | Quote Quote
  | Paragraph Paragraph
  | VerbatimRangedTag VerbatimRangedTag
  deriving (Show, Eq, Generic)

data Quote = QuoteCons
  { level :: Int,
    status :: Maybe TaskStatus,
    content :: NestableBlocks
  }
  deriving (Show, Eq, Generic)

data List = ListCons
  { level :: Int,
    ordering :: ListOrdering,
    items :: [(Maybe TaskStatus, NestableBlocks)]
  }
  deriving (Show, Eq, Generic)

data ListOrdering = OrderedList | UnorderedList deriving (Show, Eq, Generic)

data VerbatimRangedTag = VerbatimRangedTagCons
  { tag :: VerbatimRangedTagType,
    content :: Text
  }
  deriving (Show, Eq, Generic)

newtype VerbatimRangedTagType = VerbatimRangedTagCode
  { language :: Maybe Text
  }
  deriving (Show, Eq, Generic)

data TaskStatus = Undone | Done | Unclear | Urgent | Recurring | InProgress | OnHold | Cancelled deriving (Show, Eq, Generic, Enum, Bounded)

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

norgLocationDescription :: NorgLocation -> Paragraph
norgLocationDescription (HeadingLocation _ t) = t
norgLocationDescription (LineNumberLocation i) = ParagraphCons [Word $ pack (show i)]
norgLocationDescription (MagicLocation t) = t

rawParagraph :: Paragraph -> Text
rawParagraph (ParagraphCons paraElements) = flip foldMap paraElements $ \case
  Word t -> t
  Punctuation char -> pack [char]
  Space -> " "
  StyledParagraph _ para -> rawParagraph para
  VerbatimParagraph _ verbatimText -> verbatimText
  Link linkLocation maybeDescription -> flip fromMaybe (rawParagraph <$> maybeDescription) $ case linkLocation of
    CurrentFile norgLocation -> rawParagraph $ norgLocationDescription norgLocation
    Url path -> path
    NorgFile path norgLocation -> path <> maybe "" (rawParagraph . norgLocationDescription) norgLocation
    ExternalFile path lineNumber -> path <> "-" <> pack (show lineNumber)
