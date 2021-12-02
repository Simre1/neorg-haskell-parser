{-# LANGUAGE TemplateHaskell #-}

module Neorg.Document where

import Data.Data (Proxy)
import qualified Data.Map as M
import Data.Text (pack)
import Data.Text as T (Text, unwords)
import Data.Time.Calendar (Day, showGregorian)
import qualified Data.Vector as V
import Debug.Trace
import Optics.Core
import Optics.TH
import Data.Void (Void)
import qualified Text.Megaparsec as P
import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits (Symbol, symbolVal, sameSymbol, KnownSymbol)
import Data.Maybe
import Type.Set

data Document (tags :: TypeSet) = Document
  { _documentBlocks :: Blocks tags 
  }
  deriving (Show, Eq)

data DocumentMeta = DocumentMeta
  { _documentTitle :: Maybe Text,
    _documentDescription :: Maybe Text,
    _documentAuthor :: Maybe Text,
    _documentCategories :: V.Vector Text,
    _documentCreated :: Maybe Day,
    _documentVersion :: Maybe Text
  }
  deriving (Show, Eq)

emptyDocumentMeta = DocumentMeta Nothing Nothing Nothing V.empty Nothing Nothing

data IndentationLevel = I0 | I1 | I2 | I3 | I4 | I5 deriving (Eq, Ord, Bounded, Show)

instance Enum IndentationLevel where
  toEnum 0 = I0
  toEnum 1 = I1
  toEnum 2 = I2
  toEnum 3 = I3
  toEnum 4 = I4
  toEnum 5 = I5
  toEnum x = if x < 0 then I0 else I5
  fromEnum I0 = 0
  fromEnum I1 = 1
  fromEnum I2 = 2
  fromEnum I3 = 3
  fromEnum I4 = 4
  fromEnum I5 = 5

indentationLevelToInt :: IndentationLevel -> Int
indentationLevelToInt = fromEnum

data Block (tags :: TypeSet)
  = Paragraph Inline
  | Heading (Heading tags)
  | Quote Quote
  | List List
  | HorizonalLine
  | Marker Marker
  | Tag (SomeTag tags)
  deriving (Show, Eq)

-- Tag :: SomeTag tags -> Block

type Blocks tags = V.Vector (Block tags)

data Heading (tags :: TypeSet) = HeadingCons
  { _headingText :: Inline,
    _headingLevel :: IndentationLevel,
    _headingContent :: Blocks tags
  }
  deriving (Show, Eq)

data ListBlock = ListParagraph Inline | SubList List deriving (Show, Eq)

type ListBlocks = V.Vector ListBlock

data Quote = QuoteCons
  { _quoteLevel :: IndentationLevel,
    _quoteContent :: Inline
  }
  deriving (Show, Eq)

data Marker = MarkerCons {_markerId :: Text, _markerText :: Text} deriving (Show, Eq)

--
-- data Definition = DefinitionCons
--   { _definitionObject :: Text,
--     _definitionContent ::  ???
--   }
--   deriving (Show, Eq)

--
-- listBlockToBlock :: ListBlock -> Block
-- listBlockToBlock (ListParagraph p) = Paragraph p
-- listBlockToBlock (SubList list) = List list

data UnorderedList = UnorderedListCons
  { _uListLevel :: IndentationLevel,
    _uListItems :: V.Vector (V.Vector ListBlock)
  }
  deriving (Show, Eq)

data OrderedList = OrderedListCons
  { _oListLevel :: IndentationLevel,
    _oListItems :: V.Vector (V.Vector ListBlock)
  }
  deriving (Show, Eq)

data TaskStatus = TaskDone | TaskPending | TaskUndone deriving (Show, Eq)

data TaskList = TaskListCons
  { _tListLevel :: IndentationLevel,
    _tListItems :: V.Vector (TaskStatus, V.Vector ListBlock)
  }
  deriving (Show, Eq)

data List = UnorderedList UnorderedList | OrderedList OrderedList | TaskList TaskList deriving (Show, Eq)

data Inline
  = Text Text
  | Bold Inline
  | Italic Inline
  | Underline Inline
  | Strikethrough Inline
  | Superscript Inline
  | Subscript Inline
  | Spoiler Inline
  | Verbatim Text
  | Math Text
  | ConcatInline (V.Vector Inline)
  | Link Text Inline
  | Space
  deriving (Show, Eq)

instance Semigroup Inline where
  i1 <> i2 = ConcatInline $ V.fromList [i1,i2]

instance Monoid Inline where
  mempty = ConcatInline V.empty

canonalizeInline :: Inline -> Inline
canonalizeInline = \case
  ConcatInline inlines ->
    let vector = V.fromList $ processInlinesV inlines
     in if V.length vector == 1
          then V.head vector
          else ConcatInline vector
  a -> a
  where
    processInlinesV = processInlines . V.toList
    processInlines :: [Inline] -> [Inline]
    processInlines = \case
        (Text t1 : x : r) -> case canonalizeInline x of
          (Text t2) -> processInlines $ Text (t1 <> t2) : r
          c -> Text t1 : processInlines (c : r)
        (Text t1 : r) -> Text t1 : processInlines r
        -- (ConcatInline is1 : ConcatInline is2 : r) -> processInlinesV (is1 <> is2) <> processInlines r
        (ConcatInline is1 : r) -> processInlines $ V.toList is1 <> r
        (Bold i : r) -> Bold (canonalizeInline i) : processInlines r
        (Italic i : r) -> Italic (canonalizeInline i) : processInlines r
        (Underline i : r) -> Underline (canonalizeInline i) : processInlines r
        (Strikethrough i : r) -> Strikethrough (canonalizeInline i) : processInlines r
        (Superscript i : r) -> Superscript (canonalizeInline i) : processInlines r
        (Subscript i : r) -> Subscript (canonalizeInline i) : processInlines r
        (Spoiler i : r) -> Spoiler (canonalizeInline i) : processInlines r
        (Math t : r) -> Math t : processInlines r
        (Verbatim t : r) -> Verbatim t : processInlines r
        (Link t i : r) -> Link t (canonalizeInline i) : processInlines r
        (Space : r) -> Space : processInlines r
        [] -> []
      

      -- V.fromList
      --   . V.foldr
      --     ( \i b -> case (i, b) of
      --         (Text t1, x : r) -> case canonalizeInline x of
      --           (Text t2) -> Text (t1 <> t2) : r
      --           c -> Text t1 : c : r
      --         (Text t, r) -> Text t : r

      --     )
      --     []

-- prettyInline :: Inline -> Text
-- prettyInline = \case
--   Text t -> t
--   Bold i -> pack ['*'] <> prettyInline i <> pack ['*']
--   Italic i -> pack ['/'] <> prettyInline i <> pack ['/']
--   Underline i -> pack ['_'] <> prettyInline i <> pack ['_']
--   StrikeThrough i -> pack ['-'] <> prettyInline i <> pack ['-']
--   Superscript i -> pack ['^'] <> prettyInline i <> pack ['^']
--   Subscript i -> pack [','] <> prettyInline i <> pack [',']
--   Spoiler i -> pack ['|'] <> prettyInline i <> pack ['|']
--   ConcatInline i -> V.foldMap prettyInline i
--   Link t i -> undefined
--
-- renderDocument :: Document -> Text
-- renderDocument = toStrict . toLazyText . renderDocument
--   where
--     renderDocument :: Document -> Builder
--     renderDocument (Document blocks meta) = renderMeta meta <> newline <> V.foldMap (\block -> renderBlock block <> newline) blocks
--     renderBlock :: Block -> Builder
--     renderBlock = \case
--       Paragraph inline -> renderInline inline <> newline
--       Heading heading -> renderHeading heading
--       -- List list -> renderList list
--       Quote quote -> renderQuote quote
--     renderMeta :: DocumentMeta -> Builder
--     renderMeta (DocumentMeta title description author categories created version) =
--       "@document.meta" <> newline
--         <> maybe mempty ((<> newline) . ("  title: " <>) . fromText) title
--         <> maybe mempty ((<> newline) . ("  description: " <>) . fromText) description
--         <> maybe mempty ((<> newline) . ("  author: " <>) . fromText) author
--         <> ( if V.length categories > 0
--                then "  categories: " <> (fromText . T.unwords . V.toList) categories <> newline
--                else mempty
--            )
--         <> maybe mempty ((<> newline) . ("  created: " <>) . fromString . showGregorian) created
--         <> maybe mempty ((<> newline) . ("  version: " <>) . fromText) version
--         <> "@end"
--         <> newline
--     renderBlocks :: Blocks -> Builder
--     renderBlocks = V.foldMap renderBlock
--     renderQuote :: Quote -> Builder
--     renderQuote (QuoteCons level content) = levelToChars '>' level <> " " <> renderInline content
--     renderInline :: Inline -> Builder
--     renderInline = \case
--       Text text -> fromText text
--       ConcatInline inlines -> V.foldMap renderInline inlines
--       Bold inline -> "*" <> renderInline inline <> "*"
--       Italic inline -> "/" <> renderInline inline <> "/"
--     renderHeading :: Heading -> Builder
--     renderHeading (HeadingCons headingText headingLevel headingContent) =
--       levelToChars '*' headingLevel <> "H " <> renderInline headingText <> newline <> renderBlocks headingContent <> "---" <> newline
    -- renderList :: List order -> Builder
    -- renderList (ListCons listLevel listOrder listItems) =
    --   flip V.foldMap listItems $ \blocks ->
    --     levelToChars '-' listLevel <> "L " <> renderBlocks (listBlockToBlock <$> blocks)
    -- newline :: Builder
    -- newline = "\n"
    -- levelToChars :: Char -> IndentationLevel -> Builder
    -- levelToChars char level = case level of
    --   I0 -> singleton char
    --   I1 -> fromString $ replicate 2 char
    --   I2 -> fromString $ replicate 3 char
    --   I3 -> fromString $ replicate 4 char
    --   I4 -> fromString $ replicate 5 char
    --   I5 -> fromString $ replicate 6 char
  -- contentParser :: f a -> TagArguments a -> P.Parser (TagContent a)
--

type TagParser = P.Parsec Void Text

class (KnownSymbol a, Eq (TagArguments a), Eq (TagContent a), Show (TagArguments a), Show (TagContent a)) => Tag (a :: Symbol) where
  type TagArguments a
  type TagContent a

class Tag a => ParseTagContent (a :: Symbol) where
  parseTagContent :: f a -> TagArguments a -> TagParser (TagContent a)

data SomeTag (tags :: TypeSet) where
  SomeTag :: Tag t => Proxy t -> TagArguments t -> TagContent t -> SomeTag tags

instance Show (SomeTag tags) where
  show (SomeTag tag args content) = show (symbolVal tag, args, content)

instance Eq (SomeTag tags) where
  (SomeTag p1 args1 content1) == (SomeTag p2 args2 content2) = 
    isJust (p1 `sameSymbol` p2) && args1 == unsafeCoerce args2 && content1 == unsafeCoerce content2

instance Tag "code" where
  type TagArguments "code" = Maybe Text
  type TagContent "code" = Text

instance Tag "math" where
  type TagArguments "math" = ()
  type TagContent "math" = Text

instance Tag "comment" where
  type TagArguments "comment" = ()
  type TagContent "comment" = Text

instance Tag "embed" where
  type TagArguments "embed" = Text
  type TagContent "embed" = Text

instance Tag "document.meta" where
  type TagArguments "document.meta" = ()
  type TagContent "document.meta" = DocumentMeta

instance Tag "table" where
  type TagArguments "table" = ()
  type TagContent "table" = Table

newtype Table = Table (V.Vector TableRow) deriving (Show, Eq)

data TableRow = TableRowDelimiter | TableRowInlines (V.Vector Inline) deriving (Show, Eq)

makeLenses ''Document
makeLenses ''DocumentMeta
makeLenses ''Heading
makeLenses ''Quote
makeLenses ''Marker
makeLenses ''UnorderedList
makeLenses ''OrderedList
makeLenses ''TaskList


