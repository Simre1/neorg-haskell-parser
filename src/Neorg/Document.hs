{-# LANGUAGE TemplateHaskell #-}

module Neorg.Document where

import Data.Data (Proxy)
import qualified Data.Map as M
import Data.Text (pack)
import Data.Text as T (Text, unwords)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Time.Calendar (Day, showGregorian)
import qualified Data.Vector as V
import Debug.Trace
import Optics.Core
import Optics.TH

data Document = Document
  { _documentBlocks :: Blocks,
    _documentMeta :: DocumentMeta
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

data Block
  = Paragraph Inline
  | Heading Heading
  | Quote Quote
  | List List
  | HorizonalLine
  | Marker Marker
  deriving (Show, Eq)

-- Tag :: SomeTag tags -> Block

type Blocks = V.Vector Block

data Heading = HeadingCons
  { _headingText :: Inline,
    _headingLevel :: IndentationLevel,
    _headingContent :: Blocks
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
  | StrikeThrough Inline
  | Superscript Inline
  | Subscript Inline
  | Spoiler Inline
  | ConcatInline (V.Vector Inline)
  | Link Text Inline
  | Space
  deriving (Show, Eq)

canonalizeInline :: Inline -> Inline
canonalizeInline = \case
  ConcatInline inlines ->
    let vector = processInlines inlines
     in if V.length vector == 1
          then V.head vector
          else ConcatInline vector
  a -> a
  where
    toVector :: Inline -> V.Vector Inline
    toVector = \case
      ConcatInline v -> processInlines v
      i -> V.singleton i
    processInlines :: V.Vector Inline -> V.Vector Inline
    processInlines =
      V.fromList
        . V.foldr
          ( \i b -> case (i, b) of
              (Text t1, (Text t2) : r) -> Text (t1 <> t2) : r
              (Text t, r) -> Text t : r
              (ConcatInline is, r) -> V.toList (processInlines is) <> r
              (Bold i, r) -> Bold (canonalizeInline i) : r
              (Italic i, r) -> Italic (canonalizeInline i) : r
              (Underline i, r) -> Underline (canonalizeInline i) : r
              (StrikeThrough i, r) -> StrikeThrough (canonalizeInline i) : r
              (Superscript i, r) -> Superscript (canonalizeInline i) : r
              (Subscript i, r) -> Subscript (canonalizeInline i) : r
              (Spoiler i, r) -> Spoiler (canonalizeInline i) : r
              (Link t i, r) -> Link t (canonalizeInline i) : r
              (Space, r) -> Space : r
          )
          []

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

-- class Tag a where
--   tagName :: f a -> T.Text
--   type TagArguments a
--   type TagContent a
--   -- contentParser :: f a -> TagArguments a -> P.Parser (TagContent a)
--
-- data Ordered
--
-- instance Tag Ordered where
--   tagName _ = "ordered"
--   type TagArguments Ordered = (Int, Int, Int)
--   type TagContent Ordered = List 'Ordered
--   -- contentParser = undefined
--
-- data SomeTag (tags :: [*]) where
--   SomeTag :: Exists t tags => Proxy t -> TagArguments t -> TagContent t -> SomeTag tags
--
-- type family Exists a (as :: [*]) where
--   Exists a (a : _) = Tautology
--   Exists a (b : as) = Exists a as
-- --
-- -- class ParseTagArguments a where
-- --   generateParser :: f a -> P.Parser a
-- --
-- data Any = forall a. Any a
--
-- newtype TagHandler tags = TagHandler (M.Map T.Text Any)
--
-- handleTag :: TagHandler tags -> T.Text -> Maybe (P.Parser (SomeTag tags))
-- handleTag (TagHandler tagHandlers) tagName = fmap handle $ M.lookup tagName
--   where
--     handle (Any any) =
--       let (argParser, contentParser) = unsafeCoerce any
--       in argParser *> contentParser
--
-- class MakeTags (tags :: [*]) where
--   makeTagHandler :: Proxy tags -> TagHandler tags
--

makeLenses ''Document
makeLenses ''DocumentMeta
makeLenses ''Heading
makeLenses ''Quote
makeLenses ''Marker
makeLenses ''UnorderedList
makeLenses ''OrderedList
makeLenses ''TaskList

class Tautology

instance Tautology
