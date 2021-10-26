module Neorg.Document where

import Data.Data (Proxy)
import qualified Data.Map as M
import Data.Text as T (Text, unwords)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Time.Calendar (Day, showGregorian)
import qualified Data.Vector as V
import Debug.Trace

data Document = Document
  { documentBlocks :: Blocks,
    documentMeta :: DocumentMeta
  }

data DocumentMeta = DocumentMeta
  { documentTitle :: Maybe Text,
    documentDescription :: Maybe Text,
    documentAuthor :: Maybe Text,
    documentCategories :: V.Vector Text,
    documentCreated :: Maybe Day,
    documentVersion :: Maybe Text
  }

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

data Block where
  Paragraph :: Inline -> Block
  Heading :: Heading -> Block
  List :: List order -> Block
  Quote :: Quote -> Block

-- Tag :: SomeTag tags -> Block

type Blocks = V.Vector Block

data Heading = HeadingCons
  { headingText :: Inline,
    headingLevel :: IndentationLevel,
    headingContent :: Blocks
  }

data OrderS (order :: Order) where
  OrderedS :: OrderS 'Ordered
  UnorderedS :: OrderS 'Unordered

data Order = Ordered | Unordered

data ListBlock where
  ListParagraph :: Inline -> ListBlock
  SubList :: List order -> ListBlock

data Quote = QuoteCons {quoteLevel :: IndentationLevel, quoteContent :: Blocks}

listBlockToBlock :: ListBlock -> Block
listBlockToBlock (ListParagraph p) = Paragraph p
listBlockToBlock (SubList list) = List list

data List (order :: Order) = ListCons
  { listLevel :: IndentationLevel,
    listOrder :: OrderS order,
    listItems :: V.Vector (V.Vector ListBlock)
  }

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
  deriving (Show)

renderDocument :: Document -> Text
renderDocument = toStrict . toLazyText . renderDocument
  where
    renderDocument :: Document -> Builder
    renderDocument (Document blocks meta) = renderMeta meta <> newline <> V.foldMap (\block -> renderBlock block <> newline) blocks
    renderBlock :: Block -> Builder
    renderBlock = \case
      Paragraph inline -> renderInline inline <> newline
      Heading heading -> renderHeading heading
      List list -> renderList list
      Quote quote -> renderQuote quote
    renderMeta :: DocumentMeta -> Builder
    renderMeta (DocumentMeta title description author categories created version) =
      "@document.meta" <> newline
        <> maybe mempty ((<> newline) . ("  title: " <>) . fromText) title
        <> maybe mempty ((<> newline) . ("  description: " <>) . fromText) description
        <> maybe mempty ((<> newline) . ("  author: " <>) . fromText) author
        <> ( if V.length categories > 0
               then "  categories: " <> (fromText . T.unwords . V.toList) categories <> newline
               else mempty
           )
        <> maybe mempty ((<> newline) . ("  created: " <>) . fromString . showGregorian) created
        <> maybe mempty ((<> newline) . ("  version: " <>) . fromText) version
        <> "@end"
        <> newline
    renderBlocks :: Blocks -> Builder
    renderBlocks = V.foldMap renderBlock
    renderQuote :: Quote -> Builder
    renderQuote (QuoteCons level content) = levelToChars '>' level <> " " <> renderBlocks content
    renderInline :: Inline -> Builder
    renderInline = \case
      Text text -> fromText text
      ConcatInline inlines -> V.foldMap renderInline inlines
      Bold inline -> "*" <> renderInline inline <> "*"
      Italic inline -> "/" <> renderInline inline <> "/"
    renderHeading :: Heading -> Builder
    renderHeading (HeadingCons headingText headingLevel headingContent) =
      levelToChars '*' headingLevel <> "H " <> renderInline headingText <> newline <> renderBlocks headingContent <> "---" <> newline
    renderList :: List order -> Builder
    renderList (ListCons listLevel listOrder listItems) =
      flip V.foldMap listItems $ \blocks ->
        levelToChars '-' listLevel <> "L " <> renderBlocks (listBlockToBlock <$> blocks)
    newline :: Builder
    newline = "\n"
    levelToChars :: Char -> IndentationLevel -> Builder
    levelToChars char level = case level of
      I0 -> singleton char
      I1 -> fromString $ replicate 2 char
      I2 -> fromString $ replicate 3 char
      I3 -> fromString $ replicate 4 char
      I4 -> fromString $ replicate 5 char
      I5 -> fromString $ replicate 6 char

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
class Tautology

instance Tautology
