module Main where

import Control.Monad.Trans.State
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as B
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Neorg.Document
import Neorg.Parser.Base
import Neorg.Parser.Document (document)
import System.Environment (getArgs)
import System.IO (stderr)
import Text.Pandoc.Builder qualified as P

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [fileName] -> Just . (,fileName) <$> T.readFile fileName
    [] -> Just . (,"STDIN") <$> T.getContents
    _ -> pure Nothing

  case input of
    Just (content, name) -> do
      let parsedDocument = parseText name document content
      case parsedDocument of
        Left err -> logError err
        Right doc -> runConvert (convertDocument doc) >>= B.putStr . encode
    Nothing -> logError $ T.pack "Supply zero arguments or exactly one norg file as an argument"

logError :: Text -> IO ()
logError = T.hPutStr stderr

newtype Convert a = Convert (IO a) deriving (Functor, Applicative, Monad)

warning :: Text -> Convert ()
warning t = Convert $ logError t

runConvert :: Convert a -> IO a
runConvert (Convert io) = io

convertDocument :: Document -> Convert P.Pandoc
convertDocument (Document (Blocks blocks)) = P.doc . mconcat <$> traverse convertBlock blocks

convertBlock :: Block -> Convert P.Blocks
convertBlock = \case
  Heading heading -> convertHeading heading
  PureBlock pb -> convertPureBlock pb
  HorizontalRule -> undefined

convertPureBlock :: PureBlock -> Convert P.Blocks
convertPureBlock = \case
  Paragraph i -> P.para <$> convertParagraph i
  Quote quote -> convertQuote quote
  List list -> convertList list

convertList :: List -> Convert P.Blocks
convertList (ListCons level ordering items) = undefined

convertHeading :: Heading -> Convert P.Blocks
convertHeading = undefined

convertQuote :: Quote -> Convert P.Blocks
convertQuote = undefined

--
--  ->
--   fmap (P.bulletList) $
--     traverse (applicativeConcatMap $ convertPureBlock handler) $
--       ul ^. uListItems
-- OrderedList ol ->
--   fmap (P.orderedList) $
--     traverse (applicativeConcatMap $ convertPureBlock handler) $
--       ol ^. oListItems
-- TaskList tl ->
--   let convertTaskListBlock taskStatus lb = convertPureBlock handler lb
--    in fmap (P.bulletList . V.toList) $
--         traverse (\(taskStatus, items) -> applicativeConcatMap (convertTaskListBlock taskStatus) items) $
--           tl ^. tListItems
--
-- convertQuote :: Quote -> Convert P.Blocks
-- convertQuote quote = P.blockQuote . P.para <$> convertInline (quote ^. quoteContent)
--
-- convertHeading :: TagHandler tags (Convert P.Blocks) -> Heading -> Convert P.Blocks
-- convertHeading handler heading = do
--   text <- convertInline $ heading ^. headingText
--   let ref = T.intercalate "-" $ T.words $ T.toLower $ inlineToText $ heading ^. headingText
--   pure $ P.headerWith (ref, [], []) (succ . fromEnum $ heading ^. headingLevel) text
--
-- convertDefinition :: TagHandler tags (Convert P.Blocks) -> Definition tags -> Convert P.Blocks
-- convertDefinition handler definition = do
--   definitionText <- convertInline $ definition ^. definitionObject
--   definitionBlocks <- traverse (convertPureBlock handler) $ definition ^. definitionContent
--   pure $ P.definitionList [(definitionText, V.toList definitionBlocks)]
--
-- convertDelimiter :: Delimiter -> Convert P.Blocks
-- convertDelimiter delimiter = case delimiter of
--   HorizonalLine -> pure P.horizontalRule
--   _ -> pure mempty
--
--

convertParagraph :: Paragraph -> Convert P.Inlines
convertParagraph (ParagraphCons elems) = mconcat <$> traverse convertParagraphElement elems

convertParagraphElement :: ParagraphElement -> Convert P.Inlines
convertParagraphElement = \case
  Word t -> pure $ P.text t
  Punctuation char -> pure $ P.str $ T.pack [char]
  Space -> pure P.space
  StyledParagraph style para ->
    let pandocStyle = case style of
          Bold -> P.strong
          Italic -> P.emph
          Underline -> P.underline
          StrikeThrough -> P.strikeout
          Superscript -> P.superscript
          Subscript -> P.subscript
          Spoiler -> P.strikeout
     in pandocStyle <$> convertParagraph para
  VerbatimParagraph vType verbatimText ->
    let pandocVerbatimType = case vType of
          Code -> P.code
          Math -> P.math
     in pure $ pandocVerbatimType verbatimText
  Link location maybeDescription -> do
    maybeDescriptionPandoc <- traverse convertParagraph maybeDescription
    let description def = fromMaybe def maybeDescriptionPandoc
    case location of
      Url path -> pure $ P.link path "" $ description (P.text path)
      ExternalFile path -> do
        warning "External files cannot be properly linked"
        pure $ P.link path "" $ description (P.text path)
      NorgFile path norgLocation -> do
        warning "Norg files cannot be properly linked"
        pure $ P.link path "" $ description (maybe (P.text path) ((P.text path <>) . norgLocationDescription) norgLocation)
      CurrentFile norgLocation -> do
        pure $ P.link (norgLocationLink norgLocation) "" $ description $ norgLocationDescription norgLocation
  where
    norgLocationLink (HeadingLocation _ t) = T.pack "#" <> T.intercalate "-" (T.words $ T.toLower t)
    norgLocationLink _ = ""
    norgLocationDescription (HeadingLocation _ t) = P.text t
    norgLocationDescription (LineNumberLocation i) = P.text $ T.pack (show i)
    norgLocationDescription (MagicLocation t) = P.text t
