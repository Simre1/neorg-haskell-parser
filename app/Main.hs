module Main where

import Control.Monad
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as B
import Data.Maybe
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Neorg.Document
import Neorg.Parser (parseDocument)
import Neorg.SemanticAnalysis
import Options.Applicative
import System.IO (stderr)
import Text.Pandoc.Builder qualified as P

data InputArgs = TransformFile String | TransformSTDIN

main :: IO ()
main = do
  input <- execParser $ info args infoMod
  parseAndTransform input
  where
    infoMod = fullDesc
    args =
      TransformFile
        <$> strOption
          ( long "file"
              <> short 'f'
              <> action "file"
              <> help "Specify a file to read in and transform it to a pandoc json"
          )
        <|> flag'
          TransformSTDIN
          ( long "stdin"
              <> short 'i'
              <> help "Read a norg file in on stdin and transform it to a pandoc json"
          )

parseAndTransform :: InputArgs -> IO ()
parseAndTransform inputArgs = do
  input <- case inputArgs of
    TransformFile fileName -> do
      guard $ fileName /= "--help" || fileName /= "-h"
      Just . (,fileName) <$> T.readFile fileName
    TransformSTDIN -> Just . (,"STDIN") <$> T.getContents

  case input of
    Just (content, name) -> do
      let parsedDocument = parseDocument name content
      case parsedDocument of
        Left err -> logError err
        Right doc -> runConvert (extractDocumentInformation doc) (convertDocument doc) >>= B.putStr . encode
    Nothing -> logError $ T.pack "Supply zero arguments or exactly one norg file as an argument"

convertDocument :: Document -> Convert P.Pandoc
convertDocument (Document blocks) = P.doc <$> convertBlocks blocks

convertBlock :: Block -> Convert P.Blocks
convertBlock (Block _ blockContent) = case blockContent of
  Heading heading -> convertHeading heading
  NestableBlock pb -> convertNestableBlock pb
  HorizontalRule -> pure P.horizontalRule

convertBlocks :: Blocks -> Convert P.Blocks
convertBlocks (Blocks blocks) = mconcat <$> traverse convertBlock blocks

convertNestableBlock :: NestableBlock -> Convert P.Blocks
convertNestableBlock = \case
  Paragraph i -> P.para <$> convertParagraph i
  Quote quote -> convertQuote quote
  List list -> convertList list
  VerbatimRangedTag verbatimRangedTag -> convertVerbatimRangedTag verbatimRangedTag

convertNestableBlocks :: NestableBlocks -> Convert P.Blocks
convertNestableBlocks (NestableBlocks blocks) = mconcat <$> traverse convertNestableBlock blocks

convertList :: List -> Convert P.Blocks
convertList (ListCons _ ordering items) = do
  makeList <$> traverse makeItem items
  where
    makeItem (maybeTaskStatus, item) = do
      taskStatus <- traverse convertTaskStatus maybeTaskStatus
      blocks <- convertNestableBlocks item
      pure $ maybe mempty P.plain taskStatus <> blocks
    makeList = case ordering of
      OrderedList -> P.orderedList
      UnorderedList -> P.bulletList

convertHeading :: Heading -> Convert P.Blocks
convertHeading (HeadingCons level status title content) = do
  titleText <- convertParagraph title
  taskStatus <- traverse convertTaskStatus status
  ref <- norgLocationLink (HeadingLocation level title)
  pandocContent <- convertBlocks content
  pure $ P.headerWith (ref, [], []) level (maybe titleText (<> titleText) taskStatus) <> pandocContent

convertQuote :: Quote -> Convert P.Blocks
convertQuote (QuoteCons _ status content) = do
  taskStatus <- traverse convertTaskStatus status
  blocks <- convertNestableBlocks content
  pure $ P.blockQuote $ maybe mempty P.plain taskStatus <> blocks

convertVerbatimRangedTag :: VerbatimRangedTag -> Convert P.Blocks
convertVerbatimRangedTag (VerbatimRangedTagCons tagType content) = case tagType of
  VerbatimRangedTagCode language -> pure $ P.codeBlockWith ("", maybeToList language, []) content

convertTaskStatus :: TaskStatus -> Convert P.Inlines
convertTaskStatus status = pure $ P.text $ case status of
  Undone -> "u "
  Done -> "d "
  Unclear -> "u "
  Urgent -> "u "
  Recurring -> "r "
  InProgress -> "i "
  OnHold -> "o "
  Cancelled -> "c "

convertParagraph :: Paragraph -> Convert P.Inlines
convertParagraph (ParagraphCons elems) = mconcat <$> traverse convertParagraphElement elems

convertParagraphElement :: ParagraphElement -> Convert P.Inlines
convertParagraphElement = \case
  Word t -> pure $ P.text t
  Punctuation char -> pure $ P.str $ T.pack [char]
  Space -> pure P.space
  StyledParagraph paraStyle para ->
    let pandocStyle = case paraStyle of
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
      NorgFile path norgLocation -> do
        warning "Norg files cannot be properly linked"
        maybeNorgLocation <- traverse (convertParagraph . norgLocationDescription) norgLocation
        pure $ P.link path "" $ description $ maybe (P.text path) (P.text path <>) maybeNorgLocation
      ExternalFile path lineNumber -> do
        when (isJust lineNumber) $ warning "Line numbers in external files cannot be properly linked"
        pure $ P.link path "" $ description $ P.text path
      CurrentFile norgLocation -> do
        loc <- norgLocationLink norgLocation
        defaultDescription <- convertParagraph $ norgLocationDescription norgLocation
        pure $ P.link loc "" $ description defaultDescription

norgLocationLink :: NorgLocation -> Convert Text
norgLocationLink norgLocation = do
  foundNorgLocation <- flip findNorgLocation norgLocation <$> documentInfo
  case foundNorgLocation of
    Nothing -> do
      warning $ "Could not link to the location: " <> pack (show norgLocation)
      pure ""
    Just location -> case location of
      HeadingLocation _ paragraph -> pure $ linkLocationFromParagraph paragraph
      MagicLocation paragraph -> pure $ linkLocationFromParagraph paragraph
      LineNumberLocation ln -> pure $ linkLocationFromLineNumber ln
  where
    linkLocationFromParagraph p = T.pack "#" <> T.intercalate "-" (T.words $ T.toLower $ rawParagraph p)
    linkLocationFromLineNumber ln = pack "#" <> "line-number-" <> pack (show ln)

logError :: Text -> IO ()
logError = T.hPutStrLn stderr

newtype Convert a = Convert (ReaderT DocumentInformation IO a) deriving (Functor, Applicative, Monad)

warning :: Text -> Convert ()
warning t = Convert $ lift $ logError t

documentInfo :: Convert DocumentInformation
documentInfo = Convert ask

runConvert :: DocumentInformation -> Convert a -> IO a
runConvert di (Convert io) = runReaderT io di
