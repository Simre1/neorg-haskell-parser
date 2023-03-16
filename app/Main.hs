module Main where

import Control.Monad
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
import Options.Applicative
import System.Environment (getArgs)
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
convertDocument (Document blocks) = P.doc <$> convertBlocks blocks

convertBlock :: Block -> Convert P.Blocks
convertBlock = \case
  Heading heading -> convertHeading heading
  PureBlock pb -> convertPureBlock pb
  HorizontalRule -> pure P.horizontalRule

convertBlocks :: Blocks -> Convert P.Blocks
convertBlocks (Blocks blocks) = mconcat <$> traverse convertBlock blocks

convertPureBlock :: PureBlock -> Convert P.Blocks
convertPureBlock = \case
  Paragraph i -> P.para <$> convertParagraph i
  Quote quote -> convertQuote quote
  List list -> convertList list

convertPureBlocks :: PureBlocks -> Convert P.Blocks
convertPureBlocks (PureBlocks blocks) = mconcat <$> traverse convertPureBlock blocks

convertList :: List -> Convert P.Blocks
convertList (ListCons level ordering items) = do
  makeList <$> traverse makeItem items
  where
    makeItem (maybeTaskStatus, item) = do
      taskStatus <- traverse convertTaskStatus maybeTaskStatus
      blocks <- convertPureBlocks item
      pure $ maybe mempty P.plain taskStatus <> blocks
    makeList = case ordering of
      OrderedList -> P.orderedList
      UnorderedList -> P.bulletList

convertHeading :: Heading -> Convert P.Blocks
convertHeading (HeadingCons level status title content) = do
  titleText <- convertParagraph title
  taskStatus <- traverse convertTaskStatus status
  let ref = T.intercalate "-" $ T.words $ T.toLower $ paragraphToText title
  content <- convertBlocks content
  pure $ P.headerWith (ref, [], []) level (maybe titleText (<> titleText) taskStatus) <> content

convertQuote :: Quote -> Convert P.Blocks
convertQuote (QuoteCons level status content) = do
  taskStatus <- traverse convertTaskStatus status
  blocks <- convertPureBlocks content
  pure $ P.blockQuote $ maybe mempty P.plain taskStatus <> blocks

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
      NorgFile path norgLocation -> do
        warning "Norg files cannot be properly linked"
        pure $ P.link path "" $ description (maybe (P.text path) ((P.text path <>) . P.text . norgLocationDescription) norgLocation)
      CurrentFile norgLocation -> do
        pure $ P.link (norgLocationLink norgLocation) "" $ description $ P.text $ norgLocationDescription norgLocation
  where
    norgLocationLink (HeadingLocation _ t) = T.pack "#" <> T.intercalate "-" (T.words $ T.toLower t)
    norgLocationLink _ = ""

norgLocationDescription (HeadingLocation _ t) = t
norgLocationDescription (LineNumberLocation i) = T.pack (show i)
norgLocationDescription (MagicLocation t) = t

paragraphToText :: Paragraph -> Text
paragraphToText (ParagraphCons paraElements) = flip foldMap paraElements $ \case
  Word t -> t
  Punctuation char -> T.pack [char]
  Space -> " "
  StyledParagraph _ para -> paragraphToText para
  VerbatimParagraph _ verbatimText -> verbatimText
  Link linkLocation maybeDescription -> flip fromMaybe (paragraphToText <$> maybeDescription) $ case linkLocation of
    CurrentFile norgLocation -> norgLocationDescription norgLocation
    Url path -> path
    NorgFile path norgLocation -> path <> maybe "" norgLocationDescription norgLocation
