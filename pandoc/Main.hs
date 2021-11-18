module Main where

import Control.Monad.Trans.State
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import Data.Foldable (fold)
import Data.Functor
import qualified Data.Sequence as S
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Debug.Trace
import Neorg.Document
import Neorg.Document.Tag
import Neorg.Parser
import Optics.Core
import System.Environment (getArgs)
import qualified Text.Pandoc.Builder as P
import qualified Text.Pandoc.Definition as P
import Type.Set

main :: IO ()
main = do
  fileName <-
    getArgs <&> \case
      [name] -> name
      _ -> error "Supply one norg file as argument"
  fileContent <- T.readFile fileName
  case parse (pack fileName) fileContent of
    Left err -> T.putStrLn err
    Right doc -> B.putStr $ encode $ convertDocument tagHandler doc

type Convert a = State () a

runConvert :: Convert a -> a
runConvert c = evalState c ()

convertDocument :: forall tags. tags ~ SupportedTags => GenerateTagParser tags => TagHandler tags (Convert P.Blocks) -> Document tags -> P.Pandoc
convertDocument handler (Document blocks _meta) = runConvert $ P.doc . V.foldMap id <$> traverse convertBlock blocks
  where
    convertBlock :: Block tags -> Convert P.Blocks
    convertBlock = \case
      Paragraph i -> convertParagraph i
      Heading heading -> convertHeading heading
      Quote quote -> convertQuote quote
      List list -> convertList list
      HorizonalLine -> convertHorizonalLine
      Marker marker -> convertMarker marker
      Tag tag -> convertTag tag

    convertParagraph :: Inline -> Convert P.Blocks
    convertParagraph = fmap P.para . convertInline

    convertMarker :: Marker -> Convert P.Blocks
    convertMarker = error "not implemented"

    convertList :: List -> Convert P.Blocks
    convertList = \case
      UnorderedList ul ->
        fmap (P.bulletList . V.toList) $
          traverse (applicativeConcatMap convertListBlock) $
            ul ^. uListItems
      OrderedList ol ->
        fmap (P.bulletList . V.toList) $
          traverse (applicativeConcatMap convertListBlock) $
            ol ^. oListItems
      TaskList tl ->
        let convertTaskListBlock taskStatus lb = convertListBlock lb
         in fmap (P.bulletList . V.toList) $
              traverse (\(taskStatus, items) -> applicativeConcatMap (convertTaskListBlock taskStatus) items) $
                tl ^. tListItems
    convertQuote :: Quote -> Convert P.Blocks
    convertQuote quote = P.blockQuote . P.para <$> convertInline (quote ^. quoteContent)

    convertListBlock :: ListBlock -> Convert P.Blocks
    convertListBlock = \case
      ListParagraph i -> P.para <$> convertInline i
      SubList l -> convertList l

    convertHeading :: Heading tags -> Convert P.Blocks
    convertHeading heading = do
      text <- convertInline $ heading ^. headingText
      let header = P.header (succ . fromEnum $ heading ^. headingLevel) text
      subBlocks <- applicativeConcatMap convertBlock (heading ^. headingContent)
      pure $ header <> subBlocks

    convertHorizonalLine :: Convert P.Blocks
    convertHorizonalLine = pure P.horizontalRule

    convertInline :: Inline -> Convert P.Inlines
    convertInline = \case
      Text t -> pure $ P.text t
      Bold inline -> P.strong <$> convertInline inline
      Italic inline -> P.emph <$> convertInline inline
      Underline inline -> P.underline <$> convertInline inline
      Strikethrough inline -> P.strikeout <$> convertInline inline
      Superscript inline -> P.superscript <$> convertInline inline
      Subscript inline -> P.subscript <$> convertInline inline
      Spoiler inline -> P.strikeout <$> convertInline inline -- TODO: No native spoilers in pandoc
      ConcatInline inlines -> V.foldMap id <$> traverse convertInline inlines
      Link url inlines -> P.link url "" <$> convertInline inlines -- TODO: Investigate title field
      Space -> pure P.space
      Verbatim t -> pure $ P.code t
      Math t -> pure $ P.math t
    convertTag :: SomeTag tags -> Convert P.Blocks
    convertTag = handleSomeTag handler

type SupportedTags = FromList '[]

tagHandler :: TagHandler SupportedTags (Convert P.Blocks)
tagHandler = emptyTagHandler

vecToMany :: V.Vector a -> P.Many a
vecToMany = P.Many . S.fromList . V.toList

applicativeConcatMap :: (Applicative f, Monoid m, Traversable t) => (a -> f m) -> t a -> f m
applicativeConcatMap f elems = fold <$> traverse f elems
