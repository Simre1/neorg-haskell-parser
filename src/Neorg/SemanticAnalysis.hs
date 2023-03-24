module Neorg.SemanticAnalysis where

import Data.Foldable
import Data.Map qualified as M
import Data.Set qualified as S
import Neorg.Document
import GHC.Generics
import Optics.Core
import Control.Monad.Trans.State

newtype DocumentInformation = DocumentInformation
  { linkReferences :: LinkReferences
  } deriving (Eq, Show, Generic)

data LinkReferences = LinkReferences
  { lines :: S.Set Int,
    headings :: S.Set Paragraph
  } deriving (Eq, Show, Generic)

extractDocumentInformation :: Document -> DocumentInformation
extractDocumentInformation (Document (Blocks blocks)) = execState (traverse_ extractBlock blocks) emptyDocumentInformation
  where 
    extractBlock :: Block -> State DocumentInformation ()
    extractBlock (Block lineNumber content) = do
      modify $ #linkReferences % #lines %~ S.insert lineNumber 
      case content of
        Heading heading -> do
          modify $ #linkReferences % #headings %~ S.insert (heading ^. #title)
          traverse_ extractBlock $ heading ^. #content % coerced @_ @[Block]
        _ -> pure ()
    emptyDocumentInformation = DocumentInformation $ LinkReferences S.empty S.empty


findNorgLocation :: DocumentInformation -> NorgLocation -> Maybe NorgLocation
findNorgLocation di nl = case nl of
  LineNumberLocation x ->
    if S.size lineNumbers > 0
      then Just $ LineNumberLocation $ minimumBy (\a b -> abs (b - x) `compare` abs (a - x)) lineNumbers
      else Nothing
  HeadingLocation level location -> HeadingLocation level <$> M.lookup (rawParagraph location) rawTextHeadings
  MagicLocation location -> MagicLocation <$> M.lookup (rawParagraph location) rawTextHeadings
  where
    (LinkReferences lineNumbers headings ) = linkReferences di
    rawTextHeadings = M.fromList $ (\x -> (rawParagraph x, x)) <$> S.toList headings
