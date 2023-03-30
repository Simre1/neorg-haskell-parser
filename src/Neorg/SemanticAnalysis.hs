module Neorg.SemanticAnalysis where

import Control.Monad.Trans.State
import Data.Foldable
import Data.Map qualified as M
import Data.Set qualified as S
import GHC.Generics
import Neorg.Document
import Optics.Core

data DocumentInformation = DocumentInformation
  { linkTargets :: LinkTargets,
    norgLinks :: NorgLinks
  }
  deriving (Eq, Show, Generic)

data LinkTargets = LinkTargets
  { lines :: S.Set Int,
    headings :: S.Set Paragraph
  }
  deriving (Eq, Show, Generic)

newtype NorgLinks = NorgLinks
  { norgLinks :: S.Set NorgLocation
  }
  deriving (Eq, Show, Generic)

extractDocumentInformation :: Document -> DocumentInformation
extractDocumentInformation (Document (Blocks blocks)) = execState (traverse_ extractBlock blocks) emptyDocumentInformation
  where
    extractBlock :: Block -> State DocumentInformation ()
    extractBlock (Block lineNumber content) = do
      modify $ #linkTargets % #lines %~ S.insert lineNumber
      case content of
        Heading heading -> do
          modify $ #linkTargets % #headings %~ S.insert (heading ^. #title)
          traverse_ extractBlock $ heading ^. #content % coerced @_ @[Block]
        NestableBlock nestableBlock -> extractNestableBlock nestableBlock
        _ -> pure ()

    extractNestableBlock :: NestableBlock -> State DocumentInformation ()
    extractNestableBlock = \case
      List list -> traverse_ extractNestableBlock $ mconcat $ list ^. #items % mapping (_2 % coerced @_ @[NestableBlock])
      Quote quote -> traverse_ extractNestableBlock $ quote ^. #content % coerced @_ @[NestableBlock]
      Paragraph para -> extractParagraph para
      VerbatimRangedTag _ -> pure ()

    extractParagraph :: Paragraph -> State DocumentInformation ()
    extractParagraph (ParagraphCons paragraphElements) = forM_ paragraphElements $ \case
      StyledParagraph _ para -> extractParagraph para
      Link (CurrentFile norgLocation) _ -> do
        modify $ #norgLinks % coerced %~ S.insert norgLocation
      _ -> pure ()

    emptyDocumentInformation = DocumentInformation (LinkTargets S.empty S.empty) (NorgLinks S.empty)

findNorgLocation :: DocumentInformation -> NorgLocation -> Maybe NorgLocation
findNorgLocation di nl = case nl of
  LineNumberLocation x ->
    if S.size lineNumbers > 0
      then Just $ LineNumberLocation $ minimumBy (\a b -> abs (b - x) `compare` abs (a - x)) lineNumbers
      else Nothing
  HeadingLocation level location -> HeadingLocation level <$> M.lookup (rawParagraph location) rawTextHeadings
  MagicLocation location -> MagicLocation <$> M.lookup (rawParagraph location) rawTextHeadings
  where
    (LinkTargets lineNumbers headings) = linkTargets di
    rawTextHeadings = M.fromList $ (\x -> (rawParagraph x, x)) <$> S.toList headings

linkForLocationExists :: DocumentInformation -> NorgLocation -> Bool
linkForLocationExists di nl = nl `S.member` (di ^. #norgLinks % coerced)
