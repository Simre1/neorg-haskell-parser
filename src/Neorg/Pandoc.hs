module Neorg.Pandoc where

import qualified Data.Vector as V
import qualified Text.Pandoc.Definition as P
import qualified Data.Text.IO as T
import Neorg.Document
import Data.Default
import Neorg.Parser
import Data.Either
import qualified Data.Aeson as A
import Data.ByteString.Lazy


documentToPandoc :: Document tags -> P.Pandoc
documentToPandoc (Document blocks) = P.Pandoc mempty $
  toPandocBlock <$> V.toList blocks
  where 
    toPandocBlock :: Block tags -> P.Block
    toPandocBlock = \case
          Heading heading -> 
            let header = P.Header (indentationLevelToInt $ headingLevel heading) P.nullAttr (toPandocInline $ headingText heading)
                subBlocks = toPandocBlock <$> V.toList (headingContent heading)
            in P.Div P.nullAttr (header:subBlocks)
          Paragraph inline -> P.Para (toPandocInline inline)
          List list -> 
            let items = V.toList $ fmap (toPandocBlock . listBlockToBlock) . V.toList <$> listItems list
                makeList = case listOrder list of
                  OrderedS -> P.OrderedList (0, P.DefaultStyle, P.DefaultDelim)
                  UnorderedS -> P.BulletList 
            in makeList items
    toPandocInline :: Inline -> [P.Inline]
    toPandocInline = \case
      Text t -> [P.Str t]
      ConcatInline inlines -> V.toList inlines >>= toPandocInline

documentToPandocJson :: Document tags -> ByteString
documentToPandocJson = A.encode . documentToPandoc
