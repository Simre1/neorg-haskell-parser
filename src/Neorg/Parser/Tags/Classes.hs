{-# LANGUAGE UndecidableInstances #-}
module Neorg.Parser.Tags.Classes where

import Data.Data (Proxy (Proxy))
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Void
import GHC.TypeLits (Symbol, symbolVal)
import Neorg.Document
import Neorg.Parser.Types
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Type.Set (TypeSet (Empty, Branch))
import Data.Functor
import Cleff

type TagParser = Parser '[]

class Tag a => ParseTagContent (a :: Symbol) where
  parseTagContent :: f a -> TagArguments a -> TagParser (TagContent a)

newtype ParseTag
  = ParseTag
      ( forall x.
        (forall a. Tag a => Proxy a -> TagArguments a -> TagContent a -> x) ->
        TagParser x
      )

class GenerateTagParser (tags :: TypeSet) where
  generateTagParser :: Proxy tags -> M.Map T.Text ParseTag

instance GenerateTagParser 'Empty where
  generateTagParser _ = M.empty

instance (GenerateTagParser r, GenerateTagParser l, ParseTagContent a, Tag a, ParseTagArguments (TagArguments a)) => GenerateTagParser ('Branch a l r) where
  generateTagParser _ = M.insert tagKey tagParser (generateTagParser (Proxy @l)) `M.union` generateTagParser (Proxy @r)
    where
      tag :: Proxy a
      tag = Proxy
      tagKey = T.pack (symbolVal tag)
      tagParser = ParseTag $ \f -> do
        arguments <- parseTagArguments (Proxy @(TagArguments a))
        P.hspace
        void P.newline
        content <- parseTagContent tag arguments
        pure $ f tag arguments content

parseTag :: forall tags. GenerateTagParser tags => T.Text -> Maybe (TagParser (SomeTag tags))
parseTag t =
  M.lookup t tagMap <&> \case
    ParseTag f -> f SomeTag
  where
    tagMap = generateTagParser (Proxy @tags)

runTagParser :: TagParser a -> T.Text -> Either (P.ParseErrorBundle T.Text Void) a
runTagParser parser = runPure . P.runParserT parser "Tag"

class ParseTagArguments a where
  parseTagArguments :: f a -> TagParser a

instance ParseTagArguments T.Text where
  parseTagArguments _ = P.takeWhile1P (Just "Text tag argument") (> ' ')

instance ParseTagArguments a => ParseTagArguments (Maybe a) where
  parseTagArguments _ = P.optional (parseTagArguments (Proxy @a))

instance ParseTagArguments () where
  parseTagArguments _ = pure ()
