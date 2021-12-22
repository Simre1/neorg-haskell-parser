{-# LANGUAGE UndecidableInstances #-}

module Neorg.Document.Tag where

import Control.Applicative (optional)
import Data.Data (Proxy (..))
import Data.Functor ((<&>))
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Void (Void)
import GHC.TypeLits (symbolVal)
import Neorg.Document
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Type.Set (FromList, Merge, TypeSet (..))
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad (void)

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

newtype Function r = Function (forall a. Proxy a -> TagArguments a -> TagContent a -> r)

newtype TagHandler tags r = TagHandler (M.Map T.Text (Function r))

emptyTagHandler :: TagHandler 'Empty r
emptyTagHandler = TagHandler M.empty

handleTag :: forall a r. Tag a => (TagArguments a -> TagContent a -> r) -> TagHandler (FromList '[a]) r
handleTag f = TagHandler $ M.singleton (T.pack $ symbolVal (Proxy @a)) (Function $ \_ -> unsafeCoerce f)

-- Left-biased merge
mergeHandler :: TagHandler tags1 r -> TagHandler tags2 r -> TagHandler (Merge tags1 tags2) r
mergeHandler (TagHandler tags1) (TagHandler tags2) = TagHandler $ M.union tags1 tags2

handleSomeTag :: TagHandler tags r -> SomeTag tags -> r
handleSomeTag (TagHandler handlers) (SomeTag p args content) =
  let (Function f) = handlers M.! T.pack (symbolVal p)
   in f p args content

runTagParser :: TagParser a -> T.Text -> Either (P.ParseErrorBundle T.Text Void) a
runTagParser parser = P.runParser parser "Tag"

class ParseTagArguments a where
  parseTagArguments :: f a -> TagParser a

instance ParseTagArguments T.Text where
  parseTagArguments _ = P.takeWhile1P (Just "Text tag argument") (> ' ')

instance ParseTagArguments a => ParseTagArguments (Maybe a) where
  parseTagArguments _ = optional (parseTagArguments (Proxy @a))

instance ParseTagArguments () where
  parseTagArguments _ = pure ()

