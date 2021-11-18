{-# LANGUAGE UndecidableInstances #-}

module Neorg.Document.Tag where

import Data.Data
import qualified Data.Map as M
import qualified Data.Text as T
import Neorg.Document
import Data.Functor ((<&>))
import GHC.TypeLits (Symbol, symbolVal)

import Type.Set
import Unsafe.Coerce (unsafeCoerce)
import Data.Void (Void)
import qualified Text.Megaparsec as P

--

-- data Ordered
--
-- instance Tag Ordered where
--   tagName _ = "ordered"
--   type TagArguments Ordered = (Int, Int, Int)
--   type TagContent Ordered = List 'Ordered
--   contentParser = undefined
--

data ParseTag
  = ParseTag
      ( forall x.
        (forall a. Tag a => Proxy a -> TagArguments a -> TagContent a -> x) ->
        TagParser x
      )

class GenerateTagParser (tags :: TypeSet) where
  generateTagParser :: Proxy tags -> M.Map T.Text ParseTag

instance GenerateTagParser Empty where
  generateTagParser _ = M.empty

instance (GenerateTagParser r, GenerateTagParser l, Tag a, ParseTagArguments a) => GenerateTagParser (Branch a l r) where
  generateTagParser _ = M.insert tagKey tagParser (generateTagParser (Proxy @l)) `M.union` (generateTagParser (Proxy @r))
    where
      tag :: Proxy a
      tag = Proxy
      tagKey = T.pack (symbolVal tag)
      tagParser = ParseTag $ \f -> do
        arguments <- parseTagArguments tag
        content <- parseTagContent tag arguments
        pure $ f tag arguments content

class ParseTagArguments a where
  parseTagArguments :: f a -> TagParser (TagArguments a)

parseTag :: forall tags. GenerateTagParser tags => T.Text -> Maybe (TagParser (SomeTag tags))
parseTag t = M.lookup t tagMap <&> \case
  ParseTag f -> f SomeTag
  where tagMap = generateTagParser (Proxy @tags)

data Function r = Function (forall a. Proxy a -> TagArguments a -> TagContent a -> r)

newtype TagHandler tags r = TagHandler (M.Map T.Text (Function r))

emptyTagHandler :: TagHandler Empty r
emptyTagHandler = TagHandler M.empty

handleTag :: forall a r. Tag a => (TagArguments a -> TagContent a -> r) -> TagHandler '[a] r
handleTag f = TagHandler $ M.singleton (T.pack $ symbolVal (Proxy @a)) (Function $ \_ -> unsafeCoerce f)

-- Left-biased merge
mergeHandlers :: TagHandler tags1 r -> TagHandler tags2 r -> TagHandler (Merge tags1 tags2) r
mergeHandlers (TagHandler tags1) (TagHandler tags2) = TagHandler $ M.union tags1 tags2

handleSomeTag :: TagHandler tags r -> SomeTag tags -> r
handleSomeTag (TagHandler handlers) (SomeTag p args content) = 
  let (Function f) = handlers M.! T.pack (symbolVal p)
  in f p args content

runTagParser :: TagParser a -> T.Text -> Either (P.ParseErrorBundle T.Text Void) a
runTagParser parser = P.runParser parser "Tag"

--
-- handleTag :: TagHandler tags -> T.Text -> Maybe (P.Parser (SomeTag tags))
-- handleTag (TagHandler tagHandlers) tagName = fmap handle $ M.lookup tagName
--   where
--     handle (Any any) =
--       let (argParser, contentParser) = unsafeCoerce any
--       in argParser *> contentParser
--
-- class MakeTags (tags :: TypeSet) where
--   makeTagHandler :: Proxy tags -> TagHandler tags
--

class Tautology

instance Tautology
