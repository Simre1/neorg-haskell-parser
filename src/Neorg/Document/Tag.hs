{-# LANGUAGE UndecidableInstances #-}

module Neorg.Document.Tag where

import Data.Data (Proxy (..))
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.TypeLits (symbolVal)
import Neorg.Document
import Type.Set (FromList, Merge, TypeSet (..))
import Unsafe.Coerce (unsafeCoerce)

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
