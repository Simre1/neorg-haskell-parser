{-# LANGUAGE UndecidableInstances #-}
module Neorg.Sum where

import Data.Typeable (Typeable, TypeRep, typeRep, typeOf, Proxy (Proxy))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Map.Merge.Strict as M

data Sum f (as :: [*]) where
  Sum :: (Member a as, Typeable a) => f a -> Sum f as

type family Member a (as :: [*]) where
  Member a (a:_) = Tautology
  Member a (b:as) = Member a as

class Tautology

instance Tautology

toSum :: (Member a as, Typeable a) => f a -> Sum f as
toSum = Sum

data HandleSum f (as :: [*]) r where
  HandleSum :: M.Map TypeRep Any -> HandleSum f as r

data Any = forall a. Any (forall. a)

handle :: forall f a b. Typeable a => (f a -> b) -> HandleSum f '[a] b
handle f = HandleSum $ M.singleton (typeRep $ Proxy @a) (Any f)

mergeHandlers :: HandleSum f as r -> HandleSum f bs r -> HandleSum f (Merge as bs) r
mergeHandlers (HandleSum handlers1) (HandleSum handlers2) = HandleSum (M.union handlers1 handlers2)

(<:>) :: HandleSum f as r -> HandleSum f bs r -> HandleSum f (Merge as bs) r
(<:>) = mergeHandlers

type family Append a as where
  Append a (a:as) = a:as 
  Append b (a:as) = b:Append b as
  Append b '[] = '[b]

type family Merge (as :: [*]) (bs :: [*]) where
  Merge as '[] = as
  Merge as (b:bs) = Merge (Append b as) bs
