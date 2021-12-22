module Type.Forall where

import Data.Typeable (Typeable, typeRep)
import Unsafe.Coerce (unsafeCoerce)

data Forall f = forall tags. (Typeable tags, Eq (f tags), Show (f tags)) => Forall (f tags)

deriving instance Show (Forall f)

instance Eq (Forall f) where
  (Forall f1) == (Forall f2) = (typeRep f1 == typeRep f2) && f1 == unsafeCoerce f2
