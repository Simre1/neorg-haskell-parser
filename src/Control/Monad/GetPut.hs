module Control.Monad.GetPut where

import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.State (StateT, get, put)

class Get a m where
  envGet :: m a

class Put a m where
  envPut :: a -> m ()

type Modify a m = (Get a m, Put a m)


envModify :: forall a m. (Monad m, Modify a m) => (a -> a) -> m ()
envModify f = envGet >>= envPut . f

instance {-# OVERLAPPING #-} Monad m => Get a (ReaderT a m) where
  envGet = ask

instance {-# OVERLAPPING #-} Monad m => Get a (StateT a m) where
  envGet = get

instance (MonadTrans t, Monad m, Get a m) => Get a (t m) where
  envGet = lift envGet

instance {-# OVERLAPPING #-} Monad m => Put a (StateT a m) where
  envPut = put

instance (MonadTrans t, Monad m, Put a m) => Put a (t m) where
  envPut = lift . envPut
