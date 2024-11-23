module HBS2.Tests.MapStore where

import Control.Monad.State as State
import Data.ByteString.Lazy (ByteString)
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as Map

import HBS2.Hash

type MapStore = Map (Hash HbSync) ByteString
type MapStoreM a = MapStoreT Identity a
newtype MapStoreT m a = MapStoreT {unMapStoreT :: StateT MapStore m a}
    deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadState MapStore)

runMapStoreM :: MapStoreM a -> a
runMapStoreM = runIdentity . runMapStoreT

runMapStoreT :: (Monad m) => MapStoreT m a -> m a
runMapStoreT = flip evalStateT mempty . unMapStoreT

mapStorePutBlock :: (Monad m) => ByteString -> MapStoreT m (Hash HbSync)
mapStorePutBlock bs =
    h <$ State.modify (Map.insert h bs)
  where
    h = hashObject bs

mapStoreReadBlock :: (Monad m) => (Hash HbSync) -> MapStoreT m (Maybe ByteString)
mapStoreReadBlock h =
    State.gets (Map.lookup h)

mapStoreDeleteBlock :: (Monad m) => (Hash HbSync) -> MapStoreT m ()
mapStoreDeleteBlock h =
    State.modify (Map.delete h)
