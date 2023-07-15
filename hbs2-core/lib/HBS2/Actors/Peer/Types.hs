module HBS2.Actors.Peer.Types where

import HBS2.Storage
import HBS2.Hash

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString)


instance (IsKey HbSync) => Storage AnyStorage HbSync ByteString IO where
  putBlock (AnyStorage s) = putBlock s
  enqueueBlock (AnyStorage s) = enqueueBlock s
  getBlock (AnyStorage s) = getBlock s
  getChunk (AnyStorage s) = getChunk s
  hasBlock (AnyStorage s) = hasBlock s
  updateRef (AnyStorage s) = updateRef s
  getRef (AnyStorage s) = getRef s
  delBlock (AnyStorage s) = delBlock s
  delRef (AnyStorage s) = delRef s

data AnyStorage = forall zu . ( Storage zu HbSync ByteString IO
                              ) => AnyStorage zu

class HasStorage m where
  getStorage :: m AnyStorage


instance (Monad m, HasStorage m) => HasStorage (MaybeT m) where
  getStorage = lift getStorage


