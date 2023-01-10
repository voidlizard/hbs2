{-# Language FunctionalDependencies #-}
module HBS2.Storage where

import Data.Kind
import Data.Proxy

import HBS2.Hash

type family Block block :: Type
type family Key block   :: Type

-- class HasHashFunction h a b where
--   hashFun :: Proxy a -> b -> Hash h

class ( Monad m
      , Hashed (StorageHash a block) block
      ) => Storage a block m | a -> block where

  type family StorageHash a block :: Type

  putBlock :: a -> Block block -> m (Maybe (Key block))
  getBlock :: a -> Key block -> m (Maybe (Block block))
  listBlocks :: a -> ( Key block -> m () ) -> m ()





