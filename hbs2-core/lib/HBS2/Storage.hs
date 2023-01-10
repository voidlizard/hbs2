{-# Language FunctionalDependencies #-}
module HBS2.Storage where

import Data.Kind

import HBS2.Hash

type family Block block :: Type
type family Key block   :: Type

newtype Offset = Offset Integer
                 deriving newtype (Eq,Ord,Enum,Num,Real,Integral)

newtype Size = Size Integer
               deriving newtype (Eq,Ord,Enum,Num,Real,Integral)


class ( Monad m
      , Hashed (StorageHash a block) block
      ) => Storage a block m | a -> block where

  type family StorageHash a block :: Type

  putBlock :: a -> Block block -> m (Maybe (Key block))
  getBlock :: a -> Key block -> m (Maybe (Block block))

  getChunk :: a -> Key block -> Offset -> Size -> m (Maybe (Block block))

  listBlocks :: a -> ( Key block -> m () ) -> m ()



