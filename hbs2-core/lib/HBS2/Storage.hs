{-# Language FunctionalDependencies #-}
module HBS2.Storage where

import Data.Kind
import Data.Hashable hiding (Hashed)

import HBS2.Hash
import HBS2.Prelude.Plated

newtype StoragePrefix = StoragePrefix  { fromPrefix :: FilePath }
                        deriving stock (Data,Show)
                        deriving newtype (IsString)

type family Block block :: Type
type family Key block   :: Type

newtype Offset = Offset Integer
                 deriving newtype (Eq,Ord,Enum,Num,Real,Integral,Hashable)
                 deriving stock (Show)

newtype Size = Size Integer
               deriving newtype (Eq,Ord,Enum,Num,Real,Integral,Hashable)
               deriving stock (Show)


class ( Monad m
      , Hashed (StorageHash a block) block
      ) => Storage a block m | a -> block where

  type family StorageHash a block :: Type

  putBlock :: a -> Block block -> m (Maybe (Key block))

  getBlock :: a -> Key block -> m (Maybe (Block block))

  getChunk :: a -> Key block -> Offset -> Size -> m (Maybe (Block block))

  hasBlock :: a -> Key block -> m Bool

  listBlocks :: a -> ( Key block -> m () ) -> m ()



