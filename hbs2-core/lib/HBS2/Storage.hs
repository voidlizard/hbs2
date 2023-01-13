{-# Language FunctionalDependencies #-}
module HBS2.Storage where

import Data.Kind
import Data.Hashable hiding (Hashed)
import Prettyprinter

import HBS2.Hash
import HBS2.Prelude.Plated

class Pretty (Hash h) => IsKey h where
  type Key h :: Type

instance Key HbSync ~ Hash HbSync => IsKey HbSync where
  type instance Key HbSync = Hash HbSync

newtype StoragePrefix = StoragePrefix  { fromPrefix :: FilePath }
                        deriving stock (Data,Show)
                        deriving newtype (IsString)

type family Block block :: Type

newtype Offset = Offset Integer
                 deriving newtype (Eq,Ord,Enum,Num,Real,Integral,Hashable)
                 deriving stock (Show)

newtype Size = Size Integer
               deriving newtype (Eq,Ord,Enum,Num,Real,Integral,Hashable)
               deriving stock (Show)

class ( Monad m
      , IsKey h
      , Hashed h block
      ) => Storage a h block m | a -> block, a -> h where

  putBlock :: a -> Block block -> m (Maybe (Key h))

  enqueueBlock :: a -> Block block -> m (Maybe (Key h))

  getBlock :: a -> Key h -> m (Maybe (Block block))

  getChunk :: a -> Key h -> Offset -> Size -> m (Maybe (Block block))

  hasBlock :: a -> Key h -> m Bool

  -- listBlocks :: a -> ( Key block -> m () ) -> m ()



