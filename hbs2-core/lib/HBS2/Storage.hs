{-# Language FunctionalDependencies #-}
module HBS2.Storage where

import HBS2.Hash
import HBS2.Prelude.Plated

import Data.Kind
import Data.Hashable hiding (Hashed)
import Lens.Micro.Platform
import Prettyprinter


class Pretty (Hash h) => IsKey h where
  type family Key h :: Type

instance Key HbSync ~ Hash HbSync => IsKey HbSync where
  type instance Key HbSync = Hash HbSync

newtype StoragePrefix = StoragePrefix  { fromPrefix :: FilePath }
                        deriving stock (Data,Show)
                        deriving newtype (IsString,Pretty)

type family Block block :: Type

newtype Offset = Offset Integer
                 deriving newtype (Eq,Ord,Enum,Num,Real,Integral,Hashable,Pretty)
                 deriving stock (Show)

newtype Size = Size Integer
               deriving newtype (Eq,Ord,Enum,Num,Real,Integral,Hashable,Pretty)
               deriving stock (Show)

class ( Monad m
      , IsKey h
      , Hashed h block
      ) => Storage a h block m | a -> block, a -> h where

  putBlock :: a -> Block block -> m (Maybe (Key h))

  enqueueBlock :: a -> Block block -> m (Maybe (Key h))

  getBlock :: a -> Key h -> m (Maybe (Block block))

  getChunk :: a -> Key h -> Offset -> Size -> m (Maybe (Block block))

  hasBlock :: a -> Key h -> m (Maybe Integer)

  -- listBlocks :: a -> ( Key block -> m () ) -> m ()

  writeLinkRaw :: a -> Key h -> Block block -> m (Maybe (Key h))

  readLinkRaw :: a -> Key h -> m (Maybe (Block block))

calcChunks :: forall a b . (Integral a, Integral b)
           => Integer  -- | block size
           -> Integer  -- | chunk size
           -> [(a, b)]

calcChunks s1 s2 = fmap (over _1 fromIntegral . over _2 fromIntegral)  chu
  where
    chu = fmap (,s2) (takeWhile (<s1) $ iterate (+s2) 0)
