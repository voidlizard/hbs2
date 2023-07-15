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

  putBlock :: a -> block -> m (Maybe (Key h))

  enqueueBlock :: a -> block -> m (Maybe (Key h))

  getBlock :: a -> Key h -> m (Maybe block)

  delBlock :: a -> Key h -> m ()

  getChunk :: a -> Key h -> Offset -> Size -> m (Maybe block)

  hasBlock :: a -> Key h -> m (Maybe Integer)

  updateRef :: Hashed h k => a -> k -> Key h -> m ()

  getRef :: Hashed h k => a -> k -> m (Maybe (Key h))

  delRef :: Hashed h k => a -> k -> m ()

calcChunks :: forall a b . (Integral a, Integral b)
           => Integer  -- | block size
           -> Integer  -- | chunk size
           -> [(a, b)]

calcChunks s1 s2 = fmap (over _1 fromIntegral . over _2 fromIntegral)  chu
  where
    (n,rest) = s1 `divMod` s2
    chu = [ (x*s2,s2) | x <- [0..n-1] ] <> [(n * s2, rest) | rest > 0]



