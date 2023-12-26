{-# Language FunctionalDependencies #-}
{-# Language DefaultSignatures #-}
module HBS2.Storage where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Data.Types.Refs (RefMetaData(..))

import Data.Kind
import Lens.Micro.Platform
import Data.ByteString.Lazy (ByteString)
import Control.Monad.Trans.Maybe
import Data.Word

import Codec.Serialise()

class Pretty (Hash h) => IsKey h where
  type family Key h :: Type

instance Key HbSync ~ Hash HbSync => IsKey HbSync where
  type instance Key HbSync = Hash HbSync

newtype StoragePrefix = StoragePrefix  { fromPrefix :: FilePath }
                        deriving stock (Data,Show)
                        deriving newtype (IsString,Pretty)

newtype Offset = Offset Integer
                 deriving newtype (Eq,Ord,Enum,Num,Real,Integral,Hashable,Pretty,Serialise)
                 deriving stock (Show)

newtype Size = Size Integer
               deriving newtype (Eq,Ord,Enum,Num,Real,Integral,Hashable,Pretty,Serialise)
               deriving stock (Show)

data ExpiredAfter a = ExpiredAfter Word64 a
                      deriving stock (Generic)

instance Serialise a => Serialise (ExpiredAfter a)


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

  updateRef :: (Hashed h k, RefMetaData k) => a -> k -> Key h -> m ()

  getRef :: (Hashed h k, Pretty k, RefMetaData k) => a -> k -> m (Maybe (Key h))

  delRef :: (Hashed h k, RefMetaData k) => a -> k -> m ()


data AnyStorage = forall zu  . ( Storage zu HbSync ByteString IO
                               ) => AnyStorage zu

class HasStorage m where
  getStorage :: m AnyStorage

instance (Monad m, HasStorage m) => HasStorage (MaybeT m) where
  getStorage = lift getStorage

instance Hashed h a => Hashed h (ExpiredAfter a) where
  hashObject (ExpiredAfter _ a) = hashObject a

instance RefMetaData a => RefMetaData (ExpiredAfter a) where
  refMetaData (ExpiredAfter t x) = [("expires", show t)] <> refMetaData x

instance (IsKey HbSync, MonadIO m) => Storage AnyStorage HbSync ByteString m  where
  putBlock (AnyStorage s) = liftIO . putBlock s
  enqueueBlock (AnyStorage s) = liftIO . enqueueBlock s
  getBlock (AnyStorage s) = liftIO . getBlock s
  getChunk (AnyStorage s) h a b = liftIO $ getChunk s h a b
  hasBlock (AnyStorage s) = liftIO . hasBlock s
  updateRef (AnyStorage s) r v = liftIO $ updateRef s r v
  getRef (AnyStorage s) = liftIO . getRef s
  delBlock (AnyStorage s) = liftIO . delBlock s
  delRef (AnyStorage s) = liftIO . delRef s





calcChunks :: forall a b . (Integral a, Integral b)
           => Integer  -- | block size
           -> Integer  -- | chunk size
           -> [(a, b)]

calcChunks s1 s2 = fmap (over _1 fromIntegral . over _2 fromIntegral)  chu
  where
    (n,rest) = s1 `divMod` s2
    chu = [ (x*s2,s2) | x <- [0..n-1] ] <> [(n * s2, rest) | rest > 0]



