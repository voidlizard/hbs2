{-# Language FunctionalDependencies #-}
module HBS2.Storage where

import Data.Kind

type family Block block :: Type
type family Key block   :: Type

class Monad m => Storage a block m | a -> block where

  putBlock :: a -> Block block -> m (Maybe (Key block))
  getBlock :: a -> Key block -> m (Maybe (Block block))
  listBlocks :: a -> ( Key block -> m () ) -> m ()





