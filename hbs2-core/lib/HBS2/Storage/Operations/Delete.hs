module HBS2.Storage.Operations.Delete where

import HBS2.Prelude.Plated
import HBS2.Data.Detect
import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Merkle
import HBS2.Storage

import HBS2.System.Logger.Simple

import Streaming.Prelude qualified as S
import Streaming.Prelude (Stream, Of(..))
import Control.Monad.Trans.Maybe
import Control.Monad
import Data.Coerce
import Data.Maybe


deleteMerkleTree :: MonadIO m => AnyStorage  -> HashRef -> m ()
deleteMerkleTree sto root =  do
    what <- S.toList_ $ deepScan ScanDeep (const none) (coerce root) (getBlock sto) $ \ha -> do
      S.yield ha

    for_ (reverse what) $ \ha -> do
      delBlock sto ha

