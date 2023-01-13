module HBS2.Net.Proto.Actors.BlockInfo where

import HBS2.Prelude
import HBS2.Hash

-- needs: logger
-- needs: reader and shit
-- needs: messaging
newtype BlockInfoActor  = BlockInfoActor ()


-- TODO: send block info request
-- TODO: receive block info request
-- TODO: cache block info per peer
-- TODO: get block info per peer


createBlockInfoActor :: MonadIO m => m BlockInfoActor
createBlockInfoActor = do
  pure $ BlockInfoActor ()

sendBlockInfoRequest :: MonadIO m => BlockInfoActor -> Hash h -> m  ()
sendBlockInfoRequest b h = do
  undefined


