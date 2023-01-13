module HBS2.Net.Proto.Actors.BlockInfo where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Net.Proto
import HBS2.Clock

import Data.Function

-- needs: logger
-- needs: reader and shit
-- needs: messaging
-- needs: cookie manager
-- needs: peer manager

data BlockInfoActor =
  BlockInfoActor
  {
  }


-- TODO: send block info request
-- TODO: receive block info request
-- TODO: cache block info per peer
-- TODO: get block info per peer


createBlockInfoActor :: MonadIO m => m BlockInfoActor
createBlockInfoActor = do
  pure $ BlockInfoActor

runBlockInfoActor :: MonadIO m => BlockInfoActor -> m ()
runBlockInfoActor _ =
  fix \next -> do
    pause (1 :: Timeout 'Seconds)
    next

requestBlockInfo :: MonadIO m
                 => BlockInfoActor
                 -> Maybe (Peer peer)
                 -> Hash h
                 -> m  ()

requestBlockInfo b h = do
  undefined

getBlockInfo :: MonadIO m
             => BlockInfoActor
             -> Maybe (Peer peer)
             -> m (Maybe BlockInfo)

getBlockInfo _ _ = undefined


