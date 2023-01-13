module HBS2.Net.Proto.Actors.BlockInfo where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Net.Proto
import HBS2.Clock

import Data.Function
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Control.Concurrent.STM.TBMQueue qualified as TBMQ
import Control.Concurrent.STM

-- needs: logger
-- needs: reader and shit
-- needs: messaging
-- needs: cookie manager
-- needs: peer manager

data BlockInfoActor =
  BlockInfoActor
  { tasks :: TBMQueue (IO ())
  }


-- TODO: send block info request
-- TODO: receive block info request
-- TODO: cache block info per peer
-- TODO: get block info per peer


createBlockInfoActor :: MonadIO m => m BlockInfoActor
createBlockInfoActor = do
  qtask <- liftIO $ atomically $ TBMQ.newTBMQueue 500 -- FIXME: settings
  pure $ BlockInfoActor undefined

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


