module HBS2.Net.Proto.Actors.BlockInfo where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Net.Proto
import HBS2.Clock
import HBS2.Actors

import Data.Function
import Data.Kind
import Prettyprinter

-- needs: logger
-- needs: reader and shit
-- needs: messaging
-- needs: cookie manager
-- needs: peer manager

data BlockInfoActor (m :: Type -> Type) =
  BlockInfoActor
  { tasks :: Pipeline m ()
  }


-- TODO: send block info request
-- TODO: receive block info request
-- TODO: cache block info per peer
-- TODO: get block info per peer


createBlockInfoActor :: MonadIO m => m (BlockInfoActor m )
createBlockInfoActor = do
  pip <- newPipeline 200 -- FIXME: to settings!
  pure $ BlockInfoActor pip

runBlockInfoActor :: MonadIO m => BlockInfoActor m -> m ()
runBlockInfoActor b = runPipeline (tasks b)

stopBlockInfoActor :: MonadIO m => BlockInfoActor m -> m ()
stopBlockInfoActor b = stopPipeline (tasks b)

requestBlockInfo :: forall peer h m . ( MonadIO m
                                      , Pretty (Hash h)
                                      )
                 => BlockInfoActor m
                 -> Maybe (Peer peer)
                 -> Hash h
                 -> m  ()

requestBlockInfo b _ h = do
  addJob (tasks b) do
    liftIO $ print ( "request-info" <+> pretty h)

getBlockInfo :: MonadIO m
             => BlockInfoActor m
             -> Maybe (Peer peer)
             -> m (Maybe BlockInfo)

getBlockInfo _ _ = undefined


