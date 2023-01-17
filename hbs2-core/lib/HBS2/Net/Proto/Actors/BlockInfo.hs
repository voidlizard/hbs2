module HBS2.Net.Proto.Actors.BlockInfo where

import HBS2.Actors
import HBS2.Hash
import HBS2.Net.PeerLocator
import HBS2.Net.Proto
import HBS2.Prelude

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
  , peers :: AnyPeerLocator
  }

-- TODO: send block info request
-- TODO: receive block info request
-- TODO: cache block info per peer
-- TODO: get block info per peer


createBlockInfoActor :: MonadIO m => AnyPeerLocator -> m (BlockInfoActor m )
createBlockInfoActor l = do
  pip <- newPipeline 200 -- FIXME: to settings!
  pure $ BlockInfoActor pip l

runBlockInfoActor :: MonadIO m => BlockInfoActor m -> m ()
runBlockInfoActor b = runPipeline (tasks b)

stopBlockInfoActor :: MonadIO m => BlockInfoActor m -> m ()
stopBlockInfoActor b = stopPipeline (tasks b)

requestBlockInfo :: forall peer h m . ( MonadIO m
                                      , Pretty (Hash h)
                                      , HasPeer peer
                                      )
                 => BlockInfoActor m
                 -> Maybe (Peer peer)
                 -> Hash h
                 -> m  ()

requestBlockInfo b mp h = do
  addJob (tasks b) do
    -- peers <- getPeers
    --
    -- TODO: get given peer or some other peers
    somePeers <- knownPeers @_ @peer (peers b)


    -- TODO: get cookie from cookie generator
    -- TODO: set waiting for request
    -- TODO: send block info request to messaging

    liftIO $ print ( "request-info" <+> pretty h)

getBlockInfo :: MonadIO m
             => BlockInfoActor m
             -> Maybe (Peer peer)
             -> m (Maybe BlockInfo)

getBlockInfo _ _ = undefined


