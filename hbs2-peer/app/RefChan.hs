{-# Language AllowAmbiguousTypes #-}
module RefChan where

import HBS2.Prelude.Plated

import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Clock
import HBS2.Data.Types.Refs
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.RefChan
import HBS2.Net.Proto.Types

import HBS2.System.Logger.Simple

import PeerTypes
import PeerConfig

import Control.Monad


data RefChanWorkerEnv e = RefChanWorkerEnv

refChanWorkerEnv :: forall m e . MonadIO m
                 => PeerConfig
                 -> m (RefChanWorkerEnv e)

refChanWorkerEnv _ = pure $ RefChanWorkerEnv @e

refChanWorker :: forall e s m . ( MonadIO m, MyPeer e
                                , HasStorage m
                                , Signatures s
                                , s ~ Encryption e
                                , IsRefPubKey s
                                , Pretty (AsBase58 (PubKey 'Sign s))
                                )
             => RefChanWorkerEnv e
             -> m ()

refChanWorker _ = forever do
  pause @'Seconds 10
  debug "I'm refchan worker"

