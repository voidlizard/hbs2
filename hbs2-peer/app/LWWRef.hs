module LWWRef where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Actors.Peer
import HBS2.Events
import HBS2.Data.Types.Refs
import HBS2.Data.Detect
import HBS2.Net.PeerLocator
import HBS2.Net.Proto
import HBS2.Base58
import HBS2.Storage
import HBS2.Storage.Operations.Missed
import HBS2.Hash
import HBS2.Peer.Proto
import HBS2.Net.Auth.Credentials
import HBS2.Merkle

import HBS2.Misc.PrettyStuff

import Brains
import PeerConfig
import PeerTypes

import Control.Monad
import UnliftIO

lwwRefWorker :: forall e s m . ( MonadIO m
                               , MonadUnliftIO m
                               , MyPeer e
                               , HasStorage m
                               , Sessions e (KnownPeer e) m
                               , Signatures s
                               , s ~ Encryption e
                               , IsRefPubKey s
                               )
             => PeerConfig
             -> SomeBrains e
             -> m ()

lwwRefWorker conf brains = do
  forever do
    debug $ yellow "lwwRefWorker"
    pause @'Seconds 20


