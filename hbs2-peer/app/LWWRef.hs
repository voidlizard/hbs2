module LWWRef where

import HBS2.Prelude.Plated
import HBS2.Actors.Peer
import HBS2.Data.Types.Refs
import HBS2.Net.Proto
import HBS2.Base58
import HBS2.Storage
import HBS2.Storage.Operations.Missed
import HBS2.Hash
import HBS2.Peer.Proto
import HBS2.Peer.Proto.LWWRef
import HBS2.Net.Auth.Credentials

import HBS2.Misc.PrettyStuff

import Brains
import PeerConfig
import PeerTypes

import Control.Monad
import UnliftIO
import Lens.Micro.Platform

{- HLINT ignore "Functor law" -}

lwwRefWorker :: forall e s m . ( MonadIO m
                               , MonadUnliftIO m
                               , MyPeer e
                               , HasStorage m
                               , Sessions e (KnownPeer e) m
                               , HasGossip e (LWWRefProto e) m
                               , Signatures s
                               , s ~ Encryption e
                               , IsRefPubKey s
                               )
             => PeerConfig
             -> SomeBrains e
             -> m ()

lwwRefWorker conf brains = do

   let listRefs = listPolledRefs @e brains (Just "lwwref")
                   <&> fmap (\(a,_,b) -> (a,b))
                   <&> fmap (over _2 ( (*60) . fromIntegral) )

   polling (Polling 5 5) listRefs $ \ref -> do
    debug $ yellow "POLLING LWWREF" <+> pretty (AsBase58 ref)
    gossip (LWWRefProto1 @e (LWWProtoGet (LWWRefKey ref)))



