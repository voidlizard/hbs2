{-# Language AllowAmbiguousTypes #-}
module HBS2.CLI.Run.Internal.RefChan (createNewRefChan) where

import HBS2.CLI.Prelude hiding (mapMaybe)
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.KeyMan

import HBS2.Peer.Proto.RefChan
import HBS2.Storage
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema()
import HBS2.Data.Types.SignedBox
import HBS2.Data.Types.Refs
import HBS2.Storage.Operations.Class
import HBS2.KeyMan.Keys.Direct

import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefChan

import Lens.Micro.Platform

createNewRefChan :: forall c m . ( IsContext c
                                 , MonadUnliftIO m
                                 , Exception (BadFormException c)
                                 , HasClientAPI RefChanAPI UNIX m
                                 , HasClientAPI StorageAPI UNIX m
                                 , HasClientAPI PeerAPI UNIX m
                                 , HasStorage m
                                 )
                 => Maybe (PubKey Sign HBS2Basic)
                 -> RefChanHeadBlock L4Proto
                 -> m  (PubKey Sign HBS2Basic)

createNewRefChan mbk rch = do

  peerApi  <- getClientAPI @PeerAPI @UNIX
  rchanApi <- getClientAPI @RefChanAPI @UNIX
  sto      <- getStorage

  refchan <- maybe1 mbk (keymanNewCredentials (Just "refchan") 0) pure

  creds <- runKeymanClientRO $ loadCredentials refchan
               >>= orThrowUser "can't load credentials"

  let box = makeSignedBox @'HBS2Basic (view peerSignPk creds) (view peerSignSk creds) rch

  href <- writeAsMerkle sto  (serialise box)

  --FIXME: timeout-hardcode
  callService @RpcPollAdd peerApi (refchan, "refchan", 17)
      >>= orThrowUser "can't subscribe to refchan"

  callService @RpcRefChanHeadPost rchanApi (HashRef href)
      >>= orThrowUser "can't post refchan head"

  pure refchan


