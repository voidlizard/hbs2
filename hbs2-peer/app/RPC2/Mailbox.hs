{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.Mailbox where

import HBS2.Peer.Prelude

import HBS2.Actors.Peer
import HBS2.Data.Types.SignedBox
import HBS2.Peer.Proto
import HBS2.Peer.Proto.Mailbox
import HBS2.Storage
import HBS2.Net.Messaging.Unix
import HBS2.Misc.PrettyStuff

import PeerTypes

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.Mailbox

import Lens.Micro.Platform
import Control.Monad.Reader
import Control.Monad.Trans.Maybe


instance (MonadIO m) => HandleMethod m RpcMailboxPoke where

  handleMethod key = do
    debug "rpc.RpcMailboxPoke"

