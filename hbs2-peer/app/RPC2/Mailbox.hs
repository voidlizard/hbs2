{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.Mailbox where

import HBS2.Peer.Prelude

import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.Actors.Peer
import HBS2.Data.Types.SignedBox
import HBS2.Peer.Proto
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Ref
import HBS2.Peer.Proto.Mailbox.Types

import HBS2.Storage
import HBS2.Net.Messaging.Unix
import HBS2.Misc.PrettyStuff

import HBS2.Peer.RPC.API.Peer

import PeerTypes

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.Mailbox

import Data.Either
import Lens.Micro.Platform
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

type ForMailboxRPC m = (MonadIO m, HasRpcContext MailboxAPI RPC2Context m)


instance (MonadIO m) => HandleMethod m RpcMailboxPoke where

  handleMethod key = do
    debug "rpc.RpcMailboxPoke"

instance Monad m => HasRpcContext MailboxAPI RPC2Context (ResponseM UNIX (ReaderT RPC2Context m)) where
  getRpcContext = lift ask

instance (ForMailboxRPC m) => HandleMethod m RpcMailboxCreate where

  handleMethod (puk, t) = do
    AnyMailboxService mbs <- getRpcContext @MailboxAPI @RPC2Context <&> rpcMailboxService
    void $ mailboxCreate @HBS2Basic mbs t puk
    debug $ "rpc.RpcMailboxCreate" <+> pretty (AsBase58 puk) <+> pretty t


instance (ForMailboxRPC m) => HandleMethod m RpcMailboxGetStatus where

  handleMethod puk = do
    AnyMailboxService mbs <- getRpcContext @MailboxAPI @RPC2Context <&> rpcMailboxService
    debug $ "rpc.RpcMailboxGetStatus" <+> pretty (AsBase58 puk)
    mailboxGetStatus @HBS2Basic mbs (MailboxRefKey puk)

instance (ForMailboxRPC m) => HandleMethod m RpcMailboxDelete where

  handleMethod puk = do
    AnyMailboxService mbs <- getRpcContext @MailboxAPI @RPC2Context <&> rpcMailboxService
    void $ mailboxDelete @HBS2Basic mbs puk
    debug $ "rpc.RpcMailboxDelete" <+> pretty (AsBase58 puk)

instance (ForMailboxRPC m) => HandleMethod m RpcMailboxList where

  handleMethod _ = do
    AnyMailboxService mbs <- getRpcContext @MailboxAPI @RPC2Context <&> rpcMailboxService
    r <- mailboxListBasic @HBS2Basic mbs
    pure $ fromRight mempty r

instance (ForMailboxRPC m) => HandleMethod m RpcMailboxSend where

  handleMethod mess = do
    co <- getRpcContext @MailboxAPI @RPC2Context
    let w = rpcMailboxService co
    debug $ "rpc.RpcMailboxSend"
    void $ mailboxSendMessage w mess

instance (ForMailboxRPC m) => HandleMethod m RpcMailboxGet where

  handleMethod mbox = do
    RPC2Context{..} <- getRpcContext @MailboxAPI @RPC2Context
    debug $ "rpc.RpcMailboxGet"
    getRef rpcStorage (MailboxRefKey @HBS2Basic mbox)
             <&> fmap HashRef


