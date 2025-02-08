{-# Language UndecidableInstances #-}
module HBS2.Peer.RPC.Internal.Types
  ( module  HBS2.Peer.RPC.Internal.Types
  , module HBS2.Peer.RPC.Class
  ) where

import HBS2.Prelude
import HBS2.Actors.Peer
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Types
import HBS2.Storage()
import HBS2.Data.Types.Refs (HashRef)
import HBS2.Data.Types.SignedBox
import HBS2.Net.Messaging.Unix
import HBS2.Net.Messaging.TCP
import HBS2.Net.Messaging.Encrypted.ByPass (ByPassStat)
import HBS2.Net.Proto.Service
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.RPC.Class
import HBS2.Peer.Brains

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Parse

import Data.Kind
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent (ThreadId)
import Data.ByteString ( ByteString )
import UnliftIO
import HBS2.Prelude (asyncLinked)

data RPC2Context =
  RPC2Context
  { rpcSelf                :: ThreadId
  , rpcConfig              :: [Syntax C]
  , rpcMessaging           :: MessagingUnix
  , rpcTCP                 :: Maybe MessagingTCP
  , rpcPokeAnswer          :: String
  , rpcPeerEnv             :: PeerEnv L4Proto
  , rpcLocalMultiCast      :: Peer L4Proto
  , rpcStorage             :: AnyStorage
  , rpcBrains              :: SomeBrains L4Proto
  , rpcByPassInfo          :: IO ByPassStat
  , rpcProbes              :: TVar [AnyProbe]
  , rpcDoFetch             :: HashRef -> IO ()
  , rpcDoRefChanHeadPost   :: HashRef -> IO ()
  , rpcDoRefChanPropose    :: (PubKey 'Sign 'HBS2Basic, SignedBox ByteString 'HBS2Basic) -> IO ()
  , rpcDoRefChanNotify     :: (PubKey 'Sign 'HBS2Basic, SignedBox ByteString 'HBS2Basic) -> IO ()
  , rpcMailboxService      :: AnyMailboxService HBS2Basic
  , rpcMailboxAdapter      :: AnyMailboxAdapter HBS2Basic
  }

instance (Monad m, Messaging MessagingUnix UNIX (Encoded UNIX)) => HasFabriq UNIX (ReaderT RPC2Context m) where
  getFabriq = asks (Fabriq . rpcMessaging)

instance Monad m => HasOwnPeer UNIX (ReaderT RPC2Context m) where
  ownPeer = asks ( msgUnixSelf . rpcMessaging )

instance (MonadUnliftIO m, HasProtocol UNIX (ServiceProto (api :: [Type]) UNIX))
  => HasDeferred (ServiceProto api UNIX) UNIX  m where
  deferred m = void $ async m



