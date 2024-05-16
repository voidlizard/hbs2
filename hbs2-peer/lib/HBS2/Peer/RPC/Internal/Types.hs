{-# Language UndecidableInstances #-}
module HBS2.Peer.RPC.Internal.Types
  ( module  HBS2.Peer.RPC.Internal.Types
  , module HBS2.Peer.RPC.Class
  ) where

import HBS2.Actors.Peer
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Types
import HBS2.Storage()
import HBS2.Data.Types.Refs (HashRef)
import HBS2.Data.Types.SignedBox
import HBS2.Net.Messaging.Unix
import HBS2.Net.Messaging.Encrypted.ByPass (ByPassStat)
import HBS2.Net.Proto.Service
import HBS2.Peer.RPC.Class
import HBS2.Peer.Brains

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Parse

import Data.Kind
import Control.Monad.Reader
import Data.ByteString ( ByteString )
import UnliftIO

data RPC2Context =
  RPC2Context
  { rpcConfig             :: [Syntax C]
  , rpcMessaging          :: MessagingUnix
  , rpcPokeAnswer         :: String
  , rpcPeerEnv            :: PeerEnv L4Proto
  , rpcLocalMultiCast     :: Peer L4Proto
  , rpcStorage            :: AnyStorage
  , rpcBrains             :: SomeBrains L4Proto
  , rpcByPassInfo         :: IO ByPassStat
  , rpcDoFetch            :: HashRef -> IO ()
  , rpcDoRefChanHeadPost  :: HashRef -> IO ()
  , rpcDoRefChanPropose   :: (PubKey 'Sign 'HBS2Basic, SignedBox ByteString 'HBS2Basic) -> IO ()
  , rpcDoRefChanNotify    :: (PubKey 'Sign 'HBS2Basic, SignedBox ByteString 'HBS2Basic) -> IO ()
  }

instance (Monad m, Messaging MessagingUnix UNIX (Encoded UNIX)) => HasFabriq UNIX (ReaderT RPC2Context m) where
  getFabriq = asks (Fabriq . rpcMessaging)

instance Monad m => HasOwnPeer UNIX (ReaderT RPC2Context m) where
  ownPeer = asks ( msgUnixSelf . rpcMessaging )

instance (MonadUnliftIO m, HasProtocol UNIX (ServiceProto (api :: [Type]) UNIX))
  => HasDeferred (ServiceProto api UNIX) UNIX  m where
  deferred m = void $ async m


