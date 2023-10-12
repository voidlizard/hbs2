{-# Language UndecidableInstances #-}
module HBS2.Peer.RPC.Internal.Types
  ( module  HBS2.Peer.RPC.Internal.Types
  , module HBS2.Peer.RPC.Class
  ) where

import HBS2.Actors.Peer
import HBS2.Net.Proto.Types
import HBS2.Data.Types.Refs (HashRef)
import HBS2.Data.Types.SignedBox
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.Service
import HBS2.Peer.RPC.Class

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Parse

import Data.Kind
import Control.Monad.Reader
import Data.ByteString ( ByteString )
import UnliftIO

data RPC2Context =
  RPC2Context
  { rpcConfig             :: [Syntax MegaParsec]
  , rpcMessaging          :: MessagingUnix
  , rpcPokeAnswer         :: String
  , rpcPeerEnv            :: PeerEnv L4Proto
  , rpcLocalMultiCast     :: Peer L4Proto
  , rpcStorage            :: AnyStorage
  , rpcDoFetch            :: HashRef -> IO ()
  , rpcDoRefChanHeadPost  :: HashRef -> IO ()
  , rpcDoRefChanPropose   :: (PubKey 'Sign HBS2Basic, SignedBox ByteString L4Proto) -> IO ()
  , rpcDoRefChanNotify    :: (PubKey 'Sign HBS2Basic, SignedBox ByteString L4Proto) -> IO ()
  }

instance (Monad m, Messaging MessagingUnix UNIX (Encoded UNIX)) => HasFabriq UNIX (ReaderT RPC2Context m) where
  getFabriq = asks (Fabriq . rpcMessaging)

instance Monad m => HasOwnPeer UNIX (ReaderT RPC2Context m) where
  ownPeer = asks ( msgUnixSelf . rpcMessaging )

instance (MonadUnliftIO m, HasProtocol UNIX (ServiceProto (api :: [Type]) UNIX))
  => HasDeferred UNIX (ServiceProto api UNIX) m where
  deferred _ m = void $ async m

