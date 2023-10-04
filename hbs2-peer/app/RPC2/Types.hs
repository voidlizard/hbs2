module RPC2.Types
  ( module RPC2.Types
  , module HBS2.Peer.RPC.Class
  ) where

import HBS2.Actors.Peer
import HBS2.Net.Proto.Types
import HBS2.Data.Types.Refs (HashRef)
import HBS2.Data.Types.SignedBox
import HBS2.Peer.RPC.Class

import Data.Config.Suckless.Syntax
import PeerTypes (DownloadEnv(..))
import PeerConfig

import Data.ByteString ( ByteString )

data RPC2Context =
  RPC2Context
  { rpcConfig             :: [Syntax C]
  , rpcPokeAnswer         :: String
  , rpcPeerEnv            :: PeerEnv L4Proto
  , rpcDownloadEnv        :: DownloadEnv L4Proto
  , rpcLocalMultiCast     :: Peer L4Proto
  , rpcStorage            :: AnyStorage
  , rpcDoRefChanHeadPost  :: HashRef -> IO ()
  , rpcDoRefChanPropose   :: (PubKey 'Sign HBS2Basic, SignedBox ByteString L4Proto) -> IO ()
  , rpcDoRefChanNotify    :: (PubKey 'Sign HBS2Basic, SignedBox ByteString L4Proto) -> IO ()
  }


