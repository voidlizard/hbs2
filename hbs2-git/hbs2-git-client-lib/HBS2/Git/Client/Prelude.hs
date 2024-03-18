module HBS2.Git.Client.Prelude
  ( module HBS2.Prelude.Plated
  , module HBS2.Base58
  , module HBS2.Clock
  , module HBS2.Hash
  , module HBS2.Data.Types.Refs
  , module HBS2.Net.Auth.Credentials
  , module HBS2.Merkle
  , module HBS2.Storage
  , module HBS2.Net.Messaging.Unix
  , module HBS2.OrDie
  , module HBS2.Misc.PrettyStuff
  , module HBS2.System.Logger.Simple.ANSI

  -- peer
  , module HBS2.Net.Proto.Service
  , module HBS2.Peer.Proto.LWWRef
  , module HBS2.Peer.RPC.API.Peer
  , module HBS2.Peer.RPC.API.RefLog
  , module HBS2.Peer.RPC.API.LWWRef
  , module HBS2.Peer.RPC.API.Storage
  , module HBS2.Peer.RPC.Client.StorageClient

  , module Control.Applicative
  , module Control.Monad.Reader
  , module Control.Monad.Trans.Cont
  , module Control.Monad.Trans.Maybe
  , module System.Process.Typed
  , module Control.Monad.Except
  , module Lens.Micro.Platform
  , module UnliftIO

  , getSocketName
  , formatRef
  , deserialiseOrFail
  ) where

import HBS2.Prelude.Plated hiding (at)
import HBS2.Base58
import HBS2.Clock

import HBS2.Peer.Proto

import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Net.Auth.Credentials
import HBS2.Merkle
import HBS2.Storage
import HBS2.OrDie
import HBS2.Misc.PrettyStuff
import HBS2.System.Logger.Simple.ANSI

import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.Service

import HBS2.Peer.Proto.LWWRef
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient

import HBS2.Peer.CLI.Detect

import Control.Applicative
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.Except
import Control.Exception
import Control.Monad.Trans.Maybe
import UnliftIO
import System.Process.Typed
import Lens.Micro.Platform
import Codec.Serialise

data RPCNotFoundError = RPCNotFoundError
                        deriving stock (Show,Typeable)


instance Exception RPCNotFoundError

instance HasErrorStatus RPCNotFoundError where
  getStatus = const Failed

getSocketName :: forall m . (MonadUnliftIO m, MonadError RPCNotFoundError m) => m FilePath
getSocketName = do
  detectRPC >>= maybe (throwError RPCNotFoundError) pure


formatRef :: (Pretty a1, Pretty a2) => (a1, a2) -> Doc ann
formatRef (r,h) = pretty h <+> pretty r

