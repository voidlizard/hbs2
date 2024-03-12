module HBS2.Peer.Proto.LWWRef.Internal
  ( module HBS2.Peer.Proto.LWWRef.Internal
  , module HBS2.Peer.Proto.LWWRef
  ) where

import HBS2.Prelude.Plated
import HBS2.Peer.Proto.LWWRef

import HBS2.Hash
import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Net.Auth.Credentials
import HBS2.Base58
import HBS2.Events
import HBS2.Actors.Peer.Types
import HBS2.Peer.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Data.Types.Refs
import HBS2.Misc.PrettyStuff

import HBS2.System.Logger.Simple

import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Hashable hiding (Hashed)
import Data.ByteString (ByteString)
import Type.Reflection (someTypeRep)
import Lens.Micro.Platform


lwwRefProto :: forall e s m proto . ( MonadIO m
                                    , Request e proto m
                                    , Response e proto m
                                    , HasDeferred proto e m
                                    , HasGossip e (LWWRefProto e) m
                                    , IsPeerAddr e m
                                    , Pretty (Peer e)
                                    , Sessions e (KnownPeer e) m
                                    , Signatures s
                                    , Pretty (AsBase58 (PubKey 'Sign s))
                                    , s ~ Encryption e
                                    , proto ~ LWWRefProto e
                                    )
                  => LWWRefProto e -> m ()

lwwRefProto req = do
  debug $ yellow "lwwRefProto"
  pure ()

