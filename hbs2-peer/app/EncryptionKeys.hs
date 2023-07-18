module EncryptionKeys where

import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Clock
import HBS2.Data.Detect
import HBS2.Data.Types.Refs
import HBS2.Events
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.Auth.Credentials
import HBS2.Net.PeerLocator
import HBS2.Net.Proto
import HBS2.Net.Proto.EncryptionHandshake
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.System.Logger.Simple

import PeerConfig
import PeerTypes

import Codec.Serialise
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable(for_)
import Data.Function(fix)
import Data.Functor
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe
import Data.Text qualified as Text


encryptionHandshakeWorker :: forall e m s .
    ( MonadIO m
    , m ~ PeerM e IO
    , s ~ Encryption e
    , e ~ L4Proto
    , HasPeerLocator e m
    -- , HasPeer e
    -- , HasNonces (EncryptionHandshake e) m
    -- , Request e (EncryptionHandshake e) m
    -- , Sessions e (EncryptionHandshake e) m
    -- , Sessions e (PeerInfo e) m
    -- , Sessions e (KnownPeer e) m
    -- , Pretty (Peer e)
    -- , HasCredentials s m
    )
  => PeerConfig
  -> PeerEnv e
  -> PeerCredentials s
  -> EncryptionHandshakeAdapter e m s
  -> m ()

encryptionHandshakeWorker pconf penv creds EncryptionHandshakeAdapter{..} = do

    -- e :: PeerEnv e <- ask
    let ourpubkey = pubKeyFromKeypair @s $ _envAsymmetricKeyPair penv

    pl <- getPeerLocator @e

    forever do
        liftIO $ pause @'Seconds 30

        peers <- knownPeers @e pl

        forM_ peers \peer -> do
            -- TODO: Только если ещё не знаем ключ ноды
            sendBeginEncryptionExchange @e penv creds peer ourpubkey
