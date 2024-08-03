module HBS2.CLI.Run.Peer where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Hash
import HBS2.Base58
import HBS2.Data.Types.Refs
import HBS2.Storage
import HBS2.Peer.RPC.Client
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Net.Auth.Schema()

import Data.List qualified as L
import Data.Maybe
import Control.Monad.Trans.Cont
import Data.Text qualified as Text
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Lens.Micro.Platform

import Text.InterpolatedString.Perl6 (qc)

{- HLINT ignore "Functor law" -}

putTextLit :: forall c m . (IsContext c, MonadUnliftIO m)
           => AnyStorage
           -> Text
           -> RunM c m (Syntax c)

putTextLit sto s = do
    h <- putBlock sto (LBS8.pack (Text.unpack s))
           `orDie` "can't store block"
         <&> HashRef

    pure (mkStr @c (show $ pretty h))

peerEntries :: forall c m . ( IsContext c
                            , MonadUnliftIO m
                            , HasClientAPI PeerAPI UNIX m
                            , HasClientAPI StorageAPI UNIX m
                            , HasStorage m
                            , Exception (BadFormException c)
                            ) => MakeDictM c m ()
peerEntries = do

  entry $ bindMatch "hbs2:peer:detect" $ \case
    _ -> detectRPC <&> maybe (nil @c) mkStr

  entry $ bindMatch "hbs2:peer:get-block" $ \case
    [StringLike s] -> do
      flip runContT pure do

        sto <- getStorage
        ha <- pure (fromStringMay @HashRef s)
               `orDie` "invalid hash"

        lbs <- getBlock sto (fromHashRef ha)
                `orDie` show ("missed-block" <+> pretty ha)

        pure $ mkForm "blob" [mkStr (LBS8.unpack lbs)]

    _ -> throwIO $ BadFormException @c nil

  entry $ bindMatch "hbs2:peer:has-block" $ \case
    [StringLike s] -> do
      flip runContT pure do

        sto <- getStorage

        ha <- pure (fromStringMay @HashRef s)
               `orDie` "invalid hash"

        mbsz <- hasBlock sto (fromHashRef ha)

        pure $ maybe (mkSym "no-block") mkInt mbsz

    _ -> throwIO $ BadFormException @c nil

  -- stores *small* block
  entry $ bindMatch "hbs2:peer:put-block" $ \case
    [ListVal [SymbolVal "blob", LitStrVal s]] -> do
      flip runContT pure do
        sto <- getStorage
        lift $ putTextLit sto s

    [LitStrVal s] -> do
      flip runContT pure do
        sto <- getStorage
        lift $ putTextLit sto s

    _ -> throwIO $ BadFormException @c nil

  brief "checks if peer available"
    $ noArgs
    $ returns "dict" "dictionary of peer attributes"
    $ examples [qc|
(hbs2:peer:poke)

(dict
 (peer-key: "35gKUG1mwBTr3tQpjWwR2kBYEnDmHxesoJL5Lj7tMjq3")
 (udp: "0.0.0.0:7354")
 (tcp: "tcp://0.0.0.0:3001")
 (local-multicast: "239.192.152.145:10153")
 (rpc: "/tmp/hbs2-rpc.socket")
 (http-port: 5000))
    |]
    $ entry $ bindMatch "hbs2:peer:poke" $ \case
      _ -> do
        api <- getClientAPI @PeerAPI @UNIX
        callRpcWaitMay @RpcPoke (TimeoutSec 1) api ()
          <&> fromMaybe ""
          <&> parseTop
          <&> either (const nil) (mkForm "dict" . fmap fixContext)



