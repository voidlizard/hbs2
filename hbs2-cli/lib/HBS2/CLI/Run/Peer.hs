module HBS2.CLI.Run.Peer where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Hash
import HBS2.Base58
import HBS2.Data.Types.Refs
import HBS2.Storage
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Net.Auth.Schema()

import Data.List qualified as L
import Data.Maybe
import Control.Monad.Trans.Cont
import Data.Text qualified as Text
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Lens.Micro.Platform

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

peerEntries :: forall c m . (c ~ C, IsContext c, MonadUnliftIO m) => MakeDictM c m ()
peerEntries = do

  entry $ bindMatch "hbs2:peer:detect" $ \case
    _ -> detectRPC <&> maybe (nil @c) mkStr

  entry $ bindMatch "hbs2:peer:get-block" $ \case
    [StringLike s] -> do
      flip runContT pure do

        sto <- ContT withPeerStorage
        ha <- pure (fromStringMay @HashRef s)
               `orDie` "invalid hash"

        lbs <- getBlock sto (fromHashRef ha)
                `orDie` show ("missed-block" <+> pretty ha)

        pure $ mkForm "blob" [mkStr (LBS8.unpack lbs)]

    _ -> throwIO $ BadFormException @C nil

  entry $ bindMatch "hbs2:peer:has-block" $ \case
    [StringLike s] -> do
      flip runContT pure do

        sto <- ContT withPeerStorage
        ha <- pure (fromStringMay @HashRef s)
               `orDie` "invalid hash"

        mbsz <- hasBlock sto (fromHashRef ha)

        pure $ maybe (mkSym "no-block") mkInt mbsz

    _ -> throwIO $ BadFormException @C nil

  -- stores *small* block
  entry $ bindMatch "hbs2:peer:put-block" $ \case
    [ListVal [SymbolVal "blob", LitStrVal s]] -> do
      flip runContT pure do
        sto <- ContT withPeerStorage
        lift $ putTextLit sto s

    [LitStrVal s] -> do
      flip runContT pure do
        sto <- ContT withPeerStorage
        lift $ putTextLit sto s

    _ -> throwIO $ BadFormException @C nil

  entry $ bindMatch "hbs2:peer:poke" $ \case
    _ -> do
      so <- detectRPC `orDie` "hbs2-peer not found"
      r <- newTVarIO nil
      withRPC2 @PeerAPI  @UNIX so $ \caller -> do

        what <- callRpcWaitMay @RpcPoke (TimeoutSec 1) caller ()
                  <&> fromMaybe ""
                  <&> parseTop
                  <&> either (const nil) (mkForm "dict")

        atomically $ writeTVar r what

      readTVarIO r



