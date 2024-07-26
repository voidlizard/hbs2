module HBS2.CLI.Run.Peer where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Data.Types.Refs
import HBS2.Storage
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Net.Auth.Schema()

import Data.Maybe
import Control.Monad.Trans.Cont
import Data.Text qualified as Text
import Data.ByteString.Lazy.Char8 qualified as LBS8

{- HLINT ignore "Functor law" -}

peerEntries :: forall c m . (c ~ C, IsContext c, MonadUnliftIO m) => MakeDictM c m ()
peerEntries = do

  entry $ bindMatch "hbs2:peer:detect" $ nil_ \case
    _ -> do
      so <- detectRPC
      display so

  -- stores *small* block
  entry $ bindMatch "hbs2:peer:put-block" $ \case
    [LitStrVal s] -> do
      flip runContT pure do
        sto <- ContT withPeerStorage

        h <- putBlock sto (LBS8.pack (Text.unpack s))
               `orDie` "can't store block"
             <&> HashRef

        pure (mkStr @c (show $ pretty h))

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


