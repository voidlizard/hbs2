module HBS2.CLI.Run.LWWRef where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.KeyMan

import HBS2.Data.Types.Refs
import HBS2.Storage
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.LWWRef

import HBS2.Peer.Proto hiding (request)
import HBS2.Base58
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema()

import HBS2.KeyMan.Keys.Direct
import HBS2.KeyMan.App.Types

import Codec.Serialise
import Data.Coerce
import Data.Either
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as TE
import Data.Text qualified as Text
import Control.Monad.Trans.Cont


lwwRefEntries :: forall c m . (c ~ C, IsContext c, MonadUnliftIO m) => MakeDictM  c m ()
lwwRefEntries = do

  entry $ bindMatch "hbs2:lwwref:create" $ \case
    [] -> do
      reflog <- keymanNewCredentials (Just "lwwref") 0

      flip runContT pure do
        so <- detectRPC `orDie` "rpc not found"
        api <- ContT $ withRPC2 @PeerAPI  @UNIX so
        void $ callService @RpcPollAdd api (reflog, "lwwref", 31)
        pure $ mkStr (show $ pretty (AsBase58 reflog))

    _ -> throwIO (BadFormException @C nil)


  -- entry $ bindMatch "hbs2:reflog:tx:annhashref:create" $ \case
  --   [StringLike puk, StringLike hash] -> do
  --     flip runContT pure do
  --       reflog <- orThrowUser "bad reflog key" (fromStringMay puk)
  --       sto <- ContT withPeerStorage
  --       hashref <- orThrowUser "bad hash" (fromStringMay @HashRef hash)
  --       void $ hasBlock sto (fromHashRef hashref) `orDie` "no block"
  --       let sref = AnnotatedHashRef Nothing hashref
  --       rlu <- lift $ mkRefLogUpdateFrom reflog (pure $ LBS.toStrict $ serialise sref) <&> serialise
  --       pure $ mkForm "blob" [mkStr (LBS8.unpack rlu)]

  --   _ -> throwIO (BadFormException @C nil)


  -- entry $ bindMatch "hbs2:reflog:tx:post" $ nil_ \case
  --   [BlobLike blob] -> do
  --     so <- detectRPC `orDie` "no rpc found"
  --     withRPC2 @RefLogAPI so $ \caller -> do
  --       wtf <- deserialiseOrFail @(RefLogUpdate L4Proto) (LBS.fromStrict blob)
  --               & orThrowUser "invalid tx"
  --       void $ callService @RpcRefLogPost caller wtf

  --   _ -> throwIO (BadFormException @C nil)

  -- entry $ bindMatch "hbs2:reflog:tx:seqref:create" $ \case
  --   [StringLike puk, LitIntVal sn, StringLike hash] -> do
  --     flip runContT pure do
  --       reflog <- orThrowUser "bad reflog key" (fromStringMay puk)
  --       sto <- ContT withPeerStorage
  --       hashref <- orThrowUser "bad hash" (fromStringMay @HashRef hash)
  --       void $ hasBlock sto (fromHashRef hashref) `orDie` "no block"
  --       let sref = SequentialRef sn (AnnotatedHashRef Nothing hashref)
  --       rlu <- lift $ mkRefLogUpdateFrom reflog (pure $ LBS.toStrict $ serialise sref) <&> serialise
  --       pure $ mkForm "blob" [mkStr (LBS8.unpack rlu)]

  --   _ -> throwIO (BadFormException @C nil)

  -- entry $ bindMatch "hbs2:reflog:tx:create-raw" $ \case
  --   [SymbolVal "stdin", StringLike rlo] -> do
  --     reflog <- orThrowUser "bad reflog" (fromStringMay rlo)

  --     rlu <- mkRefLogUpdateFrom reflog ( liftIO BS.getContents )
  --               <&> serialise

  --     pure $ mkForm "blob" [mkStr (LBS8.unpack rlu)]

  --   [LitStrVal s, StringLike rlo] -> do
  --     reflog <- orThrowUser "bad reflog" (fromStringMay rlo)

  --     rlu <- mkRefLogUpdateFrom reflog ( pure (BS8.pack (Text.unpack s)) )
  --              <&> serialise

  --     pure $ mkForm "blob" [mkStr (LBS8.unpack rlu)]

  --   _ -> throwIO (BadFormException @C nil)



