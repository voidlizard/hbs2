module HBS2.CLI.Run.RefLog where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.KeyMan

import HBS2.Data.Types.Refs
import HBS2.Merkle
import HBS2.Storage
import HBS2.Peer.RPC.Client
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.Storage

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

import Streaming.Prelude qualified as S

getCredentialsForReflog :: MonadUnliftIO m => RefLogKey 'HBS2Basic -> m (PeerCredentials 'HBS2Basic)
getCredentialsForReflog reflog = do
  runKeymanClient (loadCredentials reflog)
     >>= orThrowUser "credentials not found"

mkRefLogUpdateFrom :: (MonadUnliftIO m) => RefLogKey 'HBS2Basic -> m ByteString ->  m (RefLogUpdate L4Proto)
mkRefLogUpdateFrom reflog mbs = do
  what <- getCredentialsForReflog reflog
  let puk = view peerSignPk what
  let privk = view peerSignSk what
  txraw <- mbs
  makeRefLogUpdate @L4Proto @'HBS2Basic (coerce puk) privk txraw


reflogEntries :: forall c m . ( IsContext c
                              , Exception (BadFormException c)
                              , MonadUnliftIO m
                              , HasStorage m
                              , HasClientAPI PeerAPI UNIX m
                              , HasClientAPI RefLogAPI UNIX m
                              , HasClientAPI StorageAPI UNIX m
                              ) => MakeDictM  c m ()
reflogEntries = do

  entry $ bindMatch "hbs2:reflog:create" $ \case
    [] -> do
      reflog <- keymanNewCredentials (Just "reflog") 0

      api <- getClientAPI @PeerAPI  @UNIX
      void $ callService @RpcPollAdd api (reflog, "reflog", 31)
      pure $ mkStr (show $ pretty (AsBase58 reflog))

    _ -> throwIO (BadFormException @C nil)

  entry $ bindMatch "hbs2:reflog:add" $ \case
    [SignPubKeyLike reflog] -> do
      -- reflog <- keymanNewCredentials (Just "reflog") 0

      api <- getClientAPI @PeerAPI @UNIX
      void $ callService @RpcPollAdd api (reflog, "reflog", 31)
      pure $ mkStr (show $ pretty (AsBase58 reflog))

    _ -> throwIO (BadFormException @C nil)



  entry $ bindMatch "hbs2:reflog:tx:annhashref:create" $ \case
    [StringLike puk, StringLike hash] -> do
      reflog <- orThrowUser "bad reflog key" (fromStringMay puk)
      sto <- getStorage
      hashref <- orThrowUser "bad hash" (fromStringMay @HashRef hash)
      void $ hasBlock sto (fromHashRef hashref) `orDie` "no block"
      let sref = AnnotatedHashRef Nothing hashref
      rlu <- mkRefLogUpdateFrom reflog (pure $ LBS.toStrict $ serialise sref) <&> serialise
      pure $ mkForm "blob" [mkStr (LBS8.unpack rlu)]

    _ -> throwIO (BadFormException @C nil)


  entry $ bindMatch "hbs2:reflog:tx:post" $ nil_ \case
    [BlobLike blob] -> do
      caller <- getClientAPI @RefLogAPI @UNIX
      wtf <- deserialiseOrFail @(RefLogUpdate L4Proto) (LBS.fromStrict blob)
              & orThrowUser "invalid tx"
      void $ callService @RpcRefLogPost caller wtf

    _ -> throwIO (BadFormException @C nil)

  entry $ bindMatch "hbs2:reflog:tx:seqref:create" $ \case
    [StringLike puk, LitIntVal sn, StringLike hash] -> do
      reflog <- orThrowUser "bad reflog key" (fromStringMay puk)
      sto <- getStorage
      hashref <- orThrowUser "bad hash" (fromStringMay @HashRef hash)
      void $ hasBlock sto (fromHashRef hashref) `orDie` "no block"
      let sref = SequentialRef sn (AnnotatedHashRef Nothing hashref)
      rlu <- mkRefLogUpdateFrom reflog (pure $ LBS.toStrict $ serialise sref) <&> serialise
      pure $ mkForm "blob" [mkStr (LBS8.unpack rlu)]

    _ -> throwIO (BadFormException @C nil)

  entry $ bindMatch "hbs2:reflog:tx:raw:create" $ \case
    [SymbolVal "stdin", SignPubKeyLike reflog] -> do

      rlu <- mkRefLogUpdateFrom (RefLogKey reflog) ( liftIO BS.getContents )
                <&> serialise

      pure $ mkForm "blob" [mkStr (LBS8.unpack rlu)]

    [LitStrVal s, StringLike rlo] -> do
      reflog <- orThrowUser "bad reflog" (fromStringMay rlo)

      rlu <- mkRefLogUpdateFrom reflog ( pure (BS8.pack (Text.unpack s)) )
               <&> serialise

      pure $ mkForm "blob" [mkStr (LBS8.unpack rlu)]

    _ -> throwIO (BadFormException @C nil)


  entry $ bindMatch "hbs2:reflog:get" $ \case
    [StringLike puk] -> do

      flip runContT pure do
        reflog <- orThrowUser "bad reflog key" (fromStringMay puk)
        api <- getClientAPI @RefLogAPI  @UNIX
        what <- callService @RpcRefLogGet api reflog
                  >>= orThrowUser "can't get reflog"
        pure $ mkStr (show $ pretty what)

    _ -> throwIO (BadFormException @C nil)

  entry $ bindMatch "hbs2:reflog:fetch" $ \case
    [StringLike puk] -> do
      flip runContT pure do
        reflog <- orThrowUser "bad reflog key" (fromStringMay puk)
        api <- getClientAPI @RefLogAPI  @UNIX
        void $ callService @RpcRefLogFetch api reflog
        pure $ mkStr "okay"

    _ -> throwIO (BadFormException @C nil)

  entry $ bindMatch "hbs2:reflog:list" $ \case
    [] -> do
      flip runContT pure do
        api <- getClientAPI @PeerAPI @UNIX
        r <- callService @RpcPollList2 api (Just "reflog", Nothing)
               >>= orThrowUser "can't get reflog list"
        pure $ mkList $ fmap (mkStr . show . pretty . AsBase58 . view _1) r

    _ -> throwIO (BadFormException @C nil)


  entry $ bindMatch "hbs2:reflog:tx:seqref:decode" $ \case
    [ListVal [SymbolVal "blob", LitStrVal s]] -> do
      let lbs =  Text.unpack s & BS8.pack & LBS.fromStrict

      SequentialRef n (AnnotatedHashRef _ h) <-  deserialiseOrFail @SequentialRef lbs
                                                   & orThrowUser "FUCKED"

      pure $ mkForm "seqref" [mkInt n, mkStr (show $ pretty h)]

    e -> throwIO $ BadFormException @c nil

  entry $ bindMatch "hbs2:reflog:tx:list" $ \case
     [e, SignPubKeyLike puk] -> do

      flip runContT pure do

        callCC \exit -> do

          api <- getClientAPI @RefLogAPI  @UNIX
          sto <- getStorage

          r <- callService @RpcRefLogGet api puk
                 >>= orThrowUser "can't get reflog value"

          rlh <- ContT $ maybe1 r (pure nil)

          hashes <- S.toList_ do
                      walkMerkle @[HashRef] (fromHashRef rlh) (getBlock sto) $ \case
                        (Left _) -> lift $ exit nil
                        (Right (hs :: [HashRef]))  -> S.each hs

          rr <- forM hashes $ \ha -> do

            tx <- getBlock sto (coerce ha)
                     >>= orThrowUser "missed-block"
                     <&> deserialiseOrFail @(RefLogUpdate L4Proto)
                     >>= orThrowUser "invalid-tx"

            let bs = view refLogUpdData tx
            let bs8 = BS8.unpack bs

            lift $ apply_ e [mkForm "blob" [mkStr bs8]]

          pure $ mkList rr

     _ -> throwIO (BadFormException @C nil)




