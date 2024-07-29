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
import HBS2.Peer.Proto.LWWRef
import HBS2.Base58
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema()
import HBS2.Data.Types.SignedBox

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

pattern HashLike:: forall {c} . HashRef -> Syntax c
pattern HashLike x <- (
  \case
    StringLike s -> fromStringMay @HashRef s
    _            -> Nothing
      -> Just x )

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

  entry $ bindMatch "hbs2:lwwref:list" $ \case
    [] -> do
      flip runContT pure do
        so <- detectRPC `orDie` "rpc not found"
        api <- ContT $ withRPC2 @PeerAPI @UNIX so
        r <- callService @RpcPollList2 api (Just "lwwref", Nothing)
               >>= orThrowUser "can't get lwwref list"
        pure $ mkList $ fmap (mkStr . show . pretty . AsBase58 . view _1) r

    _ -> throwIO (BadFormException @C nil)


  entry $ bindMatch "hbs2:lwwref:fetch" $ \case
    [StringLike puk] -> do
      flip runContT pure do
        lww <- orThrowUser "bad lwwref key" (fromStringMay puk)
        so <- detectRPC `orDie` "rpc not found"
        api <- ContT $ withRPC2 @LWWRefAPI @UNIX so
        void $ callService @RpcLWWRefFetch api lww
        pure $ mkStr "okay"

    _ -> throwIO (BadFormException @C nil)


  entry $ bindMatch "hbs2:lwwref:get" $ \case
    [StringLike puk] -> do

      flip runContT pure do
        ref <- orThrowUser "bad lwwref key" (fromStringMay puk)
        so <- detectRPC `orDie` "rpc not found"
        api <- ContT $ withRPC2 @LWWRefAPI  @UNIX so
        what <- callService @RpcLWWRefGet api ref
                  >>= orThrowUser "can't get lwwref value"
        pure $ mkStr (show $ pretty what)

    _ -> throwIO (BadFormException @C nil)


  entry $ bindMatch "hbs2:lwwref:update" $ \case
    [StringLike puks, HashLike new] -> do

      flip runContT pure do
        puk <- orThrowUser "bad lwwref key" (fromStringMay  puks)
        so <- detectRPC `orDie` "rpc not found"
        api <- ContT $ withRPC2 @LWWRefAPI  @UNIX so

        (sk,pk) <- liftIO $ runKeymanClient do
                     creds  <- loadCredentials puk
                                 >>= orThrowUser "can't load credentials"
                     pure ( view peerSignSk  creds, view peerSignPk creds )

        what <- callService @RpcLWWRefGet api puk
                  >>= orThrowUser "can't get lwwref value"

        sno' <- case what of
                 Nothing    -> pure 0
                 Just lwwv  -> pure (lwwSeq lwwv)

        let sno =  succ sno'

        let box =  makeSignedBox pk sk (LWWRef sno new Nothing)

        callService @RpcLWWRefUpdate api box
          >>= orThrowUser "lww ref update error"

        pure nil

    _ -> throwIO (BadFormException @C nil)

    -- (sk,pk) <- liftIO $ runKeymanClient do
    --              creds  <- loadCredentials puk >>= orThrowUser "can't load credentials"
    --              pure ( view peerSignSk  creds, view peerSignPk creds )

    -- seq <- case seq' of
    --          Just v -> pure v
    --          Nothing -> do
    --            let ref = LWWRefKey puk
    --            callService @RpcLWWRefGet caller ref >>= \case
    --              Left e  -> err (viaShow e) >> exitFailure
    --              Right Nothing -> err ("not found value for" <+> pretty ref) >> exitFailure
    --              Right (Just r) -> pure $ succ (lwwSeq r)

    -- let box =  makeSignedBox pk sk (LWWRef seq val Nothing)
    -- callService @RpcLWWRefUpdate caller box >>= \case
    --   Left e  -> err (viaShow e) >> exitFailure
    --   Right r -> print $ pretty r


