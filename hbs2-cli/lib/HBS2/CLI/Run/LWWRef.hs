module HBS2.CLI.Run.LWWRef where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.KeyMan

import HBS2.Data.Types.Refs
import HBS2.Storage
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.LWWRef

import HBS2.Peer.Proto.LWWRef
import HBS2.Base58
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema()
import HBS2.Data.Types.SignedBox

import HBS2.KeyMan.Keys.Direct
import HBS2.KeyMan.App.Types

import Control.Monad.Trans.Cont


lwwRefEntries :: forall c m . ( IsContext c
                              , MonadUnliftIO m
                              , Exception (BadFormException c)
                              , HasClientAPI PeerAPI UNIX m
                              , HasClientAPI LWWRefAPI UNIX m
                              ) => MakeDictM  c m ()
lwwRefEntries = do

  brief "creates a new lwwref"
    $ desc "Creates a new keyring; adds it to keyman and subsribes hbs2-peer to listen this lwwref"
    $ returns "string" "lwwref public key"
    $ entry $ bindMatch "hbs2:lwwref:create" $ \case
        [] -> do
          reflog <- keymanNewCredentials (Just "lwwref") 0
          api <- getClientAPI @PeerAPI  @UNIX
          void $ callService @RpcPollAdd api (reflog, "lwwref", 31)
          pure $ mkStr (show $ pretty (AsBase58 reflog))

        _ -> throwIO (BadFormException @C nil)

  brief "lists all lwwref that hbs2-peer is subscribed to"
    $ noArgs
    $ returns "list of string" "lwwref list"
    $ entry $ bindMatch "hbs2:lwwref:list" $ \case
      [] -> do
        api <- getClientAPI  @PeerAPI @UNIX
        r <- callService @RpcPollList2 api (Just "lwwref", Nothing)
               >>= orThrowUser "can't get lwwref list"
        pure $ mkList $ fmap (mkStr . show . pretty . AsBase58 . view _1) r

      _ -> throwIO (BadFormException @C nil)


  brief "fetches lwwref value"
    $ desc "makes peer to request lwwref from neighbors"
    $ args [arg "string" "lwwref"]
    $ returns "atom" "okay"
    $ entry $ bindMatch "hbs2:lwwref:fetch" $ \case
      [StringLike puk] -> do
        lww <- orThrowUser "bad lwwref key" (fromStringMay puk)
        api <- getClientAPI @LWWRefAPI @UNIX
        void $ callService @RpcLWWRefFetch api lww
        pure $ mkStr "okay"

      _ -> throwIO (BadFormException @C nil)


  brief "get lwwref value"
    $ args [arg "string" "lwwref"]
    $ returns "string" "hashref"
    $ examples [qc|

(hbs2:lwwref:get BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP)
(lwwref
  (seq 4)
  (value "74vDGwBYebH3oM6xPXC7kqpgu6deqi7E549QpvHvvQKf")
  )
    |]
    $ entry $ bindMatch "hbs2:lwwref:get" $ \case
      [StringLike puk] -> do

        ref <- orThrowUser "bad lwwref key" (fromStringMay puk)
        api <- getClientAPI @LWWRefAPI  @UNIX
        what <- callService @RpcLWWRefGet api ref
                  >>= orThrowUser "can't get lwwref value"
        pure $ mkStr (show $ pretty what)

      _ -> throwIO (BadFormException @C nil)


  brief "updates lwwref"
    $ desc "updates lwwref value and increments it's counter"
    $ args [arg "string" "lwwref", arg "string" "hash"]
    $ returns "nil" ""
    $ entry $ bindMatch "hbs2:lwwref:update" $ \case
      [StringLike puks, HashLike new] -> do

        puk <- orThrowUser "bad lwwref key" (fromStringMay  puks)
        api <- getClientAPI @LWWRefAPI  @UNIX

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

