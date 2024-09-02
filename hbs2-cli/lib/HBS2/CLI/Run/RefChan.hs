module HBS2.CLI.Run.RefChan
  ( module HBS2.CLI.Run.RefChan
  , keymanNewCredentials
  ) where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.KeyMan

import HBS2.Data.Types.Refs
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.Client.RefChan as Client

import HBS2.Storage.Operations.ByteString

-- import HBS2.Net.Proto
-- import HBS2.Net.Auth.Credentials
-- import HBS2.Base58
-- import HBS2.Defaults
-- import HBS2.Events
-- import HBS2.Peer.Proto.Peer
-- import HBS2.Net.Proto.Sessions
-- import HBS2.Data.Types.Refs
-- import HBS2.Data.Types.SignedBox
-- import HBS2.Storage


import HBS2.Peer.Proto.RefChan
import Data.Either
import HBS2.Base58
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema()
import HBS2.Data.Types.SignedBox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.API.Storage
import HBS2.Storage

import HBS2.KeyMan.Keys.Direct
import HBS2.KeyMan.App.Types

import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Coerce
import Control.Monad.Trans.Cont
import Control.Monad.Except
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as Text
import Codec.Serialise

import Text.InterpolatedString.Perl6 (qc)

refchanEntries :: forall c m . ( IsContext c
                               , MonadUnliftIO m
                               , Exception (BadFormException c)
                               , HasClientAPI RefChanAPI UNIX m
                               , HasClientAPI StorageAPI UNIX m
                               , HasClientAPI PeerAPI UNIX m
                               , HasStorage m
                               ) => MakeDictM  c m ()
refchanEntries = do

  brief "requests all rechans that peer is subcribed to"
    $ args []
    $ returns "list" "list of all refchans"
    $ examples [qc|

(hbs2:refchan:list)
("Atg67E6CPMJWKvR9BvwZTTEjg3Hjz4CYCaEARGANepGP"
 "A5W6jPBjzvdpxaQ2e8xBLYaRZjPXzi4yX7xjC52gTiKk"
 "EjjK7rpgRRJ4yzAhTcwis4XawwagCbmkns8n73ogY3uS")
    |]
    $ entry $ bindMatch "hbs2:refchan:list" $ \case
        [] -> do
          flip runContT pure do
            api <- getClientAPI @PeerAPI @UNIX
            r <- callService @RpcPollList2 api (Just "refchan", Nothing)
                   >>= orThrowUser "can't get refchan list"
            pure $ mkList $ fmap (mkStr . show . pretty . AsBase58 . view _1) r

        _ -> throwIO (BadFormException @c nil)


  brief "reads refchan head block"
    $ args [arg "symbol" "parsed|_", arg "string" "PUBKEY"]
    $ returns "" "string"
    $ examples [qc|

(hbs2:refchan:head:get :parsed ExTZuEy2qVBRshWdSwfxeKcMmQLbK2f5NxRwhvXda9qd)
(version 2)
(quorum 1)
(wait 10)
(peer "5tZfGUoQ79EzFUvyyY5Wh1LzN2oaqhrn9kPnfk6ByHpf" 1)
(peer "35gKUG1mwBTr3tQpjWwR2kBYEnDmHxesoJL5Lj7tMjq3" 1)
(peer "5GnroAC8FXNRL8rcgJj6RTu9mt1AbuNd5MZVnDBcCKzb" 1)
(author "Gu5FxngYYwpRfCUS9DJBGyH3tvtjXFbcZ7CbxmJPWEGH")
(author "ExTZuEy2qVBRshWdSwfxeKcMmQLbK2f5NxRwhvXda9qd")
(reader "5UXrEhYECJ2kEQZZPEf4TisfWsLNdh2nGYQQz8X9ioMv")
(reader "CcRDzezX1XQdPxRMuMKzJkfHFB4yG7vGJeTYvScKkbP8")
; (head-extensions: (count: 0) (size 0))


(hbs2:refchan:head:get :whatever ExTZuEy2qVBRshWdSwfxeKcMmQLbK2f5NxRwhvXda9qd)
HucjFUznHJeA2UYZCdUFHtnE3pTwhCW5Dp7LV3ArZBcr

    |]
    $ entry $ bindMatch "hbs2:refchan:head:get" $ \case
      [StringLike what, SignPubKeyLike puk] -> do

        flip runContT pure do

          callCC $ \exit -> do

            w <- lift (getRefChanHeadHash @UNIX puk)

            hx <- ContT $ maybe1 w (pure nil)

            case what of
              "parsed"  -> do
                  hdblk <- lift (Client.getRefChanHead @UNIX puk)

                  exit $ mkStr (show $ pretty hdblk)

              _ -> exit $ mkStr (show $ pretty $ AsBase58 hx)

            pure nil


      _ -> throwIO (BadFormException @c nil)

  entry $ bindMatch "hbs2:refchan:head:update" $ \case
      [SignPubKeyLike rchan, StringLike headFile] -> do

        sto <- getStorage

        rchanApi <- getClientAPI @RefChanAPI @UNIX

        rch <- liftIO (readFile headFile)
                 <&> fromStringMay @(RefChanHeadBlock L4Proto)
                 >>= orThrowUser "can't parse RefChanHeadBlock"

        creds <- runKeymanClient $ loadCredentials rchan
                       >>= orThrowUser "can't load credentials"

        let box = makeSignedBox @'HBS2Basic (view peerSignPk creds) (view peerSignSk creds) rch

        href <- writeAsMerkle sto  (serialise box)

        callService @RpcRefChanHeadPost rchanApi (HashRef href)
            >>= orThrowUser "can't post refchan head"

        pure nil

      _ -> throwIO (BadFormException @c nil)


  entry $ bindMatch "hbs2:refchan:get" $ \case
    [SignPubKeyLike rchan] -> do

      api <- getClientAPI @RefChanAPI @UNIX

      h <- callService @RpcRefChanGet api rchan
             >>= orThrowUser "can't request refchan head"

      pure $ maybe nil (mkStr . show . pretty . AsBase58) h

    _ -> throwIO (BadFormException @c nil)

  entry $ bindMatch "hbs2:refchan:create" $ \syn -> do

    peerApi  <- getClientAPI @PeerAPI @UNIX
    rchanApi <- getClientAPI @RefChanAPI @UNIX
    sto      <- getStorage

    rch <- case syn of
      [StringLike headFile] -> do
        liftIO (readFile headFile)
           <&> fromStringMay @(RefChanHeadBlock L4Proto)
           >>= orThrowUser "can't parse RefChanHeadBlock"

      [] -> do
       poked <- callService @RpcPoke peerApi ()
                   >>= orThrowUser "can't poke hbs2-peer"
                   <&> parseTop
                   >>= orThrowUser "invalid hbs2-peer attributes"

       ke <- [ x
             | ListVal [SymbolVal "peer-key:", SignPubKeyLike x] <- poked
             ] & headMay & orThrowUser "hbs2-peer key not found"

       let rch0 = refChanHeadDefault @L4Proto
                   & set refChanHeadPeers (HM.singleton ke 1)
                   & set refChanHeadAuthors (HS.singleton ke)

       pure rch0

      _ -> throwIO (BadFormException @c nil)

    refchan <- keymanNewCredentials (Just "refchan") 0

    creds <- runKeymanClient $ loadCredentials refchan
                   >>= orThrowUser "can't load credentials"

    let box = makeSignedBox @'HBS2Basic (view peerSignPk creds) (view peerSignSk creds) rch

    href <- writeAsMerkle sto  (serialise box)

    callService @RpcPollAdd peerApi (refchan, "refchan", 17)
        >>= orThrowUser "can't subscribe to refchan"

    callService @RpcRefChanHeadPost rchanApi (HashRef href)
        >>= orThrowUser "can't post refchan head"

    let r = mkStr @c $ show $ "; refchan " <+> pretty (AsBase58 refchan) <> line
                           <> pretty rch

    pure r

  brief "prints refchan head example"
    $ returns "nil" mempty
    $ entry $ bindMatch "hbs2:refchan:head:example" $ nil_ $ \case
        [] -> flip runContT pure do

          let rch0 = refChanHeadDefault @L4Proto

          api <- getClientAPI @PeerAPI  @UNIX

          pips <- callService @RpcPeers api ()
                    <&> either (const mempty) (HM.fromList . fmap ((,1) . fst) . take 3)


          creds <- replicateM 3 (newCredentialsEnc @HBS2Basic 1)

          let authors = fmap (view peerSignPk) creds
                            & HS.fromList

          let readers = foldMap (view peerKeyring) creds
                            & fmap (view krPk)
                            & take 3
                            & HS.fromList

          let rch = ( set refChanHeadPeers pips
                    . set refChanHeadAuthors authors
                    . set refChanHeadReaders readers
                    . set refChanHeadNotifiers authors
                    ) rch0

          liftIO $ print $
               ";" <+> "this is an example of refchan head block config"
            <> line
            <> ";" <+> "edit it before applying" <> line
            <> ";"  <+> "set up the actual keys / credentials you need" <> line
            <> line <> line
            <> ";" <+> "(version INT) is the head block version" <> line
            <> ";" <+> "the refchan head block will be set only" <>line
            <> ";" <+> "if it's version  if greater than the already existed one" <> line
            <> line

            <> ";" <+> "(quorum INT) is a number of accept messages issued by peers" <> line
            <> ";" <+> "to include propose message to the refchan" <> line
            <> line

            <> ";" <+> "(wait INT) is an quorum  wait time in seconds" <> line
            <> line

            <> ";" <+> "(peer PUBKEY WEIGHT) sets the peer allowed for posting propose/accept messages" <> line
            <> ";" <+> "PUBKEY is a SIGNATURE public key as base58 string" <> line
            <> ";" <+> "only messages from that peers will be accepted" <> line
            <> ";" <+> "WEIGHT is not used yet but reserved for the future" <> line
            <> ";" <+> "this parameter is optional but there is should be some peers or" <> line
            <> ";" <+> "all messages will be sent to nowhere" <> line
            <> line

            <> ";" <+> "(author PUBKEY) adds 'author' i.e. key that is allowed to sign the propose message" <> line
            <> ";" <+> "PUBKEY is a SIGNATURE public key as base58 string" <> line
            <> ";" <+> "only the propose  messages signed by one of thise keys will be accepted" <> line
            <> line

            <> ";" <+> "(notifier PUBKEY) adds 'notifier' i.e. key that is allowed to sign the notify message" <> line
            <> ";" <+> "PUBKEY is a SIGNATURE public key as base58 string" <> line
            <> ";" <+> "only the propose  messages signed by one of thise keys will be accepted" <> line
            <> ";" <+> "notify messages are not written to the refchan merkle tree" <> line
            <> ";" <+> "and they useful for implementing any sort of ephemeral messaging" <> line
            <> ";" <+> "those clauses are OPTIONAL and may be omitted" <> line
            <> line

            <> ";" <+> "(reader PUBKEY) adds 'author' i.e. key that is allowed to decrypt messages" <> line
            <> ";" <+> "PUBKEY is a ENCRYPTION public key as base58 string" <> line
            <> ";" <+> "NOTE: messages in a refchan are not encrypted by default" <> line
            <> ";" <+> "    it's totally up to an application for this refchan" <> line
            <> ";" <+> "    therefore this clause is just used for setting reader keys to" <> line
            <> ";" <+> "    implement any ACL/encrypting mechanism" <> line
            <> ";" <+> "    i.e. groupkey may be inherited from the RefChanHead block" <> line
            <> ";" <+> "    to encrypt data posted to a refchan" <> line
            <> ";" <+> "those clauses are OPTIONAL and may be omitted" <> line
            <> line

            <> pretty rch

        _ -> throwIO (BadFormException @c nil)


  brief "creates RefChanUpdate/AnnotatedHashRef transaction for refchan" $
    args [arg "string" "sign-key", arg "string" "payload-tree-hash"] $
    entry $ bindMatch "hbs2:refchan:tx:annref:create" $ \case
      [SignPubKeyLike signpk, HashLike hash] -> do
         sto <- getStorage
         void $ hasBlock sto (fromHashRef hash) `orDie` "no block found"
         let lbs = AnnotatedHashRef Nothing hash & serialise
         creds  <- runKeymanClient $ loadCredentials signpk >>= orThrowUser "can't find credentials"
         let box = makeSignedBox @HBS2Basic (view peerSignPk creds) (view peerSignSk creds) (LBS.toStrict lbs) & serialise
         pure $ mkForm @c "blob" [mkStr (LBS8.unpack box)]

      _ -> throwIO (BadFormException @c nil)

  brief "posts Propose transaction to the refchan" $
    args [arg "string" "refchan", arg "blob" "signed-box"] $
    entry $ bindMatch "hbs2:refchan:tx:propose" $ nil_ $ \case
      [SignPubKeyLike rchan, ListVal [SymbolVal "blob", LitStrVal box]] -> do
        api <- getClientAPI @RefChanAPI @UNIX
        bbox <- Text.unpack box & LBS8.pack & deserialiseOrFail & orThrowUser "bad transaction"
        void $ callService @RpcRefChanPropose api (rchan, bbox)

      _ -> throwIO (BadFormException @c nil)

