module HBS2.CLI.Run.RefChan where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.KeyMan

import HBS2.Data.Types.Refs
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefChan

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

import HBS2.KeyMan.Keys.Direct
import HBS2.KeyMan.App.Types

import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Coerce
import Control.Monad.Trans.Cont
import Control.Monad.Except

import Text.InterpolatedString.Perl6 (qc)

refchanEntries :: forall c m . (c ~ C, IsContext c, MonadUnliftIO m) => MakeDictM  c m ()
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
            so <- detectRPC `orDie` "rpc not found"
            api <- ContT $ withRPC2 @PeerAPI @UNIX so
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

            so <- detectRPC `orDie` "rpc not found"
            api <- ContT $ withRPC2 @RefChanAPI @UNIX so
            sto <- ContT $ withPeerStorage

            w <- callService @RpcRefChanHeadGet api puk
                    >>= orThrowUser "can't get refchan head"

            hx <- ContT $ maybe1 w (pure nil)

            case what of
              "parsed"  -> do

                  lbs <- runExceptT (readFromMerkle sto (SimpleKey (coerce hx)))
                            >>= orThrowUser "can't decode refchan head "

                  (_, hdblk) <- unboxSignedBox @(RefChanHeadBlock L4Proto) @'HBS2Basic lbs
                                   & orThrowUser "can't unbox signed box"

                  exit $ mkStr (show $ pretty hdblk)

              _ -> exit $ mkStr (show $ pretty $ AsBase58 hx)

            pure nil


      _ -> throwIO (BadFormException @c nil)

  brief "prints refchan head example"
    $ returns "nil" mempty
    $ entry $ bindMatch "hbs2:refchan:head:example" $ nil_ $ \case
        [] -> flip runContT pure do

          let rch0 = refChanHeadDefault @L4Proto

          so <- detectRPC
                   >>= orThrowUser "hbs2-peer not found"

          api <- ContT $ withRPC2 @PeerAPI  @UNIX so

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

        _ -> throwIO (BadFormException @C nil)

