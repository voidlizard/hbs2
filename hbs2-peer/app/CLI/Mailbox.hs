{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module CLI.Mailbox (pMailBox) where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.Hash
import HBS2.OrDie
import HBS2.Merkle
import HBS2.Data.Types.Refs
import HBS2.Net.Proto.Service
import HBS2.Net.Auth.Credentials
import HBS2.Storage
import HBS2.Storage.Operations.ByteString
import HBS2.Data.Types.SignedBox
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Peer.Proto.Mailbox.Entry

import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.KeyMan.Keys.Direct

import CLI.Common
import RPC2()
import PeerLogger hiding (info)

import Codec.Serialise
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString qualified as BS
import Data.Either
import Data.Coerce
import Data.Config.Suckless.Script
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe
import Data.Word
import Lens.Micro.Platform
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit
import UnliftIO

import Text.InterpolatedString.Perl6 (qc)

pattern MailboxTypeLike :: forall {c}. MailboxType -> Syntax c
pattern MailboxTypeLike w <- (mailboxTypeLike -> Just w)

mailboxTypeLike :: Syntax c -> Maybe MailboxType
mailboxTypeLike = \case
  StringLike s -> fromStringMay @MailboxType s
  _            -> Nothing

pMailBox :: Parser (IO ())
pMailBox = do
 rpc <- pRpcCommon
 what <- many (strArgument (metavar "ARGS" <> help "hbs2-cli mailbox command-line"))
 pure (runMailboxCLI rpc what)



runMailboxCLI :: RPCOpt -> [String] -> IO ()
runMailboxCLI rpc s = do

  cli <- parseTop (unwords s) & either (error.show) pure

  let t = TimeoutSec 1

  let dict sto api = makeDict @C do
        entry $ bindMatch "hey" $ nil_ $ const do
          who <- liftIO (lookupEnv "USER") <&> fromMaybe "stranger"
          liftIO $ print $ "hey," <+> pretty who

        entry $ bindMatch "poke" $ nil_ $ const do
          _ <- callRpcWaitMay @RpcMailboxPoke t api ()
                 >>= orThrowUser "rpc call timeout"

          liftIO $ print $ pretty "okay, rpc is here"

        brief "creates mailbox of given type" $
          desc createMailBoxDesc $
          examples createMailBoxExamples
            $ entry $ bindMatch "create" $ nil_ $ \syn -> do

              case syn of
                [ StringLike "--key", SignPubKeyLike puk, MailboxTypeLike tp ] -> do

                  r <- callRpcWaitMay @RpcMailboxCreate t api (puk, tp)
                         >>= orThrowUser "rpc call timeout"

                  liftIO $ print $ viaShow r

                [ StringLike "--sigil", HashLike sh, StringLike tp ] -> do
                  -- TODO: implement-create-by-sigil
                  warn $ "create by sigil (hash)"
                  error "not implemented"

                [ StringLike "--sigil-file", StringLike f, StringLike tp ] -> do
                  -- TODO: implement-create-by-sigil-file
                  warn $ "create by sigil file" <+> pretty f
                  error "not implemented"

                _ -> throwIO $ BadFormException @C nil

        brief "send message via gossip"  $
          desc sendMessageDesc
          $ entry $ bindMatch "send" $ nil_ $ \syn -> do

              blob <- case syn of
                        [ StringLike "--stdin" ] -> do
                          liftIO (LBS.hGetContents stdin)

                        [ StringLike "--file", StringLike fn ] -> do
                          liftIO (LBS.readFile fn)

                        [ HashLike h ] -> do
                           liftIO (getBlock sto (coerce h))
                               >>= orThrowUser "message not found"

                        _ -> throwIO $ BadFormException @C nil

              mess <- deserialiseOrFail @(Message HBS2Basic) blob
                        & either (const $ error "malformed message") pure

              _ <- callRpcWaitMay @RpcMailboxSend t api mess
                     >>= orThrowUser "rpc call timeout"

              pure ()

        brief "get mailbox value"
          $ entry $ bindMatch "get" $ nil_ $ \case
             [ SignPubKeyLike m ] -> do

              v <- callRpcWaitMay @RpcMailboxGet t api m
                     >>= orThrowUser "rpc call timeout"

              liftIO $ print $ pretty v

             _ -> throwIO $ BadFormException @C nil

        brief "get mailbox status"
          $ entry $ bindMatch "status" $ nil_ $ \case
             [ SignPubKeyLike m ] -> do

              v <- callRpcWaitMay @RpcMailboxGetStatus t api m
                     >>= orThrowUser "rpc call timeout"
                     >>= orThrowPassIO

              liftIO $ print $ pretty v

             _ -> throwIO $ BadFormException @C nil


        brief "fetch mailbox"
          $ entry $ bindMatch "fetch" $ nil_ $ \case
             [ SignPubKeyLike m ] -> do

              callRpcWaitMay @RpcMailboxFetch t api m
                 >>= orThrowUser "rpc call timeout"
                 >>= orThrowPassIO

             _ -> throwIO $ BadFormException @C nil

        brief "set mailbox policy" $
          desc setPolicyDesc
          -- $ examples setPolicyExamples
          $ entry $ bindMatch "set-policy" $ nil_ $ \case
             [ SignPubKeyLike m, LitIntVal v, StringLike fn ] -> lift do

                mstatus <- callRpcWaitMay @RpcMailboxGetStatus t api m
                             >>= orThrowUser "rpc call timeout"
                             >>= orThrowPassIO

                s <- liftIO $ readFile fn
                      <&> parseTop
                      >>= either (error . show) pure

                pv <- fromMaybe 0 <$> runMaybeT do
                        MailBoxStatusPayload{..} <- toMPlus mstatus
                        pbox <- toMPlus mbsMailboxPolicy
                        (who, SetPolicyPayload{..}) <- unboxSignedBox0 pbox & toMPlus

                        guard ( m == who )

                        pure sppPolicyVersion

                -- TODO: validate-policy

                creds <- runKeymanClientRO (loadCredentials m)
                           >>= orThrowUser ("can't load credentials for" <+> pretty (AsBase58 m))

                let normalized = show $ vcat (fmap pretty s)

                notice $ "policy" <> line <> pretty normalized

                notice $ "okay" <+>  pretty pv <+> "->" <+> pretty v <+> pretty fn

                hash <-  writeAsMerkle sto (LBS8.pack normalized)

                notice $ "stored policy as" <+> pretty hash

                let spp = SetPolicyPayload @HBS2Basic m (fromIntegral v) (HashRef hash)

                let box = makeSignedBox @HBS2Basic (view peerSignPk creds) (view peerSignSk creds) spp

                notice $ "signed policy payload done okay"

                r <- callRpcWaitMay @RpcMailboxSetPolicy t api (m,box)
                       >>= orThrowUser "rpc call timeout"
                       >>= orThrowPassIO

                liftIO $ print $ pretty r

             _ -> throwIO $ BadFormException @C nil


        brief "list mailboxes"
          $ entry $ bindMatch "list" $ nil_ $ const do

              let fmtMbox (m,t) = pretty m <+> pretty t

              v <- callRpcWaitMay @RpcMailboxList t api ()
                     >>= orThrowUser "rpc call timeout"

              liftIO $ print $ vcat (fmap fmtMbox v)

        brief "read message"
         $ desc [qc|;; reads message
  read HASH
|]
         $ entry $ bindMatch "read" $ nil_ $ \case
            [ HashLike mhash ] -> do

              let rms = ReadMessageServices ( liftIO . runKeymanClientRO . extractGroupKeySecret)

              (s,_,bs) <- getBlock sto (coerce mhash)
                            >>= orThrowUser "message not found"
                              <&> deserialiseOrFail @(Message HBS2Basic)
                              >>= orThrowUser "invalid message format"
                              >>= readMessage rms

              liftIO $ BS.putStr bs

              none

            _ -> throwIO $ BadFormException @C nil

        brief "delete message"
          $ desc deleteMessageDesc
          $ entry $ bindMatch "delete:message" $ nil_ $ \case
            [ SignPubKeyLike ref, HashLike mess ] -> do

               creds <- runKeymanClientRO (loadCredentials ref)
                         >>= orThrowUser ("can't load credentials for" <+> pretty (AsBase58 ref))

               let expr  = MailboxMessagePredicate1 (Op (MessageHashEq mess))
               let messP = DeleteMessagesPayload @HBS2Basic expr

               let box = makeSignedBox @HBS2Basic (view peerSignPk creds) (view peerSignSk creds) messP

               callRpcWaitMay @RpcMailboxDeleteMessages t api box
                  >>= orThrowUser "rpc call timeout"
                  >>= orThrowPassIO

            _ -> throwIO $ BadFormException @C nil

        brief "list messages"
          $ entry $ bindMatch "list:messages" $ nil_ $ \case
             [ SignPubKeyLike m ] -> void $ runMaybeT do

              v <- lift (callRpcWaitMay @RpcMailboxGet t api m)
                     >>= orThrowUser "rpc call timeout"
                     >>= toMPlus

              d <- liftIO $ newTVarIO (mempty :: HashSet HashRef)
              r <- liftIO $ newTVarIO (mempty :: HashSet HashRef)

              walkMerkle @[HashRef] (coerce v) (liftIO . getBlock sto) $ \case
                Left what -> err $ "missed block for tree" <+> pretty v <+> pretty what
                Right hs  -> void $ runMaybeT do
                  for_ hs $ \h -> do

                    -- TODO: better-error-handling
                    e <- getBlock sto (coerce h)
                             >>= toMPlus
                             <&> deserialiseOrFail @MailboxEntry
                             >>= toMPlus

                    case e of
                      Deleted _ mh -> do
                        atomically $ modifyTVar d (HS.insert mh)

                      Exists _ mh -> do
                         atomically $ modifyTVar r (HS.insert mh)

              deleted <- readTVarIO d
              rest    <- readTVarIO r

              for_ (HS.difference rest deleted) $ \mh -> do
                liftIO $ print $ pretty mh

             _ -> throwIO $ BadFormException @C nil


        brief "delete mailbox"
          $ entry $ bindMatch "delete" $ nil_ $ \case
            [ SignPubKeyLike mbox ]-> lift do
              callRpcWaitMay @RpcMailboxDelete t api mbox
                 >>= orThrowUser "rpc call timeout"

            _ -> throwIO $ BadFormException @C nil

        entry $ bindMatch "help" $ nil_ \case
          HelpEntryBound what -> helpEntry what
          [StringLike s]      -> helpList False (Just s)
          _                   -> helpList False Nothing

  flip runContT pure do

    caller <- ContT $ withMyRPC @MailboxAPI rpc
    stoAPI <- ContT $ withMyRPC @StorageAPI rpc
    let sto = AnyStorage (StorageClient stoAPI)
    lift $ run (dict sto caller) cli >>= eatNil display


-- man entries

createMailBoxDesc :: Doc a
createMailBoxDesc = [qc|
; creates a mailbox using recipient SIGN public key

create --key KEY TYPE

; creates a mailbox using key from a SIGIL with HASH (should stored first)

create --sigil HASH TYPE

; creates a mailbox using key from a SIGIL from FILE

create --sigil-file FILE TYPE

TYPE ::= hub | relay

|]

createMailBoxExamples :: ManExamples
createMailBoxExamples = [qc|
; create using recipient public key

create --key 3fKeGjaDGBKtNqeNBPsThh8vSj4TPiqaaK7uHbB8MQUV relay

; create using sigil hash

create --sigil ghna99Xtm33ncfdUBT3htBUoEyT16wTZGMdm24BQ1kh relay

; create using sigil file

create --sigil-file ./my.sigil hub

see hbs2-cli for sigil commands (create, store, load, etc)

|]


sendMessageDesc :: Doc a
sendMessageDesc = [qc|
; reads message blob from stdin

send --stdin

; read message blob from file

send --file FILE

; reads message blob from storage

send HASH

you may create a message from plain text using

hbs2-cli hbs2:mailbox:message:create

command

SEE ALSO
  hbs2:mailbox:message:create

|]


setPolicyDesc :: Doc a
setPolicyDesc = [qc|
  set-policy (MAILBOX-KEY :: PUBKEY) (VERSION :: INT) FILENAME
|]

setPolicyExamples :: ManExamples
setPolicyExamples = mempty

deleteMessageDesc :: Doc a
deleteMessageDesc = [qc|

;; deletes message from mailbox
  delete:message MAILBOX-KEY MESSAGE-HASH

|]


