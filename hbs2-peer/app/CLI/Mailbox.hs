{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module CLI.Mailbox (pMailBox) where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.OrDie
import HBS2.Data.Types.Refs
import HBS2.Net.Proto.Service
import HBS2.Net.Auth.Credentials
import HBS2.Storage
import HBS2.Data.Types.SignedBox
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Types

import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.KeyMan.Keys.Direct

import CLI.Common
import RPC2()
import PeerLogger hiding (info)

import Codec.Serialise
import Control.Monad.Trans.Cont
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce
import Data.Config.Suckless.Script
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

        brief "list mailboxes"
          $ entry $ bindMatch "list" $ nil_ $ const do

              let fmtMbox (m,t) = pretty m <+> pretty t

              v <- callRpcWaitMay @RpcMailboxList t api ()
                     >>= orThrowUser "rpc call timeout"

              liftIO $ print $ vcat (fmap fmtMbox v)

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

