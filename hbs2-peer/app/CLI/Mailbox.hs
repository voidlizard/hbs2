{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module CLI.Mailbox (pMailBox) where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Data.Types.Refs
import HBS2.Net.Proto.Service
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.SignedBox
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Types

import HBS2.Peer.RPC.API.Mailbox
import HBS2.KeyMan.Keys.Direct

import CLI.Common
import RPC2()
import PeerLogger hiding (info)

import Data.Config.Suckless.Script

import System.Exit
import System.Environment (lookupEnv)

import Control.Monad.Trans.Cont
import Options.Applicative
import Data.Maybe

import Data.Word
import Lens.Micro.Platform
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

  let dict api = makeDict @C do
        entry $ bindMatch "hey" $ nil_ $ const do
          who <- liftIO (lookupEnv "USER") <&> fromMaybe "stranger"
          liftIO $ print $ "hey," <+> pretty who

        entry $ bindMatch "poke" $ nil_ $ const do
          _ <- callRpcWaitMay @RpcMailboxPoke t api ()
                 >>= orThrowUser "rpc call timeout"

          liftIO $ print $ pretty "okay, rpc is here"

        brief "creates mailbox of given type" $
          desc [qc|
; creates a mailbox using recipient SIGN public key

create --key KEY TYPE

; creates a mailbox using key from a SIGIL with HASH (should stored first)

create --sigil HASH TYPE

; creates a mailbox using key from a SIGIL from FILE

create --sigil-file FILE TYPE

TYPE ::= hub | relay

|] $
          examples [qc|

; create using recipient public key

create --key 3fKeGjaDGBKtNqeNBPsThh8vSj4TPiqaaK7uHbB8MQUV relay

; create using sigil hash

create --sigil ghna99Xtm33ncfdUBT3htBUoEyT16wTZGMdm24BQ1kh relay

; create using sigil file

create --sigil-file ./my.sigil hub

see hbs2-cli for sigil commands (create, store, load, etc)

|]
            $ entry $ bindMatch "create" $ nil_ $ \syn -> do

              case syn of
                [ StringLike "--key", SignPubKeyLike puk, MailboxTypeLike tp ] -> do

                  _ <- callRpcWaitMay @RpcMailboxCreate t api (puk, tp)
                         >>= orThrowUser "rpc call timeout"

                  liftIO $ print $ pretty "done"

                [ StringLike "--sigil", HashLike sh, StringLike tp ] -> do
                  -- TODO: implement-create-by-sigil
                  warn $ "create by sigil (hash)"
                  error "not implemented"

                [ StringLike "--sigil-file", StringLike f, StringLike tp ] -> do
                  -- TODO: implement-create-by-sigil-file
                  warn $ "create by sigil file" <+> pretty f
                  error "not implemented"

                _ -> throwIO $ BadFormException @C nil

        entry $ bindMatch "help" $ nil_ \case
          HelpEntryBound what -> helpEntry what
          [StringLike s]      -> helpList False (Just s)
          _                   -> helpList False Nothing

  flip runContT pure do

    caller <- ContT $ withMyRPC @MailboxAPI rpc
    lift $ run (dict caller) cli >>= eatNil display

