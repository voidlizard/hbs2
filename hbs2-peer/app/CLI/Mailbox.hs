module CLI.Mailbox (pMailBox) where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Net.Proto.Service
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.SignedBox
import HBS2.Peer.Proto.LWWRef

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

        entry $ bindMatch "create" $ nil_ $ const do
          warn "mailbox create is not here yet"
          -- TODO: mailbox-create
          --  - [ ] answer: via RPC or direct
          --  - [ ] answer: peer state or separate database (separate)
          --  - [ ] implement: MailboxWorker
          --  - [ ] implement: interwire MailboxWorker and mailboxProto

        entry $ bindMatch "help" $ nil_ \case
          HelpEntryBound what -> helpEntry what
          [StringLike s]      -> helpList False (Just s)
          _                   -> helpList False Nothing

  flip runContT pure do

    caller <- ContT $ withMyRPC @MailboxAPI rpc
    lift $ run (dict caller) cli >>= eatNil display

  -- withMyRPC @LWWRefAPI rpc $ \caller -> do
  --   callService @RpcLWWRefGet caller ref >>= \case
  --     Left e  -> err (viaShow e) >> exitFailure
  --     Right r -> print $ pretty r


