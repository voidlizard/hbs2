module HBS2.CLI.Run.Internal.KeyMan where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Hash
import HBS2.System.Dir
import HBS2.Net.Auth.Credentials

import HBS2.KeyMan.Keys.Direct
import HBS2.KeyMan.State
import HBS2.KeyMan.App.Types

import Codec.Serialise
import Data.Either
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)



keymanGetConfig :: (IsContext c, MonadUnliftIO m) => m [Syntax c]
keymanGetConfig = do
    (_,lbs,_) <- readProcess (shell [qc|hbs2-keyman config|] & setStderr closed)

    let conf = TE.decodeUtf8 (LBS.toStrict lbs)
                 & parseTop
                 & fromRight mempty

    pure $ fmap fixContext conf

keymanUpdate :: MonadUnliftIO m => m ()
keymanUpdate = do
  void $ runProcess (shell [qc|hbs2-keyman update|])

keymanNewCredentials :: MonadUnliftIO m => Maybe String -> Int -> m (PubKey 'Sign 'HBS2Basic)
keymanNewCredentials suff n = do
  conf <- keymanGetConfig @C

  path <- [ p
          | ListVal [SymbolVal "default-key-path", StringLike p] <- conf
          ] & headMay & orThrowUser "default-key-path not set"

  creds <- newCredentialsEnc @'HBS2Basic n

  let s = show $ pretty $ AsCredFile (AsBase58 creds)

  let psk = view peerSignPk creds

  let fpath = path </> show (pretty (AsBase58 psk) <> "-" <> pretty suff <> ".key")

  liftIO $ writeFile fpath s

  keymanUpdate

  pure psk

