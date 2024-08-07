module HBS2.CLI.Run.Keyring where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Net.Auth.Credentials
import HBS2.KeyMan.App.Types

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as Text

keyringEntries :: forall c m . ( MonadUnliftIO m
                               , IsContext c
                               , Exception (BadFormException c)
                               ) => MakeDictM c m ()
keyringEntries = do
  entry $ bindMatch "hbs2:keyring:list-encryption" $ \syn -> do
    lbs <- case syn of

          [ ListVal [ SymbolVal "file", StringLike fn ] ] -> do
            liftIO $ BS.readFile fn

          [ LitStrVal s ] -> do
            pure (BS8.pack (Text.unpack s))

          _ -> throwIO (BadFormException @C nil)

    cred <- pure (parseCredentials @'HBS2Basic (AsCredFile lbs))
              `orDie` "bad keyring file"

    let e = [ mkStr @c (show (pretty (AsBase58 p))) | KeyringEntry p _ _ <- view peerKeyring cred ]

    pure $ mkList @c e

  brief "creates a new keyring (credentials)"
    $ args [arg "int?" "encrypt-keys-num"]
    $ returns "keyring" "string"
    $ entry $ bindMatch "hbs2:keyring:new" $ \syn -> do
      n <- case syn of
            [LitIntVal k] -> pure k
            []            -> pure 1
            _ -> throwIO (BadFormException @C nil)

      cred0 <- newCredentials @'HBS2Basic
      cred <- foldM (\cred _ -> addKeyPair Nothing cred) cred0 [1..n]
      pure $ mkStr @c $ show $ pretty $ AsCredFile $ AsBase58 cred


  entry $ bindMatch "hbs2:keyring:show" $ \case
    [StringLike fn] -> do
      bs <- liftIO $ BS.readFile fn
      cred <- parseCredentials @'HBS2Basic (AsCredFile bs)
                  & orThrowUser "bad credentials file"

      pure $ mkStr $ show $ pretty (ListKeyringKeys cred)

    _ -> throwIO $ BadFormException @c nil


