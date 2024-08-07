module HBS2.CLI.Run.KeyMan
  (keymanEntries) where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.KeyMan

import HBS2.Hash
import HBS2.System.Dir

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


keymanEntries :: (MonadUnliftIO m, IsContext c) => MakeDictM c m ()
keymanEntries = do
  entry $ bindMatch "hbs2:keyman:list" $ nil_ \case
    _ -> do
      void $ runKeymanClient  $ KeyManClient $ do
        k <- listKeys
        display_ $ vcat (fmap pretty k)

  entry $ bindMatch "hbs2:keyman:update" $ nil_ $ \_ -> do
    keymanUpdate

  entry $ bindMatch "hbs2:keyman:config" $ \_ -> do
    mkForm  "dict" <$> keymanGetConfig

  entry $ bindMatch "hbs2:keyman:keys:add" $ \case
    [ LitStrVal ke ] -> do
      conf <- keymanGetConfig @C
      let path = head [ s | ListVal [ SymbolVal "default-key-path", StringLike s ] <- conf ]
      mkdir path
      let n =  hashObject @HbSync (serialise ke) & pretty & show
      let fname = n `addExtension` ".key"
      let fpath = path </> fname
      liftIO $ TIO.writeFile fpath ke
      keymanUpdate
      pure $ mkStr fpath

    _ -> throwIO (BadFormException @C nil)

