module HBS2.CLI.Run.KeyMan where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

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

fixContext :: (IsContext c1, IsContext c2) => Syntax c1 -> Syntax c2
fixContext = go
  where
    go = \case
      List    _ xs -> List noContext (fmap go xs)
      Symbol  _ w  -> Symbol noContext w
      Literal _ l  -> Literal noContext l


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

