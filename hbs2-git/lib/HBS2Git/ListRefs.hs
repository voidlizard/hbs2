module HBS2Git.ListRefs where

import HBS2Git.Types
import HBS2.Prelude
import HBS2Git.App
import HBS2.Data.Types.Refs (HashRef)

import HBS2.Git.Local.CLI

import Data.Functor
import Data.Text qualified as Text
import Data.Traversable
import Prettyprinter.Render.Terminal

data AsRemoteEntry = AsRemoteEntry
  { remoteName :: Text,
    remoteURL :: Text,
    remoteRefValue :: Maybe HashRef
  }

remoteNameColWidth :: Int
remoteNameColWidth = 16

remoteURLColWidth :: Int
remoteURLColWidth = 51

remoteRefValueColWidth :: Int
remoteRefValueColWidth = 44

instance Pretty AsRemoteEntry where
  pretty (AsRemoteEntry {..}) =
    fill remoteNameColWidth (pretty remoteName)
      <+> fill remoteURLColWidth (pretty remoteURL)
      <+> fill remoteRefValueColWidth (maybe "-" pretty remoteRefValue)

hbs2Prefix :: Text
hbs2Prefix = "hbs2://"

-- TODO: backlog-list-refs-all-option
--  сделать опцию --all которая выведет
--  все известные ref-ы из стейта.
--  Сейчас выводятся только локальные

runListRefs :: MonadIO m => App m ()
runListRefs = do
  refs <- gitGetRemotes <&> filter isHbs2
  remoteEntries <-
    forM
      refs
      ( \(name, url) -> do
          refVal <- getRefVal url
          pure $
            AsRemoteEntry
              { remoteName = name,
                remoteURL = url,
                remoteRefValue = refVal
              }
      )
  let header =
        fill remoteNameColWidth (green "Name")
          <+> fill remoteURLColWidth (green "URL")
          <+> fill remoteRefValueColWidth (green "Reference value")
  liftIO $ putDoc $ header <> line
  liftIO $ putDoc $ vcat $ pretty <$> remoteEntries
  where
    isHbs2 (_, b) = Text.isPrefixOf hbs2Prefix b

getRefVal :: (MonadIO m, HasCatAPI m) => Text -> m (Maybe HashRef)
getRefVal url =
  case Text.stripPrefix hbs2Prefix url of
    Nothing -> do
      liftIO $ print $ pretty "wrong URL format" <+> pretty url
      pure Nothing
    Just refStr -> case fromStringMay $ Text.unpack refStr of
      Nothing -> do
        liftIO $ print $ pretty "can't parse ref" <+> pretty refStr
        pure Nothing
      Just ref -> do
        mRefVal <- readRefHttp ref
        case mRefVal of
          Nothing -> do
            liftIO $ print $ pretty "readRefHttp error" <+> pretty ref
            pure Nothing
          Just v -> pure $ Just v
