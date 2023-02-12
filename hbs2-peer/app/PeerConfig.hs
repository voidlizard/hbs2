module PeerConfig where

import HBS2.Prelude
import HBS2.System.Logger.Simple

import Data.Config.Suckless

import Control.Exception
import Data.Maybe
import Data.Functor
import System.Directory
import Prettyprinter
import System.FilePath

type C = MegaParsec

cfgName :: FilePath
cfgName = "config"

newtype PeerConfig =
  PeerConfig [Syntax C]
  deriving newtype (Monoid, Semigroup)


peerConfigDefault :: MonadIO m => m FilePath
peerConfigDefault = liftIO $
  catchAny (getXdgDirectory XdgConfig "hbs2-peer" <&> (</> cfgName))
           (const $ pure ".hbs2-peer.conf")

  where
    catchAny :: IO a -> (SomeException -> IO a) -> IO a
    catchAny = Control.Exception.catch


peerConfigInit :: Maybe FilePath -> IO ()
peerConfigInit mbfp = do
  debug $ "peerConfigInit" <+> pretty mbfp

  defDir <- peerConfigDefault <&> takeDirectory

  let dir = fromMaybe defDir mbfp

  createDirectoryIfMissing True dir

  let conf = dir </> cfgName

  here <- liftIO $ doesFileExist conf

  unless here do
    appendFile (dir</>cfgName) ";; hbs2-peer config file"

peerConfigRead :: MonadIO m => Maybe FilePath -> m PeerConfig
peerConfigRead mbfp = do
  debug $ "peerConfigRead" <+> pretty mbfp

  xdg <- peerConfigDefault

  let cfgPath = fromMaybe xdg mbfp </> cfgName

  here <- liftIO $ doesFileExist cfgPath

  unless here do
    debug "no config found"

  pure mempty



