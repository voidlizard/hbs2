{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language PatternSynonyms #-}
module PeerConfig
  ( module PeerConfig
  , module Data.Config.Suckless.Syntax
  , module Data.Config.Suckless.Parse
  , module Data.Config.Suckless.KeyValue
  ) where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.Net.Auth.Credentials

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Parse
import Data.Config.Suckless.KeyValue

import PeerLogger

import Control.Exception
import Control.Monad.Reader
import Data.Maybe
import System.Directory
import System.FilePath
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text qualified as Text
import Text.InterpolatedString.Perl6 (qc)

data FeatureSwitch =
  FeatureOn | FeatureOff
  deriving (Eq,Ord,Show,Generic)

data PeerListenTCPKey
data PeerDownloadLogKey
data PeerHttpPortKey
data PeerTcpProbeWaitKey
data PeerUseHttpDownload
data PeerBrainsDBPath

instance Monad m => HasConf (ReaderT PeerConfig m) where
  getConf = asks (\(PeerConfig syn) -> syn)

instance Monad m => HasCfgKey PeerListenTCPKey (Maybe String) m where
  key = "listen-tcp"

instance Monad m => HasCfgKey PeerHttpPortKey (Maybe Integer) m where
  key = "http-port"

instance Monad m => HasCfgKey PeerTcpProbeWaitKey (Maybe Integer) m where
  key = "tcp-probe-wait"

instance Monad m => HasCfgKey PeerUseHttpDownload b m where
  key = "http-download"

instance Monad m => HasCfgKey PeerBrainsDBPath b m where
  key = "brains-db"

instance Monad m => HasCfgKey PeerDownloadLogKey (Maybe String) m where
  key = "download-log"

data PeerKnownPeersFile

instance Monad m => HasCfgKey PeerKnownPeersFile (Set String) m where
  key = "known-peers-file"


instance {-# OVERLAPPABLE #-} (HasConf m, HasCfgKey a b m) => HasCfgValue a FeatureSwitch m where
  cfgValue = lastDef FeatureOff . val <$> getConf
    where
      val syn = [ if e == "on" then FeatureOn else FeatureOff
                | ListVal (Key s [SymbolVal e]) <- syn, s == key @a @b @m
                ]

cfgName :: FilePath
cfgName = "config"

newtype PeerConfig =
  PeerConfig { fromPeerConfig :: [Syntax C] }
  deriving newtype (Monoid, Semigroup, Pretty)


peerConfigDefault :: MonadIO m => m FilePath
peerConfigDefault = liftIO $
  catchAny (getXdgDirectory XdgConfig "hbs2-peer" <&> (</> cfgName))
           (const $ pure ".hbs2-peer.conf")

  where
    catchAny :: IO a -> (SomeException -> IO a) -> IO a
    catchAny = Control.Exception.catch

peerStateDirDefault :: MonadIO m => m FilePath
peerStateDirDefault = liftIO $ getXdgDirectory XdgData "hbs2-peer"

defConfigData :: String
defConfigData = [qc|

listen "0.0.0.0:7351"
listen-tcp "0.0.0.0:10351"

; default storage is $HOME/.local/share/hbs2
; storage "./storage"

; edit path to a keyring file
; key    "./key"
|]

peerConfigInit :: MonadIO m => Maybe FilePath -> m ()
peerConfigInit mbfp = liftIO do

  debug $ "peerConfigInit" <+> pretty mbfp

  defDir <- peerConfigDefault <&> takeDirectory

  let dir = fromMaybe defDir mbfp

  createDirectoryIfMissing True dir

  let conf = dir </> cfgName

  here <- liftIO $ doesFileExist conf

  unless here do
    let cfgPath = dir</>cfgName
    appendFile cfgPath ";; hbs2-peer config file"
    appendFile cfgPath defConfigData

    cred0 <- newCredentials @'HBS2Basic
    let keyname = "default.key"
    let keypath = dir</>keyname

    khere <- doesFileExist keypath

    unless khere do
      writeFile keypath (show $ pretty (AsCredFile $ AsBase58 cred0))
      appendFile cfgPath [qc|key "./default.key"|]
      appendFile cfgPath ""

peerConfDef :: String
peerConfDef = [qc|
|]

rpcSoDef :: FilePath
rpcSoDef = "/tmp/hbs2-rpc.socket"

getRpcSocketNameM :: HasConf m => m FilePath
getRpcSocketNameM = do
  syn <- getConf

  let soname = lastDef rpcSoDef [ Text.unpack n
                                | ListVal (Key "rpc" [SymbolVal "unix", LitStrVal n]) <- syn
                                ]
  pure soname

getRpcSocketName :: PeerConfig -> FilePath
getRpcSocketName = runReader getRpcSocketNameM

peerConfigRead :: MonadIO m => Maybe FilePath -> m PeerConfig
peerConfigRead mbfp = do

  peerConfigInit mbfp

  debug $ "peerConfigRead" <+> pretty mbfp

  xdg <- peerConfigDefault

  let cfgPath = maybe xdg (</> cfgName) mbfp
  let dir = takeDirectory cfgPath

  debug $ "searching config" <+> pretty cfgPath

  here <- liftIO $ doesFileExist cfgPath

  if not here then do
    debug "no config found"
    pure mempty

  else do

    -- FIXME: config-parse-error-handling
    --  Handle parse errors

    debug $ pretty cfgPath

    let parseConf f = liftIO $ readFile f <&> parseTop <&> either mempty id

    confData' <- parseConf cfgPath

    knownPeersFiles <- flip runReaderT confData' $ (Set.toList <$> cfgValue @PeerKnownPeersFile)
                         >>= mapM (canonicalizePath' dir)

    knownPeersConfData <- concat <$> mapM parseConf knownPeersFiles

    let confData = confData' <> either mempty id (parseTop  peerConfDef) <> knownPeersConfData

    -- debug $ pretty confData

    config <- transformBiM (canonicalizeConfPaths ["key", "storage", "download-log", "state-dir"] dir) confData

    pure $ PeerConfig config
    where
      canonicalizePath' :: MonadIO m => FilePath -> FilePath -> m FilePath
      canonicalizePath' dir = liftIO . canonicalizePath . (dir </>)

      canonicalizeConfPaths :: MonadIO m => [Id] -> FilePath -> Syntax C -> m (Syntax C)
      canonicalizeConfPaths keys dir x@(List co (Key k [LitStrVal path])) =
        if k `elem` keys
          then do
            canonicalPath <- canonicalizePath' dir $ Text.unpack path
            pure $ List @C co [Symbol co k, Literal co (mkLit (Text.pack canonicalPath))]
          else pure x
      canonicalizeConfPaths _ _ x = pure x

