{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language PatternSynonyms #-}
module PeerConfig
  ( module PeerConfig
  , module Data.Config.Suckless.Syntax
  , module Data.Config.Suckless.Parse
  ) where

import HBS2.Prelude.Plated
import HBS2.System.Logger.Simple
import HBS2.Base58

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Parse

import Control.Exception
import Data.Either
import Data.Functor
import Data.Kind
import Data.Maybe
import Prettyprinter
import System.Directory
import System.FilePath
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text qualified as Text
import Text.InterpolatedString.Perl6 (qc)

data FeatureSwitch =
  FeatureOn | FeatureOff
  deriving (Eq,Ord,Show,Generic)

class HasCfgKey a b where
  -- type family CfgValue a :: Type
  key :: Id

class HasCfgKey a b => HasCfgValue a b where
  cfgValue :: PeerConfig -> b

type C = MegaParsec

pattern Key :: forall {c}. Id -> [Syntax c] -> [Syntax c]
pattern Key n ns <- SymbolVal  n : ns

data PeerListenTCPKey
data PeerDownloadLogKey
data PeerHttpPortKey
data PeerTcpProbeWaitKey
data PeerUseHttpDownload

instance HasCfgKey PeerListenTCPKey (Maybe String) where
  key = "listen-tcp"

instance HasCfgKey PeerHttpPortKey (Maybe Integer) where
  key = "http-port"

instance HasCfgKey PeerTcpProbeWaitKey (Maybe Integer) where
  key = "tcp-probe-wait"

instance HasCfgKey PeerUseHttpDownload FeatureSwitch where
  key = "http-download"

instance HasCfgKey PeerDownloadLogKey (Maybe String) where
  key = "download-log"

data PeerKnownPeersFile

instance HasCfgKey PeerKnownPeersFile [String] where
  key = "known-peers-file"

cfgName :: FilePath
cfgName = "config"

newtype PeerConfig =
  PeerConfig [Syntax C]
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
rpc    "127.0.0.1:13331"

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
    appendFile (dir</>cfgName) ";; hbs2-peer config file"
    appendFile (dir</>cfgName) defConfigData

peerConfDef :: String
peerConfDef = [qc|
  download-log  "./download-log"
|]

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

    knownPeersFiles <- mapM (canonicalizePath' dir) (cfgValue @PeerKnownPeersFile $ PeerConfig confData')

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

instance {-# OVERLAPPABLE #-} (IsString b, HasCfgKey a (Maybe b)) => HasCfgValue a (Maybe b) where
  cfgValue (PeerConfig syn) = val
    where
      val =
        lastMay [ fromString (show $ pretty e)
                | ListVal @C (Key s [LitStrVal e]) <- syn, s == key @a @(Maybe b)
                ]


instance {-# OVERLAPPABLE #-} (HasCfgKey a (Maybe Integer)) => HasCfgValue a (Maybe Integer) where
  cfgValue (PeerConfig syn) = val
    where
      val =
        lastMay [ e
                | ListVal @C (Key s [LitIntVal e]) <- syn, s == key @a @(Maybe Integer)
                ]

instance (HasCfgKey a FeatureSwitch) => HasCfgValue a FeatureSwitch where
  cfgValue (PeerConfig syn) = val
    where
      val =
        lastDef FeatureOff
                [ FeatureOn
                | ListVal @C (Key s [SymbolVal (Id e)]) <- syn, s == key @a @FeatureSwitch, e == "on"
                ]

instance {-# OVERLAPPABLE #-} (IsString b, HasCfgKey a [b]) => HasCfgValue a [b] where
  cfgValue (PeerConfig syn) = val
    where
      val = [ fromString (show $ pretty e)
            | ListVal @C (Key s [LitStrVal e]) <- syn, s == key @a @[b]
            ]

instance {-# OVERLAPPABLE #-} (Ord b, IsString b, HasCfgKey a (Set b)) => HasCfgValue a (Set b) where
  cfgValue (PeerConfig syn) = Set.fromList val
    where
      val = [ fromString (show $ pretty e)
            | ListVal @C (Key s [LitStrVal e]) <- syn, s == key @a @(Set b)
            ]

