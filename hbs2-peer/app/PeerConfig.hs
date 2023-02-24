{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language PatternSynonyms #-}
module PeerConfig
  ( module PeerConfig
  , module Data.Config.Suckless
  ) where

import HBS2.Prelude.Plated
import HBS2.System.Logger.Simple
import HBS2.Base58

import Data.Config.Suckless

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

class HasCfgKey a b where
  -- type family CfgValue a :: Type
  key :: Id

class HasCfgKey a b => HasCfgValue a b where
  cfgValue :: PeerConfig -> b

type C = MegaParsec

pattern Key :: forall {c}. Id -> [Syntax c] -> [Syntax c]
pattern Key n ns <- SymbolVal  n : ns

data PeerDownloadLogKey

instance HasCfgKey PeerDownloadLogKey (Maybe String) where
  key = "download-log"

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

    confData' <- liftIO $ readFile cfgPath <&> parseTop <&> either mempty id

    let confData = confData' <> either mempty id (parseTop  peerConfDef)

    debug $ pretty confData

    config <- flip transformBiM confData $ \case
                List co (Key "key" [LitStrVal p]) -> do
                  kp <- liftIO $ canonicalizePath (dir </> Text.unpack p)
                  pure $ List @C co [Symbol co "key", Literal co (mkLit (Text.pack kp)) ]

                List co (Key "storage" [LitStrVal p]) -> do
                  kp <- liftIO $ canonicalizePath (dir </> Text.unpack p)
                  pure $ List @C co [Symbol co "storage", Literal co (mkLit (Text.pack kp)) ]

                List co (Key "download-log" [LitStrVal p]) -> do
                  kp <- liftIO $ canonicalizePath (dir </> Text.unpack p)
                  pure $ List @C co [Symbol co "download-log", Literal co (mkLit (Text.pack kp)) ]

                x -> pure x

    pure $ PeerConfig config


instance {-# OVERLAPPABLE #-} (IsString b, HasCfgKey a (Maybe b)) => HasCfgValue a (Maybe b) where
  cfgValue (PeerConfig syn) = val
    where
      val =
        lastMay [ fromString (show $ pretty e)
                | ListVal @C (Key s [LitStrVal e]) <- syn, s == key @a @(Maybe b)
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

