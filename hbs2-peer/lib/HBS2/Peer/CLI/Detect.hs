module HBS2.Peer.CLI.Detect where

import HBS2.Prelude

import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Config.Suckless
import System.Process.Typed
import Data.Text qualified as Text
import Data.Either

detectRPC :: MonadIO m => m (Maybe FilePath)
detectRPC = do

  (_, o, _) <- readProcess (shell "hbs2-peer poke")
  let answ = parseTop (fromString $ LBS.unpack o) & fromRight mempty

  pure (headMay [ Text.unpack r | ListVal (Key "rpc:" [LitStrVal r]) <- answ  ])
