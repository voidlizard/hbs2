module HBS2Git.KeysCommand
  ( module HBS2Git.KeysCommand
  , module HBS2.Net.Proto.Types
  , CryptoAction(..)
  ) where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.OrDie
import HBS2.Net.Proto.Types

import HBS2Git.App
import HBS2Git.Encryption
import HBS2.System.Logger.Simple


import Data.Function
import Data.Time.Clock.POSIX
import Data.Maybe


runKeyRefsList :: (MonadIO m, HasConf m) => m ()
runKeyRefsList = do
  conf <- getConf

  now <- liftIO getPOSIXTime

  let every = [ keyInfoRef <$> keyInfoFrom now syn | syn <- conf
              , isJust (keyInfoFrom now syn)
              ] & catMaybes

  liftIO $ print $ vcat (fmap (pretty . AsBase58) every)



runKeysUpdate :: (MonadIO m, HasConf m) => PubKey 'Sign HBS2Basic -> m ()
runKeysUpdate ref = do
  conf <- getConf

  -- TODO: generate-GK0
  --  generate basic key for OWNER only

  now <- liftIO getPOSIXTime
  let every = [ keyInfoFrom now syn | syn <- conf
              , isJust (keyInfoFrom now syn)
              ] & catMaybes

  this <- pure (lastMay [ x | x <- every, keyInfoRef x == ref ])
            `orDie` "Not found encrypted section for given ref"

  gk0 <- generateGroupKey @HBS2Basic Nothing [keyInfoOwner this]

  pure ()

  -- now <- liftIO getPOSIXTime

  -- let every = [ keyInfoFrom now syn | syn <- conf
  --             , isJust (keyInfoFrom now syn)
  --             ] & catMaybes

  -- let keys = [ x | x <- every, keyInfoRef x == ref ]

  -- info $ viaShow keys


runKeysList :: (MonadIO m, HasConf m) => PubKey 'Sign HBS2Basic -> m ()
runKeysList ref = do
  conf <- getConf

  now <- liftIO getPOSIXTime

  let every = [ keyInfoFrom now syn | syn <- conf
              , isJust (keyInfoFrom now syn)
              ] & catMaybes

  let keys = [ x | x <- every, keyInfoRef x == ref ]

  info $ viaShow keys


