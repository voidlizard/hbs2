module HBS2.Git3.State.Types
  ( module HBS2.Git3.State.Types
  , pattern SignPubKeyLike
  ) where


import HBS2.Prelude.Plated
import HBS2.Net.Auth.Credentials

import DBPipe.SQLite

type DBRef w = ( Pretty w )

class MonadIO m => HasStateDB m where
  getStateDB :: m DBPipeEnv

