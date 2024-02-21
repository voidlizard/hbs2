module PeerLogger
  ( module Logger
  , trace1, TRACE1
  ) where

import HBS2.Prelude.Plated
import HBS2.System.Logger.Simple.ANSI as Logger

data TRACE1

instance HasLogLevel TRACE1 where
  type instance LogLevel TRACE1 = 101

trace1 :: forall a m . (MonadIO m, ToLogStr a) => a -> m ()
trace1 = Logger.writeLog @TRACE1
