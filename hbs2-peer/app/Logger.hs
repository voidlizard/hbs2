module Logger where

import HBS2.Prelude

import System.IO
import Prettyprinter

debug :: (MonadIO m) => Doc ann -> m ()
debug p = liftIO $ hPrint stderr p


