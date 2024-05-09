module Fixme.Scan.Git where

import Fixme.Types

import HBS2.Git.Local


scanForFixmies :: FixmePerks m => Maybe FilePath -> m [Fixme]
scanForFixmies _ = do
  pure  mempty


