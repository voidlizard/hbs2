module HBS2.Storage.NCQ3.Internal.State where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types

import Data.ByteString.Char8 qualified as BS8
import Text.Printf

ncqGetFileName :: NCQStorage3 -> FilePath -> FilePath
ncqGetFileName ncq fp = ncqGetWorkDir ncq </> takeFileName fp

ncqGetWorkDir :: NCQStorage3 -> FilePath
ncqGetWorkDir NCQStorage3{..} = ncqRoot </> show ncqGen

ncqGetLockFileName :: NCQStorage3 -> FilePath
ncqGetLockFileName ncq = ncqGetFileName ncq ".lock"

ncqGetNewFileKey :: forall m . MonadIO m
                    => NCQStorage3
                    -> m FileKey
ncqGetNewFileKey me@NCQStorage3{..} = fix \next -> do
    n <- atomically $ stateTVar ncqStateFileSeq (\x -> (x, succ x))
    let fname = ncqMakeFossilName n
    here <- doesFileExist (ncqGetFileName me fname)
    if here then next else pure n

