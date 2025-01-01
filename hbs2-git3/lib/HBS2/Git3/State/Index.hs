module HBS2.Git3.State.Index where

import HBS2.Git3.Prelude
import HBS2.System.Dir
import HBS2.CLI.Run.Internal.Merkle (getTreeContents)
import HBS2.Git3.State.Types

import HBS2.Data.Log.Structured

import Data.List qualified as L
import Network.ByteOrder qualified as N
import System.IO.Temp as Temp
import Data.ByteString.Lazy qualified as LBS

import Codec.Compression.Zstd.Lazy qualified as ZstdL
import Streaming.Prelude qualified as S
import Streaming hiding (run,chunksOf)

import UnliftIO
import UnliftIO.IO.File qualified as UIO


-- writeReflogIndex = do

--     reflog <- getGitRemoteKey >>= orThrowUser "reflog not set"

--     api <- getClientAPI @RefLogAPI @UNIX

--     sto <- getStorage

--     flip runContT pure do

--       what' <- lift $ callRpcWaitMay @RpcRefLogGet (TimeoutSec 2) api reflog
--                 >>= orThrowUser "rpc timeout"

--       what <- ContT $ maybe1 what' none

--       idxPath <- getStatePath (AsBase58 reflog) <&> (</> "index")
--       mkdir idxPath

--       notice $ "STATE" <+> pretty idxPath

--       sink <- S.toList_ do
--         walkMerkle (coerce what) (getBlock sto) $ \case
--           Left{} -> throwIO MissedBlockError
--           Right (hs :: [HashRef]) -> do
--             for_ hs $ \h -> void $ runMaybeT do

--               tx <- getBlock sto (coerce h)
--                        >>= toMPlus

--               RefLogUpdate{..} <- deserialiseOrFail @(RefLogUpdate L4Proto) tx
--                                     & toMPlus

--               AnnotatedHashRef _ href <- deserialiseOrFail @AnnotatedHashRef (LBS.fromStrict _refLogUpdData)
--                                            & toMPlus

--               -- FIXME: error logging
--               lbs <- liftIO (runExceptT (getTreeContents sto href))
--                        >>= orThrow MissedBlockError

--               pieces <- S.toList_ do
--                 void $ runConsumeLBS (ZstdL.decompress lbs) $ readLogFileLBS () $ \o s _ -> do
--                   lift $ S.yield o

--               lift $ S.yield (h, pieces)

--       liftIO $ forConcurrently_ sink $ \(tx, pieces) -> do
--         idxName <- emptyTempFile idxPath "objects-.idx"
--         let ss = L.sort pieces
--         UIO.withBinaryFileAtomic idxName WriteMode $ \wh -> do
--           for_ ss $ \sha1 -> do
--             let key   = coerce @_ @N.ByteString sha1
--             let value = coerce @_ @N.ByteString tx
--             -- notice $ pretty sha1 <+> pretty tx
--             writeSection ( LBS.fromChunks [key,value] ) (LBS.hPutStr wh)

