module HBS2.Defaults where

import HBS2.Clock
import Data.String

defMaxDatagram :: Int
defMaxDatagram = 2048

defMaxDatagramRPC :: Int
defMaxDatagramRPC = 4096

defMessageQueueSize :: Integral a => a
defMessageQueueSize = 65536*10

defBurstMax :: Integral a => a
defBurstMax = 128

defBurst :: Integral a => a
defBurst = defBurstMax `div` 4

-- defChunkSize :: Integer
defChunkSize :: Integral a => a
defChunkSize = 1024
-- defChunkSize = 480

defBlockSize :: Integer
defBlockSize =  256 * 1024

defStorePath :: IsString a => a
defStorePath = "hbs2"

defPipelineSize :: Int
defPipelineSize = 16000

defBlockDownloadQ :: Integral a => a
defBlockDownloadQ = 65536*10

defBlockDownloadThreshold :: Integral a => a
defBlockDownloadThreshold = 1

-- typical block hash 530+ chunks * parallel wip blocks amount
defProtoPipelineSize :: Int
defProtoPipelineSize = 65536*2

defCookieTimeoutSec :: Timeout 'Seconds
defCookieTimeoutSec = 7200

defCookieTimeout :: TimeSpec
defCookieTimeout = toTimeSpec defCookieTimeoutSec

defRequestLimit :: TimeSpec
defRequestLimit = toTimeSpec defRequestLimitSec

defBlockSizeCacheTime :: TimeSpec
defBlockSizeCacheTime = toTimeSpec ( 30 :: Timeout 'Seconds )

defRequestLimitSec :: Timeout 'Seconds
defRequestLimitSec = 300

defBlockBanTime :: TimeSpec
defBlockBanTime = toTimeSpec defBlockBanTimeSec

defBlockPostponeTime :: TimeSpec
defBlockPostponeTime = toTimeSpec ( 45 :: Timeout 'Seconds)

defBlockBanTimeSec :: Timeout 'Seconds
defBlockBanTimeSec = 60 :: Timeout 'Seconds

defBlockWipTimeout :: TimeSpec
defBlockWipTimeout = defCookieTimeout

defBlockInfoTimeout :: Timeout 'Seconds
defBlockInfoTimeout = 5

defBlockInfoTimeoutSpec :: TimeSpec
defBlockInfoTimeoutSpec = toTimeSpec defBlockInfoTimeout

-- how much time wait for block from peer?
defBlockWaitMax :: Timeout 'Seconds
defBlockWaitMax = 60 :: Timeout 'Seconds

-- how much time wait for block from peer?
defChunkWaitMax :: Timeout 'Seconds
defChunkWaitMax = 30  :: Timeout 'Seconds

defSweepTimeout :: Timeout 'Seconds
defSweepTimeout = 60 -- FIXME: only for debug!

defPeerAnnounceTime :: Timeout 'Seconds
defPeerAnnounceTime = 120

defPexMaxPeers :: Int
defPexMaxPeers = 50

defDownloadFails :: Int
defDownloadFails = 100

defGetPeerMetaTimeout :: Timeout 'Seconds
defGetPeerMetaTimeout = 10

-- TODO: peer-does-not-have-a-block-ok
--  Это нормально, когда у пира нет блока.
--  У него может не быть каких-то блоков,
--  а какие-то могут быть. Нужно более умный
--  алгоритм, чем бан пира за отсутствие блока.

defUsefulLimit :: Double
defUsefulLimit = 0.25

defInterBlockDelay :: Timeout 'Seconds
defInterBlockDelay = 0.0085


defHashListChunk :: Integral a => a
defHashListChunk = 1024
{-# INLINE defHashListChunk #-}

defTreeChildNum :: Integral a => a
defTreeChildNum = 256
{-# INLINE defTreeChildNum #-}


