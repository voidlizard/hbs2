module HBS2.Defaults where

import HBS2.Clock
import Data.String

defMaxDatagram :: Int
defMaxDatagram = 2048

defMaxDatagramRPC :: Int
defMaxDatagramRPC = 4096

defMessageQueueSize :: Integral a => a
defMessageQueueSize = 65536

defBurst :: Integral a => a
defBurst = 4

defBurstMax :: Integral a => a
defBurstMax = 128

-- defChunkSize :: Integer
defChunkSize :: Integral a => a
defChunkSize = 1200

defBlockSize :: Integer
defBlockSize =  256 * 1024

defStorePath :: IsString a => a
defStorePath = "hbs2"

defPipelineSize :: Int
defPipelineSize = 16000

defBlockDownloadQ :: Integral a => a
defBlockDownloadQ = 65536

defBlockDownloadThreshold :: Integral a => a
defBlockDownloadThreshold = 2

-- typical block hash 530+ chunks * parallel wip blocks amount
defProtoPipelineSize :: Int
defProtoPipelineSize = 65536*2

defCookieTimeoutSec :: Timeout 'Seconds
defCookieTimeoutSec = 1200

defCookieTimeout :: TimeSpec
defCookieTimeout = toTimeSpec defCookieTimeoutSec

defRequestLimit :: TimeSpec
defRequestLimit = toTimeSpec defRequestLimitSec

defRequestLimitSec :: Timeout 'Seconds
defRequestLimitSec = 60

defBlockWipTimeout :: TimeSpec
defBlockWipTimeout = toTimeSpec defCookieTimeoutSec

defBlockInfoTimeout :: Timeout 'Seconds
defBlockInfoTimeout = 2

-- how much time wait for block from peer?
defBlockWaitMax :: Timeout 'Seconds
defBlockWaitMax = 2.5 :: Timeout 'Seconds

-- how much time wait for block from peer?
defChunkWaitMax :: Timeout 'Seconds
defChunkWaitMax = 0.35 :: Timeout 'Seconds

defSweepTimeout :: Timeout 'Seconds
defSweepTimeout = 30 -- FIXME: only for debug!

defPeerAnnounceTime :: Timeout 'Seconds
defPeerAnnounceTime = 120

defPexMaxPeers :: Int
defPexMaxPeers = 50



