module HBS2.Defaults where

import HBS2.Clock
import Data.String

-- defChunkSize :: Integer
defChunkSize :: Integral a => a
defChunkSize = 500

defBlockSize :: Integer
defBlockSize = 1024 * 1024

defStorePath :: IsString a => a
defStorePath = "hbs2"

defPipelineSize :: Int
defPipelineSize = 2000

defChunkWriterQ :: Integral a => a
defChunkWriterQ = 2000

defBlockDownloadQ :: Integral a => a
defBlockDownloadQ = 65536*128

defBlockDownloadThreshold :: Integral a => a
defBlockDownloadThreshold = 2

-- typical block hash 530+ chunks * parallel wip blocks amount
defProtoPipelineSize :: Int
defProtoPipelineSize = 65536*4

defCookieTimeout :: TimeSpec
defCookieTimeout = toTimeSpec ( 60  :: Timeout 'Minutes)

defBlockInfoTimeout :: TimeSpec
defBlockInfoTimeout = toTimeSpec ( 60  :: Timeout 'Minutes)

-- how much time wait for block from peer?
defBlockWaitMax :: Timeout 'Seconds
defBlockWaitMax = 10  :: Timeout 'Seconds

defBlockWaitSleep :: Timeout 'Seconds
defBlockWaitSleep = 0.01  :: Timeout 'Seconds

defSweepTimeout :: Timeout 'Seconds
defSweepTimeout = 5 -- FIXME: only for debug!



