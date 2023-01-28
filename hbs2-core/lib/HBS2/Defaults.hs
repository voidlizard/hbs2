module HBS2.Defaults where

import HBS2.Clock
import Data.String

-- defChunkSize :: Integer
defChunkSize :: Integral a => a
defChunkSize = 500

defBlockSize :: Integer
defBlockSize = 256 * 1024

defStorePath :: IsString a => a
defStorePath = "hbs2"

defPipelineSize :: Int
defPipelineSize = 16000

defChunkWriterQ :: Integral a => a
defChunkWriterQ = 16000

defBlockDownloadQ :: Integral a => a
defBlockDownloadQ = 2000

defBlockDownloadThreshold :: Integral a => a
defBlockDownloadThreshold = 2

-- typical block hash 530+ chunks * parallel wip blocks amount
defProtoPipelineSize :: Int
defProtoPipelineSize = 2000

defCookieTimeout :: TimeSpec
defCookieTimeout = toTimeSpec ( 120  :: Timeout 'Minutes)

defBlockInfoTimeout :: TimeSpec
defBlockInfoTimeout = toTimeSpec ( 120  :: Timeout 'Minutes)

-- how much time wait for block from peer?
defBlockWaitMax :: Timeout 'Seconds
defBlockWaitMax = 60 :: Timeout 'Seconds

defSweepTimeout :: Timeout 'Seconds
defSweepTimeout = 5 -- FIXME: only for debug!



