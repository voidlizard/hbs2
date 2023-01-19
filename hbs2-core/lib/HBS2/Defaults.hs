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
defPipelineSize = 100

defChunkWriterQ :: Integral a => a
defChunkWriterQ = 100

defBlockDownloadThreshold :: Integral a => a
defBlockDownloadThreshold = 2

-- typical block hash 530+ chunks * parallel wip blocks amount
defProtoPipelineSize :: Int
defProtoPipelineSize = 65536

defCookieTimeout :: TimeSpec
defCookieTimeout = toTimeSpec ( 10  :: Timeout 'Minutes)

defBlockInfoTimeout :: TimeSpec
defBlockInfoTimeout = toTimeSpec ( 10  :: Timeout 'Minutes)



