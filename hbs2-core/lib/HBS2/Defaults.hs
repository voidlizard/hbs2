module HBS2.Defaults where

import HBS2.Clock
import Data.String

defChunkSize :: Integer
defChunkSize = 500

defBlockSize :: Integer
defBlockSize = 256 * 1024

defStorePath :: IsString a => a
defStorePath = "hbs2"

defPipelineSize :: Int
defPipelineSize = 100

-- typical block hash 530+ chunks * parallel wip blocks amount
defProtoPipelineSize :: Int
defProtoPipelineSize = 65536

defCookieTimeout :: TimeSpec
defCookieTimeout = toTimeSpec ( 10  :: Timeout 'Minutes)

