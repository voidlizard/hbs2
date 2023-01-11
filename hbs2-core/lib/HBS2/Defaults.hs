module HBS2.Defaults where

import Data.String

defChunkSize :: Integer
defChunkSize = 500

defBlockSize :: Integer
defBlockSize = 256 * 1024

defStorePath :: IsString a => a
defStorePath = "hbs2"

