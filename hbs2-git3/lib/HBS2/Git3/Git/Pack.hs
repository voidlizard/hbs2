module HBS2.Git3.Git.Pack where

import HBS2.Prelude
import HBS2.Git.Local

import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.ByteString as BS
import Lens.Micro.Platform
import Data.Function
import Data.Word
import Network.ByteOrder qualified as N
import Numeric.Natural

--
-- Accordingly to https://git-scm.com/docs/pack-format
data PackFileObjectType =
      OBJ_COMMIT       -- (1)
    | OBJ_TREE         -- (2)
    | OBJ_BLOB         -- (3)
    | OBJ_TAG          -- (4)
    | OBJ_RESERVED     -- (5)
    | OBJ_OFS_DELTA    -- (6)
    | OBJ_REF_DELTA    -- (7)
    deriving stock (Eq,Ord,Show)

instance Enum PackFileObjectType where
    fromEnum OBJ_COMMIT    = 1
    fromEnum OBJ_TREE      = 2
    fromEnum OBJ_BLOB      = 3
    fromEnum OBJ_TAG       = 4
    fromEnum OBJ_RESERVED  = 5
    fromEnum OBJ_OFS_DELTA = 6
    fromEnum OBJ_REF_DELTA = 7

    toEnum 1 = OBJ_COMMIT
    toEnum 2 = OBJ_TREE
    toEnum 3 = OBJ_BLOB
    toEnum 4 = OBJ_TAG
    toEnum 6 = OBJ_OFS_DELTA
    toEnum 7 = OBJ_REF_DELTA
    toEnum n = error $ "Invalid PackFileObjectType: " ++ show n

class HasGitPackType a where
  gitPackTypeOf :: a -> PackFileObjectType


instance HasGitPackType GitObjectType where
  gitPackTypeOf = \case
    Commit -> OBJ_COMMIT
    Tree   -> OBJ_TREE
    Blob   -> OBJ_BLOB

encodeObjectSize :: PackFileObjectType -> Natural -> ByteString
encodeObjectSize objType size =
  BS.pack $ go (fromIntegral (fromEnum objType) `shiftL` 4 .|. fromIntegral (size .&. 0x0F)) (size `shiftR` 4)
  where
    go :: Word8 -> Natural -> [Word8]
    go prefix 0 = [prefix]
    go prefix sz = (prefix .|. 0x80) : go (fromIntegral (sz .&. 0x7F)) (sz `shiftR` 7)

decodeObjectSize :: ByteString -> Maybe ((PackFileObjectType, Natural),  ByteString )
decodeObjectSize source = run $ flip fix (source,0,0,0) $ \next (bs, i, tp, num) -> do
  case BS.uncons bs of
    Nothing -> Nothing
    Just (byte, rest) -> do
      let val      = clearBit byte 7

      let acc = case i of
                 0 -> (rest, succ i, fromIntegral (val `shiftR` 4), fromIntegral (val .&. 0x0F))
                 1 -> (rest, succ i, tp, num .|. fromIntegral val `shiftL` 4)
                 _ -> (rest, succ i, tp, num .|. fromIntegral val `shiftL` 7)

      if testBit byte 7 then
        next acc
      else
        pure acc

  where
    run loop = case loop of
      Just (bs, _, tp, x) | tp > 0 && tp <= 7 -> Just ((toEnum tp, fromIntegral x), bs)
      _ -> Nothing



