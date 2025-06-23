module HBS2.Storage.NCQ.Types where

import HBS2.Prelude
import HBS2.Data.Types.Refs
import HBS2.Hash

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Network.ByteOrder qualified as N
import Data.Coerce

-- Log structure:
-- (SD)*
-- S      ::= word32be, section prefix
-- D      ::= HASH PREFIX DATA
-- HASH   ::= BYTESTRING(32)
-- PREFIX ::= BYTESTRING(4)
-- DATA   ::= BYTESTRING(n) | n == S - LEN(WORD32) - LEN(HASH) - LEN(PREFIX)

newtype NCQFullRecordLen a =
  NCQFullRecordLen a
  deriving newtype (Num,Enum,Integral,Real,Ord,Eq)

-- including prefix
ncqFullDataLen :: forall a . Integral a => NCQFullRecordLen a -> a
ncqFullDataLen full = fromIntegral full - ncqKeyLen
{-# INLINE ncqFullDataLen #-}

ncqKeyLen :: forall a . Integral a => a
ncqKeyLen = 32
{-# INLINE ncqKeyLen #-}

-- 'S' in SD, i.e size, i.e section header
ncqSLen:: forall a . Integral a => a
ncqSLen = 4
{-# INLINE ncqSLen #-}

ncqDataOffset :: forall a b . (Integral a, Integral b) => a -> b
ncqDataOffset base = fromIntegral base + ncqSLen + ncqKeyLen
{-# INLINE ncqDataOffset #-}


ncqFullTombLen :: forall a . Integral a => a
ncqFullTombLen = ncqSLen + ncqKeyLen + ncqPrefixLen + 0
{-# INLINE ncqFullTombLen #-}


data NCQSectionType = B | R | T
                      deriving stock (Eq,Ord,Show)

instance Pretty NCQSectionType where
  pretty = \case
    B -> "B"
    T -> "T"
    R -> "R"

ncqPrefixLen :: Integral a => a
ncqPrefixLen = 4
{-# INLINE ncqPrefixLen #-}

ncqRefPrefix :: ByteString
ncqRefPrefix = "R;;\x00"

ncqBlockPrefix :: ByteString
ncqBlockPrefix = "B;;\x00"

ncqTombPrefix :: ByteString
ncqTombPrefix = "T;;\x00"

ncqMakeSectionBS :: Maybe NCQSectionType
                 -> HashRef
                 -> ByteString
                 -> ByteString
ncqMakeSectionBS t h bs = do
  let slen = ncqKeyLen + prefLen + fromIntegral (BS.length bs)
  let ss = N.bytestring32 slen
  let section = ss <> coerce h <> prefix <> bs
  section

  where
    (prefLen, prefix) =
      case t of
        Nothing -> (0, mempty)
        Just B  -> (ncqPrefixLen, ncqBlockPrefix)
        Just T  -> (ncqPrefixLen, ncqTombPrefix)
        Just R  -> (ncqPrefixLen, ncqRefPrefix)

{-# INLINE ncqMakeSectionBS #-}


