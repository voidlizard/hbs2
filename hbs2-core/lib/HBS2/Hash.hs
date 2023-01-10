{-# Language RankNTypes #-}
module HBS2.Hash
  ( Serialise(..)
  , module HBS2.Hash
  )
  where

import Codec.Serialise
import Crypto.Hash hiding (SHA1)
import Data.Aeson(FromJSON(..),ToJSON(..),Value(..))
import Data.Binary (Binary(..))
import Data.ByteArray qualified as BA
import Data.ByteString.Base58 (encodeBase58, bitcoinAlphabet, decodeBase58,Alphabet(..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SB
import Data.ByteString.Short (ShortByteString)
import Data.Data
import Data.Hashable (Hashable)
import Data.Kind
import Data.String(IsString(..))
import Data.Text qualified as Text
import GHC.Generics
import Prettyprinter
import Text.InterpolatedString.Perl6 (qc)
import Control.DeepSeq (NFData,force)

data HbSync = HbSync
              deriving stock (Data)


data family Hash ( a :: Type )

data HsHash

type family HashType ( a :: Type) where
  HashType HbSync = Blake2b_256

newtype instance Hash HbSync =
  HbSyncHash ShortByteString
  deriving stock (Eq,Ord,Data,Generic)
  deriving newtype (Hashable,Show)

instance NFData (Hash HbSync)
instance Serialise (Hash HbSync)
instance Binary (Hash HbSync)

newtype Internal a = Internal a

class Hashed a where
  hashObject :: a -> Hash HbSync

alphabet :: Alphabet
alphabet = bitcoinAlphabet

getAlphabet :: [Char]
getAlphabet = BS8.unpack (unAlphabet alphabet)


instance Hashed ByteString where
  hashObject s = HbSyncHash $ force $ SB.toShort $ BA.convert digest
    where
      digest = hash s :: Digest (HashType HbSync)

instance Hashed LBS.ByteString where
  hashObject s = HbSyncHash $ force $ SB.toShort $ BA.convert digest
    where
      digest = hashlazy s :: Digest (HashType HbSync)

instance IsString (Hash HbSync) where
  fromString s = maybe (error ("invalid base58: " <> show s)) HbSyncHash doDecode
    where
      doDecode = SB.toShort <$> decodeBase58 alphabet (BS8.pack s)

instance Pretty (Hash HbSync) where
  pretty (HbSyncHash s) = pretty @String [qc|{encodeBase58 bitcoinAlphabet (SB.fromShort s)}|]


instance FromJSON (Hash HbSync) where
  parseJSON = \case
    String s -> pure (fromString (Text.unpack s))
    _        -> fail "expected string"

instance ToJSON (Hash HbSync) where
  toJSON s = toJSON (show $ pretty s)


