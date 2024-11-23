module HBS2.Base58 where

import Data.ByteString.Base58 (encodeBase58, bitcoinAlphabet, decodeBase58,Alphabet(..))
import Data.ByteString  qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Word
import Numeric

import Prettyprinter

newtype AsBase58 a = AsBase58 { unAsBase58 :: a }

newtype AsHex a = AsHex { unAsHex :: a }

newtype AsHexSparse a = AsHexSparse { unAsHexSparse :: a }

alphabet :: Alphabet
alphabet = bitcoinAlphabet

getAlphabet :: [Char]
getAlphabet = BS8.unpack (unAlphabet alphabet)


toBase58 :: ByteString -> ByteString
toBase58 = encodeBase58 bitcoinAlphabet

fromBase58 :: ByteString -> Maybe ByteString
fromBase58 = decodeBase58 bitcoinAlphabet


instance Pretty (AsBase58 ByteString) where
  pretty (AsBase58 bs) = pretty $ BS8.unpack $ toBase58 bs

instance Pretty (AsBase58 LBS.ByteString) where
  pretty (AsBase58 bs) = pretty $ BS8.unpack $ toBase58 (LBS.toStrict bs)

instance Show (AsBase58 ByteString) where
  show (AsBase58 bs) = BS8.unpack $ toBase58 bs

instance Show (AsBase58 LBS.ByteString) where
  show (AsBase58 bs) = BS8.unpack . toBase58 . LBS.toStrict $ bs


byteToHex :: Word8 -> String
byteToHex byte = pad $ showHex byte ""
  where pad s = if length s < 2 then '0':s else s

byteStringToHex :: BS.ByteString -> String
byteStringToHex bs = concatMap (byteToHex . fromIntegral) (BS.unpack bs)

instance Pretty (AsHexSparse ByteString) where
  pretty (AsHexSparse bs) = pretty $ unwords $ byteToHex <$> BS.unpack bs

instance Pretty (AsHexSparse LBS.ByteString) where
  pretty (AsHexSparse bs) = pretty $ unwords $ byteToHex <$> LBS.unpack bs

instance Pretty (AsHex ByteString) where
  pretty (AsHex bs) = pretty $ byteStringToHex bs

instance Pretty (AsHex LBS.ByteString) where
  pretty (AsHex bs) = pretty $ byteStringToHex (LBS.toStrict bs)

instance Show (AsHex ByteString) where
  show (AsHex bs) = byteStringToHex bs

instance Show (AsHex LBS.ByteString) where
  show (AsHex bs) = byteStringToHex (LBS.toStrict bs)

