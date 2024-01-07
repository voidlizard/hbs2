module HBS2.Share.LocalHash where

import HBS2.Prelude.Plated
import HBS2.Defaults  (defBlockSize)
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Storage.Operations.ByteString

import HBS2.Share.App.Types

import Data.ByteArray.Hash (SipHash(..), SipKey(..))
import Data.ByteArray.Hash qualified as BA
import Streaming.Prelude qualified as S
import Data.ByteString.Lazy qualified as LBS
import Codec.Serialise

newtype LocalHash = LocalHash { fromLocalHash :: Hash HbSync }
                    deriving stock (Eq,Ord,Data,Generic,Show)

instance Serialise LocalHash

instance Pretty LocalHash where
  pretty (LocalHash h) = pretty h

localHash :: MonadUnliftIO m => FilePath -> m LocalHash
localHash fp = do
  liftIO $ withBinaryFile fp ReadMode $ \h -> do
    lbs <- LBS.hGetContents h
    readChunkedBS lbs defBlockSize
      & S.map LBS.toStrict
      & S.map (\z -> let (SipHash w) = BA.sipHash sk0 z in w)
      & S.toList_
      <&> serialise
      <&> LocalHash . hashObject @HbSync
  where
    sk0 = SipKey 5401424299739428297 3116460833428128256


