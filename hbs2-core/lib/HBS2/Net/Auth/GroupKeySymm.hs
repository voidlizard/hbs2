{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language ConstraintKinds #-}
module HBS2.Net.Auth.GroupKeySymm where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.Data.Types.EncryptedBox
import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Merkle
import HBS2.Data.Detect
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Types
import HBS2.Storage.Operations.Class
import HBS2.Storage.Operations.ByteString
import HBS2.Storage(Storage(..))

import HBS2.System.Logger.Simple

import Data.ByteArray.Hash qualified as BA

import Codec.Serialise
import Crypto.KDF.HKDF qualified as HKDF
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Crypto.Saltine.Class qualified as Saltine
import Crypto.Saltine.Core.Box qualified as AK
import Crypto.Saltine.Core.SecretBox (Key)
import Crypto.Saltine.Core.SecretBox qualified as SK
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Function
import Data.Functor
import Data.List qualified as List
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.Word (Word64)
import Data.ByteArray()
import Network.ByteOrder qualified as N
import Streaming.Prelude qualified as S
import Lens.Micro.Platform

import Streaming qualified as S
import Streaming (Stream(..), Of(..))

import System.IO.Unsafe (unsafePerformIO)

import Data.Bits (xor)

type GroupSecretAsymm = Key

data instance GroupKey 'Symm s =
  GroupKeySymm
  { recipients :: [(PubKey 'Encrypt s, EncryptedBox GroupSecretAsymm)]
  }
  deriving stock (Generic)

instance Serialise Key
instance Serialise SK.Nonce

-- NOTE: hardcoded-hbs2-basic-auth-type
data instance ToEncrypt 'Symm s LBS.ByteString =
  ToEncryptSymmBS
  { toEncryptSecret      :: GroupSecretAsymm
  , toEncryptNonce       :: BS.ByteString
  , toEncryptData        :: Stream (Of LBS.ByteString) IO ()
  , toEncryptGroupKey    :: GroupKey 'Symm s
  }


type ForGroupKeySymm s = ( Eq (PubKey 'Encrypt s)
                         , PubKey 'Encrypt s ~ AK.PublicKey
                         , PrivKey 'Encrypt s ~ AK.SecretKey
                         , Serialise (PubKey 'Encrypt s)
                         , Serialise GroupSecretAsymm
                         , Serialise SK.Nonce
                         , FromStringMaybe (PubKey 'Encrypt s)
                         )

instance ForGroupKeySymm s => Serialise (GroupKey 'Symm s)

instance Pretty (AsBase58 (PubKey 'Encrypt s)) => Pretty (GroupKey 'Symm s) where
  pretty g = vcat (fmap prettyEntry (recipients g))
    where
      prettyEntry (pk, _) = "member" <+> dquotes (pretty (AsBase58 pk))


instance ForGroupKeySymm s => Pretty (AsGroupKeyFile (GroupKey 'Symm s)) where
  pretty (AsGroupKeyFile pc) =  "# hbs2 symmetric group key file"
                                <> line <> co
    where
      co = vcat $ fmap pretty
                $ chunksOf 60
                $ show
                $ pretty (AsBase58 (serialise pc))


parseGroupKey :: forall s . (ForGroupKeySymm s, Serialise (GroupKey 'Symm s))
                 =>  AsGroupKeyFile ByteString
                 ->  Maybe (GroupKey 'Symm s)

parseGroupKey (AsGroupKeyFile bs) = parseSerialisableFromBase58 (LBS8.toStrict bs)

instance ( Serialise  (GroupKey 'Asymm s)
         )

  =>  Pretty (AsBase58 (GroupKey 'Asymm s)) where
  pretty (AsBase58 c) =
    pretty . B8.unpack . toBase58 . LBS.toStrict . serialise $ c

generateGroupKey :: forall s m . (ForGroupKeySymm s, MonadIO m)
                 => [PubKey 'Encrypt s]
                 -> m (GroupKey 'Symm s)

generateGroupKey pks' = GroupKeySymm <$> create
  where
    pks = List.sort (List.nub pks')

    create = do
      sk <- liftIO SK.newKey
      forM pks $ \pk -> do
        box <- liftIO $ AK.boxSeal pk (LBS.toStrict $ serialise sk) <&> EncryptedBox
        pure (pk, box)

lookupGroupKey :: ForGroupKeySymm s
               => PrivKey 'Encrypt s
               -> PubKey 'Encrypt s
               -> GroupKey 'Symm s
               -> Maybe GroupSecretAsymm

lookupGroupKey sk pk gk = runIdentity $ runMaybeT do
  (EncryptedBox bs) <- MaybeT $ pure $ List.lookup pk (recipients gk)
  -- error  "FOUND SHIT!"
  gkBs <- MaybeT $ pure $ AK.boxSealOpen pk sk bs
  -- error $ "DECRYPTED SHIT!"
  MaybeT $ pure $ deserialiseOrFail (LBS.fromStrict gkBs) & either (const Nothing) Just

-- FIXME: move-to-appropriate-place
class NonceFrom nonce a where
  nonceFrom :: a -> nonce

typicalNonceLength :: Integral a => a
typicalNonceLength = unsafePerformIO SK.newNonce & Saltine.encode & B8.length & fromIntegral

typicalKeyLength :: Integral a => a
typicalKeyLength = unsafePerformIO SK.newKey & Saltine.encode & B8.length & fromIntegral

instance NonceFrom SK.Nonce (SK.Nonce, Word64) where
  -- FIXME: maybe-slow-nonceFrom
  nonceFrom (n0, w) = fromJust $ Saltine.decode nss
    where
      ws = noncePrefix <> N.bytestring64 w
      ns = Saltine.encode n0
      nss = BS.packZipWith xor ns ws

      noncePrefix = BS.replicate (typicalNonceLength - 8) 0

instance NonceFrom SK.Nonce ByteString where
  -- FIXME: maybe-slow-nonceFrom
  nonceFrom lbs = fromJust $ Saltine.decode
                           $ LBS.toStrict
                           $ LBS.take typicalNonceLength
                           $ lbs <> LBS.replicate typicalNonceLength 0


instance NonceFrom SK.Nonce BS.ByteString where
  -- FIXME: maybe-slow-nonceFrom
  nonceFrom bs = fromJust $ Saltine.decode
                          $ BS.take typicalNonceLength
                          $ bs <> BS.replicate typicalNonceLength 0

instance ( MonadIO m
         , MonadError OperationError m
         , Storage sto h ByteString m
         , Storage sto h ByteString IO
         , h ~ HbSync
         , ForGroupKeySymm s
         ) => MerkleWriter (ToEncrypt 'Symm s ByteString) h sto m where

  type instance ToBlockW (ToEncrypt 'Symm s ByteString) = ByteString

  writeAsMerkle sto source = do

    let gk = toEncryptGroupKey source

    let key = toEncryptSecret source

    let nonceS = toEncryptNonce source

    let nonce0 = nonceFrom @SK.Nonce (toEncryptNonce source)

    gkh <- writeAsMerkle sto (serialise gk) <&> HashRef

    let prk = HKDF.extractSkip @_ @HbSyncHash (Saltine.encode key)

    let key0 = HKDF.expand prk nonceS typicalKeyLength & Saltine.decode & fromJust

    hashes' <- liftIO $ toEncryptData source
                & S.zip (S.enumFrom (1 :: Word64) )
                & S.mapM ( \(i,bs) -> do
                             let nonceI = nonceFrom (nonce0, i)
                             let encrypted = SK.secretbox key0 nonceI (LBS.toStrict bs)
                             pure (LBS.fromStrict encrypted)
                         )

                & S.mapM (enqueueBlock sto)
                & S.map (fmap HashRef)
                & S.toList_

    let hashes = catMaybes hashes'

--     -- FIXME: handle-hardcode
    let pt = toPTree (MaxSize 256) (MaxNum 256) hashes -- FIXME: settings

    -- FIXME: this-might-not-be-true
    result <- runWriterT $ makeMerkle 0 pt $ \(hx,mt,bss) -> do
      void $ lift $ putBlock sto bss
      tell $ [(hx,mt)]

    let root = headMay [ mt | (h,mt) <- snd result, h == fst result ]

    tree <- maybe (throwError StorageError) pure root

    let ann = MTreeAnn NoMetaData (EncryptGroupNaClSymm (fromHashRef gkh) nonceS) tree

    putBlock sto (serialise ann) >>= maybe (throwError StorageError) pure


instance ( MonadIO m
         , MonadError OperationError m
         , h ~ HbSync
         , Storage s h ByteString m
         -- TODO: why?
         , sch ~ HBS2Basic
         ) => MerkleReader (ToDecrypt 'Symm sch ByteString) s h m where

  data instance TreeKey (ToDecrypt 'Symm sch ByteString) = ToDecryptBS [KeyringEntry sch] (Hash HbSync)

  type instance ToBlockR  (ToDecrypt 'Symm sch ByteString) = ByteString
  type instance ReadResult (ToDecrypt 'Symm sch ByteString) = ByteString

  readFromMerkle sto (ToDecryptBS ke h) = do

    let keys = [ (view krPk x, view krSk x) | x <- ke ]

    bs <- getBlock sto h >>= maybe (throwError MissedBlockError) pure
    let what = tryDetect h bs

    let tree' = case what of
          MerkleAnn ann@(MTreeAnn {_mtaCrypt = EncryptGroupNaClSymm g n}) -> Just (_mtaTree ann, (g,n))
          _ -> Nothing

    (tree, (gkh,nonceS)) <- maybe1 tree' (throwError UnsupportedFormat) pure

    gkbs <- readFromMerkle sto (SimpleKey gkh)

    gk <- either (const $ throwError GroupKeyNotFound) pure (deserialiseOrFail @(GroupKey 'Symm sch) gkbs)

    let gksec' = [ lookupGroupKey sk pk gk | (pk,sk) <- keys ] & catMaybes & headMay

    gksec <- maybe1 gksec' (throwError GroupKeyNotFound) pure

    let prk = HKDF.extractSkip @_ @HbSyncHash (Saltine.encode gksec)
    let key0 = HKDF.expand prk nonceS typicalKeyLength & Saltine.decode & fromJust
    let nonce0 = nonceFrom @SK.Nonce nonceS

    hashes <- S.toList_ $
      walkMerkleTree tree (lift . getBlock sto) $ \case
        Left{} -> throwError MissedBlockError
        Right hrr -> S.each hrr

    ss <- forM (zip [1..] hashes) $ \(i :: Word64,h) -> do
      blk <- getBlock sto (fromHashRef h) >>= maybe (throwError MissedBlockError) pure

      let nonceI = nonceFrom (nonce0, i)
      let unboxed = SK.secretboxOpen key0 nonceI (LBS.toStrict blk)

      maybe1 unboxed (throwError DecryptionError) (pure . LBS.fromStrict)

    -- FIXME: stream-unboxed-blocks
    pure $ mconcat ss


