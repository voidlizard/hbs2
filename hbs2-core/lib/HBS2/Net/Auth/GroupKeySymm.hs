{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language ConstraintKinds #-}
module HBS2.Net.Auth.GroupKeySymm
  ( module HBS2.Net.Auth.GroupKeySymm
  , module HBS2.Net.Proto.Types
  , KeyringEntry(..), krPk, krSk
  ) where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.Data.Types.EncryptedBox
import HBS2.Data.Types.SmallEncryptedBlock
import HBS2.Data.Types.Refs
import HBS2.Net.Auth.Schema
import HBS2.Hash
import HBS2.Merkle
import HBS2.Data.Detect
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Types
import HBS2.Storage hiding (Key)
import HBS2.Storage.Operations.Class
import HBS2.Storage.Operations.ByteString
import HBS2.Storage(Storage(..))
import HBS2.Defaults


import Data.ByteArray.Hash qualified as BA
import Data.ByteArray.Hash (SipHash(..), SipKey(..))
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
import Data.List.Split (chunksOf)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
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

type GroupSecret = Key


-- NOTE: breaking-change

-- NOTE: not-a-monoid
--  это моноид, но опасный, потому, что секретные ключи у двух разных
--  групповых ключей могут быть разными, и если
--  просто объединить два словаря - какой-то секретный
--  ключ может быть потерян. а что делать-то, с другой стороны?
data instance GroupKey 'Symm s =
  GroupKeySymm
  { recipients :: HashMap (PubKey 'Encrypt s) (EncryptedBox GroupSecret)
  }
  deriving stock (Generic)

instance ForGroupKeySymm s => Monoid (GroupKey 'Symm s) where
  mempty = GroupKeySymm mempty

instance ForGroupKeySymm s => Semigroup (GroupKey 'Symm s) where
  (<>) (GroupKeySymm a) (GroupKeySymm b) = GroupKeySymm (a <> b)

instance Serialise Key
instance Serialise SK.Nonce

-- NOTE: hardcoded-hbs2-basic-auth-type
data instance ToEncrypt 'Symm s LBS.ByteString =
  ToEncryptSymmBS
  { toEncryptSecret      :: GroupSecret
  , toEncryptGroupKey    :: LoadedRef (GroupKey 'Symm s)
  , toEncryptNonce       :: BS.ByteString
  , toEncryptData        :: Stream (Of LBS.ByteString) IO ()
  , toEncryptMeta        :: AnnMetaData
  , toEncryptOpts        :: Maybe EncryptGroupNaClSymmOpts
  }
  deriving (Generic)

type ForGroupKeySymm (s :: CryptoScheme ) =
  (
    -- Eq (PubKey 'Encrypt s)
  -- , PubKey 'Encrypt s
  -- , PrivKey 'Encrypt s
    Serialise (PubKey 'Encrypt s)
  , Serialise GroupSecret
  , Serialise SK.Nonce
  , FromStringMaybe (PubKey 'Encrypt s)
  , Hashable (PubKey 'Encrypt s)
  )

instance ForGroupKeySymm s => Serialise (GroupKey 'Symm s)

instance Pretty (AsBase58 (PubKey 'Encrypt s)) => Pretty (GroupKey 'Symm s) where
  pretty g = vcat (fmap prettyEntry (HashMap.toList (recipients g)))
    where
      prettyEntry (pk, _) = "member" <+> dquotes (pretty (AsBase58 pk))


instance ForGroupKeySymm s => FromStringMaybe (GroupKey 'Symm s) where
  fromStringMay s = runIdentity $ runMaybeT do
    bs <- toMPlus $ fromBase58 $ B8.pack s
    toMPlus $ deserialiseOrFail @(GroupKey 'Symm s) (LBS.fromStrict bs)

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

instance ( Serialise  (GroupKey 'Symm s)
         )

  =>  Pretty (AsBase58 (GroupKey 'Symm s)) where
  pretty (AsBase58 c) =
    pretty . B8.unpack . toBase58 . LBS.toStrict . serialise $ c


generateGroupKey :: forall s m . (ForGroupKeySymm s, MonadIO m, PubKey 'Encrypt s ~ AK.PublicKey)
                 => Maybe GroupSecret
                 -> [PubKey 'Encrypt s]
                 -> m (GroupKey 'Symm s)

generateGroupKey mbk pks = GroupKeySymm <$> create
  where
    create = HashMap.fromList <$> do
      sk <- maybe1 mbk (liftIO SK.newKey) pure
      forM pks $ \pk -> do
        box <- liftIO $ AK.boxSeal pk (LBS.toStrict $ serialise sk) <&> EncryptedBox
        pure (pk, box)

lookupGroupKey :: forall s .  ( ForGroupKeySymm s
                              , PubKey 'Encrypt s ~ AK.PublicKey
                              , PrivKey 'Encrypt s ~ AK.SecretKey
                              )
               => PrivKey 'Encrypt s
               -> PubKey 'Encrypt s
               -> GroupKey 'Symm s
               -> Maybe GroupSecret

lookupGroupKey sk pk gk = runIdentity $ runMaybeT do
  (EncryptedBox bs) <- MaybeT $ pure $ HashMap.lookup pk (recipients gk)
  -- error  "FOUND SHIT!"
  gkBs <- MaybeT $ pure $ AK.boxSealOpen pk sk bs
  -- error $ "DECRYPTED SHIT!"
  MaybeT $ pure $ deserialiseOrFail (LBS.fromStrict gkBs) & either (const Nothing) Just


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

    gkh <- either pure (\k -> HashRef <$> writeAsMerkle sto (serialise k)) gk

    let prk = HKDF.extractSkip @_ @HbSyncHash (Saltine.encode key)

    let key0 = HKDF.expand prk nonceS typicalKeyLength & Saltine.decode & fromJust

    let method = case toEncryptOpts source of
          Nothing ->
            EncryptGroupNaClSymm1 (fromHashRef gkh) nonceS

          Just o@(EncryptGroupNaClSymmBlockSIP{}) ->
            EncryptGroupNaClSymm2 o (fromHashRef gkh) nonceS

    let onBlock (i,bs) = do
          case toEncryptOpts source of
            Just (EncryptGroupNaClSymmBlockSIP (a,b)) -> do
              let bss = LBS.toStrict bs
              let (SipHash sip) = BA.sipHash (SipKey a b) bss
              let nonceI = nonceFrom (nonce0, i + sip)
              let encrypted = SK.secretbox key0 nonceI  bss
              pure $ serialise (nonceI, encrypted)

            _  -> do
                    let nonceI = nonceFrom (nonce0, i)
                    let encrypted = SK.secretbox key0 nonceI (LBS.toStrict bs)
                    pure (LBS.fromStrict encrypted)

    hashes' <- liftIO $ toEncryptData source
                & S.zip (S.enumFrom (1 :: Word64) )
                & S.mapM onBlock
                & S.mapM (enqueueBlock sto)
                & S.map (fmap HashRef)
                & S.toList_

    let hashes = catMaybes hashes'

--     -- FIXME: handle-hardcode
    let pt = toPTree (MaxSize defHashListChunk) (MaxNum defTreeChildNum) hashes -- FIXME: settings

    -- FIXME: this-might-not-be-true
    result <- runWriterT $ makeMerkle 0 pt $ \(hx,mt,bss) -> do
      void $ lift $ putBlock sto bss
      tell $ [(hx,mt)]

    let root = headMay [ mt | (h,mt) <- snd result, h == fst result ]

    tree <- maybe (throwError StorageError) pure root

    let ann = MTreeAnn (toEncryptMeta source) method tree

    putBlock sto (serialise ann) >>= maybe (throwError StorageError) pure


data EncMethod = Method1 | Method2

instance ( MonadIO m
         , MonadError OperationError m
         , h ~ HbSync
         , Storage s h ByteString m
         , sch ~ 'HBS2Basic
         -- TODO: why?
         ) => MerkleReader (ToDecrypt 'Symm sch ByteString) s h m where

  data instance TreeKey (ToDecrypt 'Symm sch ByteString) =
      ToDecryptBS   [KeyringEntry sch] (Hash HbSync)
    | ToDecryptBS2  (GroupKey 'Symm sch) B8.ByteString [KeyringEntry sch] (MTreeAnn [HashRef])

  type instance ToBlockR  (ToDecrypt 'Symm sch ByteString) = ByteString
  type instance ReadResult (ToDecrypt 'Symm sch ByteString) = ByteString

  readFromMerkle sto decrypt  = do

    (keys, gk, nonceS, tree) <- decryptDataFrom decrypt

    let gksec' = [ lookupGroupKey sk pk gk | (pk,sk) <- keys ] & catMaybes & headMay

    gksec <- maybe1 gksec' (throwError (GroupKeyNotFound 2)) pure

    let prk = HKDF.extractSkip @_ @HbSyncHash (Saltine.encode gksec)
    let key0 = HKDF.expand prk nonceS typicalKeyLength & Saltine.decode & fromJust
    let nonce0 = nonceFrom @SK.Nonce nonceS

    hashes <- S.toList_ $
      walkMerkleTree (_mtaTree tree) (lift . getBlock sto) $ \case
        Left{} -> throwError MissedBlockError
        Right hrr -> S.each hrr


    method <- case _mtaCrypt tree of
          EncryptGroupNaClSymm1{} -> pure Method1
          EncryptGroupNaClSymm2 (EncryptGroupNaClSymmBlockSIP _) _ _ -> pure Method2
          _ -> throwError UnsupportedFormat

    ss <- forM (zip [1..] hashes) $ \(i :: Word64,h) -> do
      blk <- getBlock sto (fromHashRef h) >>= maybe (throwError MissedBlockError) pure

      case method of
        Method1 -> do
          let nonceI = nonceFrom (nonce0, i)
          let unboxed = SK.secretboxOpen key0 nonceI (LBS.toStrict blk)
          maybe1 unboxed (throwError DecryptionError) (pure . LBS.fromStrict)
        Method2 -> do
          (nonce, bss) <- deserialiseOrFail @(SK.Nonce, N.ByteString) blk
                            & either (const $ throwError UnsupportedFormat) pure
          let unboxed = SK.secretboxOpen key0 nonce bss
          maybe1 unboxed (throwError DecryptionError) (pure . LBS.fromStrict)

    -- FIXME: stream-unboxed-blocks
    pure $ mconcat ss

    where

      decryptDataFrom = \case
        ToDecryptBS2 gk nonce ke tree  -> do
          let keys = [ (view krPk x, view krSk x) | x <- ke ]
          pure (keys, gk, nonce, tree)

        ToDecryptBS ke h  -> do
          let keys = [ (view krPk x, view krSk x) | x <- ke ]

          bs <- getBlock sto h >>= maybe (throwError MissedBlockError) pure
          let what = tryDetect h bs

          let tree' = case what of
                MerkleAnn ann@(MTreeAnn {_mtaCrypt = EncryptGroupNaClSymm g n}) -> Just (ann, (g,n))
                _ -> Nothing

          (tree, (gkh,nonceS)) <- maybe1 tree' (throwError UnsupportedFormat) pure

          gkbs <- readFromMerkle sto (SimpleKey gkh)

          gk <- either (const $ throwError (GroupKeyNotFound 1)) pure (deserialiseOrFail @(GroupKey 'Symm sch) gkbs)

          pure (keys, gk, nonceS, tree)



encryptBlock :: ( MonadIO m
                , Storage sto h ByteString m
                , ForGroupKeySymm s
                , Serialise t
                , h ~ HbSync
                )
             => sto
             -> GroupSecret
             -> LoadedRef (GroupKey 'Symm s)
             -> Maybe BS.ByteString           -- ^ nonce
             -> t
             -> m (SmallEncryptedBlock t)

encryptBlock sto gks gk mnonce x = do

  nonceS <- maybe (liftIO AK.newNonce <&> Saltine.encode) pure mnonce

  let nonce0 = nonceFrom @SK.Nonce nonceS

  gkh <- either pure (\k -> HashRef <$> writeAsMerkle sto (serialise k)) gk

  let prk = HKDF.extractSkip @_ @HbSyncHash (Saltine.encode gks)

  let key0 = HKDF.expand prk nonceS typicalKeyLength & Saltine.decode & fromJust

  let encrypted = SK.secretbox key0 nonce0  (serialise x & LBS.toStrict)

  pure $ SmallEncryptedBlock gkh nonceS (EncryptedBox encrypted)

decryptBlock :: forall t s sto h m . ( MonadIO m
                                     , MonadError OperationError m
                                     , Storage sto h ByteString m
                                     , ForGroupKeySymm s
                                     , h ~ HbSync
                                     , Serialise t
                                     )

             => sto
             -> (GroupKey 'Symm s -> m (Maybe GroupSecret))
             -> SmallEncryptedBlock t
             -> m t

decryptBlock sto findKey (SmallEncryptedBlock{..}) = do

  gkbs <- readFromMerkle sto (SimpleKey (fromHashRef sebGK0))
  gk <- either (const $ throwError (GroupKeyNotFound 1)) pure (deserialiseOrFail @(GroupKey 'Symm s) gkbs)

  gksec' <- findKey gk
  -- [ lookupGroupKey sk pk gk | KeyringKeys pk sk <- keys ] & catMaybes & headMay

  gksec <- maybe1 gksec' (throwError (GroupKeyNotFound 2)) pure

  let prk = HKDF.extractSkip @_ @HbSyncHash (Saltine.encode gksec)
  let key0 = HKDF.expand prk sebNonce typicalKeyLength & Saltine.decode & fromJust
  let nonce0 = nonceFrom @SK.Nonce sebNonce

  let unboxed = SK.secretboxOpen key0 nonce0 (unEncryptedBox sebBox)

  lbs <- maybe1 unboxed (throwError DecryptionError) (pure . LBS.fromStrict)

  either (const $ throwError UnsupportedFormat) pure (deserialiseOrFail lbs)


deriveGroupSecret :: NonceFrom SK.Nonce n => n -> BS.ByteString -> GroupSecret
deriveGroupSecret n bs = key0
  where
    nonceS = nonceFrom @SK.Nonce n & Saltine.encode & hashObject @HbSync & fromHbSyncHash
    prk = HKDF.extractSkip @_ @HbSyncHash bs
    key0 = HKDF.expand prk nonceS typicalKeyLength & Saltine.decode & fromJust


loadGroupKeyMaybe :: ( MonadIO m
                     ) => AnyStorage -> HashRef -> m (Maybe (GroupKey 'Symm HBS2Basic))
loadGroupKeyMaybe sto h = do

  runMaybeT do

    bs <- runExceptT (readFromMerkle sto (SimpleKey (fromHashRef h)))
            <&> either (const Nothing) Just
            >>= toMPlus

    deserialiseOrFail bs
      &  toMPlus

