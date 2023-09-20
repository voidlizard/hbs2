{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2.Data.Bundle where

import HBS2.Prelude
import HBS2.Storage
import HBS2.Storage.Operations
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Net.Proto.Types
import HBS2.Net.Auth.Credentials
import HBS2.Data.Detect

import Data.Word

import Data.Function
import Codec.Compression.GZip as GZip
import Codec.Serialise
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Functor
import Data.List qualified as List
import Data.Either
import Data.Maybe

import Streaming.Prelude qualified as S
import Streaming()

{- HLINT ignore "Use newtype instead of data" -}

data BundleRefValue e =
  BundleRefValue (SignedBox BundleRef e)
  deriving stock (Generic)

instance ForSignedBox e => Serialise (BundleRefValue e)

data BundleRef =
  BundleRefSimple HashRef
  deriving stock (Generic)

instance Serialise BundleRef


makeBundleRefValue :: forall e . (ForSignedBox e, Signatures (Encryption e))
                   => PubKey 'Sign (Encryption e)
                   -> PrivKey 'Sign (Encryption e)
                   -> BundleRef
                   -> BundleRefValue e

makeBundleRefValue pk sk ref = BundleRefValue $ makeSignedBox @e pk sk ref

-- у нас может быть много способов хранить данные:
--   сжимать целиком (эффективно, но медленно)
--   сжимать по секциям (быстрее, но менее эффективно)
data BundleHead =
  BundleHeadSimple
  { bundleHeadSectionSize :: Word16
  }
  deriving stock (Generic,Show)

instance Serialise BundleHead

bundleHeadSize :: Integral a => a
bundleHeadSize = 64

data BundleSection =
  BundleSection
  { _bundleSectionSize :: Word32
  , _bundleSectionHash :: Maybe HashRef
  }
  deriving stock (Generic,Show)

instance Serialise BundleSection

sectionHeadSize :: Integral a => a
sectionHeadSize = 64

-- TODO: implement-create-bundle
--   создаёт bundle в том же самом хранилище,
--   допустим.
--   Возвращает HashRef
--   Если нам нужен сам бандл - всегда сможем
--   сделать hbs2 cat.
--   t HashRef -- это что-то, что даёт нам список хэшей?
--   для каждого хэша надо бы уметь считать
--   его данные, и куда-то их записать в лог.
--   лог может быть очень большим. как его
--   писать-то? допустим, через tmp файл -- тогда
--   довольно прямолинейно, но функция требует
--   файловой системы.
--   можно поблочно, но это прямо довольно-таки сложно
--   и неатомарно. Как же сделать?
--   непрямолинейное решение:
--     пишем в буфер (какой?)
--     как понаписали достат. кол - то что? меркл
--     дерево у нас создаётся через toPtree

createBundle :: ( MonadIO m
                , Storage sto HbSync ByteString IO
                )
             => sto
             -> [HashRef]
             -> m (Maybe HashRef)

createBundle sto refs = do
  let readBlock = liftIO . getBlock sto

  -- FIXME: handle-errors-on-missed-blocks
  blocks <- S.toList_ $ forM_ refs $ \hr -> do
              deepScan ScanDeep (const none) (fromHashRef hr) readBlock $ \ha -> do
                blk' <- readBlock ha
                let href = HashRef ha
                maybe1 blk' none $ \blk -> do
                  let compressed = compressWith params blk
                  let size = LBS.length compressed
                  let section = BundleSection (fromIntegral size) (Just href)

                  let sbs = serialise section
                  let pad = sectionHeadSize - LBS.length sbs
                  let pads = LBS.replicate pad '\x0'
                  S.yield (sbs <> pads <> compressed)

  let buHead = serialise (BundleHeadSimple sectionHeadSize)
  let buPadded = buHead <> LBS.replicate (bundleHeadSize - LBS.length buHead) '\x0'

  let blob = buPadded <> mconcat blocks

  -- FIXME: streamed-write-as-merkle
  wtf <- liftIO $ writeAsMerkle sto blob
  pure $ Just (HashRef wtf)

  where
    params = defaultCompressParams { compressLevel = bestSpeed }

data ImportError =
    ImportBlocksMissed [HashRef]
  | ImportBadJournal
  deriving stock (Eq,Show)

missedBlocks :: ImportError -> [HashRef]
missedBlocks = \case
  ImportBlocksMissed blk -> blk
  _ -> mempty

class MonadIO m => ImportBundle bundle m where
  importBundle :: forall sto . Storage sto HbSync ByteString m
               => sto
               -> ( (Maybe HashRef, ByteString) -> m () )
               -> bundle
               -> m (Either ImportError ())


instance MonadIO m => ImportBundle HashRef m where
  -- можем только целиком считать в память, потом пройтись по секциям и записать объекты
  -- по памяти капец, что может быть
  importBundle sto action bundle = do
    let h = fromHashRef bundle

    res <- S.toList_ $
            deepScan ScanDeep yieldMissed h (lift . getBlock sto) $ \ha -> do
              -- FIXME: annoying-shit
              when (ha /= h) do
                lift (getBlock sto ha) >>= maybe (yieldMissed ha) (S.yield . Right)

    let missed = lefts res

    if not (null missed) then
      pure $ Left $ ImportBlocksMissed $ foldMap missedBlocks missed
    else do
      let bss = mconcat (rights res)
      let (hs, bs) = LBS.splitAt bundleHeadSize bss
      -- FIXME: fix-possible-memory-exhausting
      case deserialiseOrFail @BundleHead hs of
        Left{} -> pure $ Left ImportBadJournal
        Right hd -> go hd bs

    where
      yieldMissed = S.yield . Left . ImportBlocksMissed . pure . HashRef

      go hd bs
        | LBS.null bs = pure $ Right ()
        | otherwise = do
            let ss = bundleHeadSectionSize hd
            let (bsh, allBsRest) = LBS.splitAt sectionHeadSize bs
            case deserialiseOrFail @BundleSection bsh of
              Left{} -> do
                pure $ Left ImportBadJournal

              Right header -> do
                let sz = fromIntegral $ _bundleSectionSize header
                let (blk, rest) = LBS.splitAt sz allBsRest
                if LBS.length blk /= sz then do
                  pure $ Left ImportBadJournal
                else do
                  action (_bundleSectionHash header, GZip.decompress blk)
                  go hd rest

