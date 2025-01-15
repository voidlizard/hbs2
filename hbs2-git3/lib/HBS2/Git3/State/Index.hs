module HBS2.Git3.State.Index where

import HBS2.Git3.Prelude
import HBS2.System.Dir
import HBS2.CLI.Run.Internal.Merkle (getTreeContents)
import HBS2.Git3.State.Types
import HBS2.Git3.Git

import HBS2.Data.Log.Structured

import Data.ByteString qualified as BS
import Data.ByteString.Lazy ( ByteString )
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List qualified as L
import Network.ByteOrder qualified as N
import System.IO.Temp as Temp
import Data.Heap (Entry(..))
import Data.Heap qualified as Heap
import Data.ByteString.Lazy qualified as LBS
import Data.Fixed
import Data.Either
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Word

import Data.Config.Suckless

import Codec.Compression.Zstd.Streaming as ZStdS
import Codec.Serialise
import Streaming.Prelude qualified as S
import Streaming hiding (run,chunksOf)
import System.TimeIt
import Lens.Micro.Platform

import UnliftIO
import UnliftIO.IO.File qualified as UIO

import Data.HashPSQ qualified as HPSQ


readLogFileLBS :: forall opts m . ( MonadIO m, ReadLogOpts opts, BytesReader m )
               => opts
               -> ( GitHash -> Int -> ByteString -> m () )
               -> m Int

readLogFileLBS _ action = flip fix 0 \go n -> do
  done <- noBytesLeft
  if done then pure n
    else do
      ssize <- readBytesMaybe 4
                  >>= orThrow SomeReadLogError
                  <&> fromIntegral . N.word32 . LBS.toStrict

      hash  <- readBytesMaybe 20
                  >>= orThrow SomeReadLogError
                  <&> GitHash . BS.copy . LBS.toStrict

      sdata <- readBytesMaybe ( ssize - 20 )
                  >>= orThrow SomeReadLogError

      void $ action hash (fromIntegral ssize) sdata
      go (succ n)

indexPath :: forall m . ( Git3Perks m
                        , MonadReader Git3Env m
                        ) => m FilePath
indexPath = do
  reflog <- getGitRemoteKey >>= orThrow Git3ReflogNotSet
  getStatePath (AsBase58 reflog)

data IndexEntry =
  IndexEntry
  { entryFile  :: FilePath
  , entryBS    :: N.ByteString
  }

data Index a =
  Index { entries    :: [IndexEntry]
        }


mergeSortedFilesN :: forall m . MonadUnliftIO m
                  => (N.ByteString -> N.ByteString) -- ^ Функция извлечения ключа
                  -> [FilePath]                -- ^ Входные файлы
                  -> FilePath                  -- ^ Выходной файл
                  -> m ()

mergeSortedFilesN _ [] out  = rm out

mergeSortedFilesN _ [_] out  = rm out

mergeSortedFilesN getKey inputFiles outFile = do

  mmaped <- for inputFiles $ \fn -> do
     bs <- liftIO (mmapFileByteString fn Nothing)
     pure $ toSectionList bs

  liftIO $ UIO.withBinaryFileAtomic outFile WriteMode $ \hOut -> do

    let seed = Heap.fromList $ mapMaybe mkState mmaped

    flip fix seed  $ \next heap -> do
      let h0 = Heap.uncons heap
      maybe1 h0 none $ \case
        (Entry _ [], rest) -> next rest
        (Entry k (e:xs), rest) -> do
          liftIO $ writeSection (LBS.fromStrict e) (LBS.hPutStr hOut)
          let zu = maybe rest (`Heap.insert` rest) (mkState xs)
          let what = Heap.toUnsortedList zu & mapMaybe (mkState . dropDupes k . payload)
                                            & Heap.fromList
          let new = what
          next new

    mapM_ rm inputFiles

  where
    dropDupes k = L.dropWhile ( (== k) . getKey )
    mkState [] = Nothing
    mkState (x:xs) = Just (Entry (getKey x) (x:xs))

compactIndex :: forall m . (Git3Perks m, MonadReader Git3Env m) => Natural -> m ()
compactIndex maxSize = do
  reflog <- getGitRemoteKey >>= orThrowUser "reflog not set"
  idxPath <- getStatePath (AsBase58 reflog)
  mkdir idxPath
  files <- listObjectIndexFiles <&> L.sortOn snd

  let blocks = fix (\next (acc, group, remaining) ->
        case remaining of
          [] -> [reverse group | not (null group)]
          ((file, size) : rest)
            | acc + size > maxSize -> reverse group : next (size, [(file, size)], rest)
            | otherwise -> next (acc + size, (file, size) : group, rest))

  forM_ (blocks (0, [], files)) $ \block -> do
    out <- liftIO $ emptyTempFile idxPath "objects-.idx"
    mergeSortedFilesN (BS.take 20) (map fst block) out

openIndex :: forall a m . (Git3Perks m, MonadReader Git3Env m)
          => m (Index a)

openIndex = do
  files <- listObjectIndexFiles
  bss <- liftIO $ for files $ \(f,_)  -> (f,) <$> mmapFileByteString f Nothing
  let entries = [ IndexEntry f bs | (f,bs) <- bss ]
  pure $ Index entries

indexEntryLookup :: forall a m . (Git3Perks m)
              => Index a
              -> GitHash
              -> m (Maybe N.ByteString)

indexEntryLookup Index{..} h = do
  already_ <- newTVarIO ( mempty :: HashMap GitHash N.ByteString )
  forConcurrently_ entries $ \IndexEntry{..} -> do
    what <- readTVarIO already_ <&> HM.lookup h
    case what of
      Just{}   -> none
      Nothing  -> do
        offset' <- binarySearchBS 56 ( BS.take 20 . BS.drop 4 ) (coerce h) entryBS
        maybe1 offset' none $ \offset -> do
          let ebs = BS.take 32 $ BS.drop (offset + 4 + 20) entryBS
          atomically $ modifyTVar already_ (HM.insert h ebs)

  readTVarIO already_ <&> headMay . HM.elems

indexFilterNewObjects :: forall a m . (Git3Perks m)
                      => Index a
                      -> HashSet GitHash
                      -> m [GitHash]

indexFilterNewObjects Index{..} hashes = do
  old_ <- newTVarIO ( mempty :: HashSet GitHash )
  forConcurrently_ entries $ \IndexEntry{..} -> do
    flip fix (HS.toList hashes) $ \next -> \case
      [] -> none
      (x:xs) -> do
        old <- readTVarIO old_ <&> HS.member x
        if old then
          next xs
        else do
          off <- binarySearchBS 56 ( BS.take 20 . BS.drop 4 ) (coerce x) entryBS
          when (isJust off) do
            atomically $ modifyTVar old_ (HS.insert x)
          next xs

  old <- readTVarIO old_
  pure $ HS.toList (hashes `HS.difference` old)


indexFilterNewObjectsMem :: forall a m . (Git3Perks m)
                      => Index a
                      -> HashSet GitHash
                      -> m [GitHash]

indexFilterNewObjectsMem idx@Index{..} hashes = do
  old_ <- newTVarIO ( mempty  :: HashSet GitHash )
  enumEntries idx $ \bs -> do
    atomically $ modifyTVar old_ (HS.insert (coerce $ BS.take 20 bs))

  old <- readTVarIO old_
  pure $ HS.toList (hashes `HS.difference` old)


listObjectIndexFiles :: forall m . ( Git3Perks m
                                   , MonadReader Git3Env m
                                   ) => m [(FilePath, Natural)]

listObjectIndexFiles = do
  path <- indexPath
  dirFiles path
    <&> filter ( ("objects*.idx" ?==) .  takeFileName )
    >>= \fs -> for fs $ \f -> do
           z <- fileSize f  <&> fromIntegral
           pure (f,z)


enumEntries :: forall a m . ( Git3Perks m
                            ) => Index a -> ( BS.ByteString -> m () ) -> m ()

enumEntries Index{..} action = do
  for_ entries $ \IndexEntry{..} -> do
    scanBS entryBS action

enumEntriesFixed :: forall a m . ( Git3Perks m
                            )
                 => Int
                 -> Index a
                 -> ( BS.ByteString -> m () )
                 -> m ()

enumEntriesFixed n Index{..} action = do

  q <- newTQueueIO

  atomically $ mapM_ (writeTQueue q) entries

  replicateM_ n $ do
    fix \next -> do
      es' <- atomically $ tryReadTQueue q
      case es' of
        Nothing -> none
        Just IndexEntry{..} -> do
          scanBS entryBS action
          next

bloomHash :: GitHash  -> [Word32]
bloomHash  gh = [a,b,c,d,e]
    where
      bs = coerce gh
      a = N.word32 (BS.take 4 bs)
      b = N.word32 (BS.take 4 $ BS.drop 4  bs)
      c = N.word32 (BS.take 4 $ BS.drop 8  bs)
      d = N.word32 (BS.take 4 $ BS.drop 12 bs)
      e = N.word32 (BS.take 4 $ BS.drop 16 bs)

bloomFilterSize :: Natural -- ^ elems?
                -> Natural -- ^ hash functions
                -> Double  -- ^ error probability
                -> Natural
bloomFilterSize n k p
    | p <= 0 || p >= 1 = 0
    | otherwise = rnd $ negate (fromIntegral n * fromIntegral k) / log (1 - p ** (1 / fromIntegral k))
  where
    rnd x = 2 ** realToFrac (ceiling (logBase 2 x)) & round

data GitTx =
    TxSegment HashRef
  | TxCheckpoint Natural HashRef

readTxMay :: forall m . ( MonadIO m
                        )
          => AnyStorage -> HashRef -> m (Maybe GitTx)

readTxMay sto href = runMaybeT do

    tx <- getBlock sto (coerce href)
             >>= toMPlus

    RefLogUpdate{..} <- deserialiseOrFail @(RefLogUpdate L4Proto) tx
      & toMPlus

    toMPlus $
     ( deserialiseOrFail (LBS.fromStrict _refLogUpdData) & either (const Nothing) fromAnn )
      <|>
     ( deserialiseOrFail (LBS.fromStrict _refLogUpdData) & either (const Nothing) fromSeq )

  where
    fromAnn = \case
      AnnotatedHashRef _ h -> Just (TxSegment h)

    fromSeq = \case
      (SequentialRef n (AnnotatedHashRef _ h)) -> Just $ TxCheckpoint (fromIntegral n) h

updateReflogIndex :: forall m . ( Git3Perks m
                                , MonadReader Git3Env m
                                , HasClientAPI PeerAPI UNIX m
                                , HasClientAPI RefLogAPI UNIX m
                                , HasStorage m
                                , HasIndexOptions m
                                ) => m ()
updateReflogIndex = do

    reflog <- getGitRemoteKey >>= orThrow Git3ReflogNotSet

    api <- getClientAPI @RefLogAPI @UNIX

    sto <- getStorage

    idx <- openIndex

    written_ <- newTVarIO mempty

    (t1,_) <- timeItT do
      enumEntries idx $ \bs -> do
        let txh = coerce (BS.take 32 $ BS.drop 20  bs) :: HashRef
        atomically $ modifyTVar written_ (HS.insert txh)

    written <- readTVarIO written_
    debug $ "read index at" <+> pretty (realToFrac @_ @(Fixed E2) t1)

    flip runContT pure do

      what' <- lift $ callRpcWaitMay @RpcRefLogGet (TimeoutSec 2) api reflog
                >>= orThrow Git3RpcTimeout

      what <- ContT $ maybe1 what' none

      idxPath <- lift indexPath
      mkdir idxPath

      debug $ "STATE" <+> pretty idxPath

      sink <- S.toList_ do
        walkMerkle (coerce what) (getBlock sto) $ \case
          Left{} -> throwIO MissedBlockError
          Right (hs :: [HashRef]) -> do
            for_ [h | h <- hs, not (HS.member h written)]  $ \h -> void $ runMaybeT do
              readTxMay sto (coerce h) >>= \case
                Nothing -> mzero
                Just (TxCheckpoint{}) -> mzero
                Just (TxSegment href) -> do
                  -- FIXME: error logging
                  chunks <- liftIO (runExceptT (getTreeContents sto href))
                               >>= orThrow MissedBlockError
                               <&> LBS.toChunks

                  what <- toMPlus =<< liftIO do
                    init <- decompress
                    flip fix (init, chunks, mempty :: LBS.ByteString) $ \next -> \case

                      (Consume work, [], o) -> do
                        r1 <- work ""
                        next (r1, [], o)

                      (Consume work, e:es, o) -> do
                        r1 <- work e
                        next (r1, es, o)

                      (Produce piece r, e, o) -> do
                        r1 <- r
                        next (r1, e, LBS.append o (LBS.fromStrict piece))

                      (ZStdS.Done bs, _, o) -> pure (Just (LBS.append o (LBS.fromStrict bs)))

                      (Error _ _, _, _) -> do
                        debug $ "not a valid segment" <+> pretty h
                        pure Nothing

                  guard (LBS.length what > 0)

                  pieces <- S.toList_ $ do
                    void $ runConsumeLBS what $ readLogFileLBS () $ \o _ lbs -> do
                      let (t, llbs) = LBS.splitAt 1 lbs
                      -- notice $ pretty (LBS8.unpack t) <+> pretty o

                      -- FIXME: do-something
                      when (t == "R") do
                        let refs = [ (t,h,x)
                                   | ListVal [LitIntVal t, GitHashLike h, StringLike x]
                                   <- parseTop (LBS8.unpack llbs) & fromRight mempty
                                   ]

                        lift $ S.yield (Left refs)

                      lift $ S.yield (Right o)

                  lift do
                    S.yield (Right (h, rights pieces))
                    S.yield (Left  (h, lefts pieces))

      liftIO $ forConcurrently_ (rights sink) $ \(tx, pieces) -> do
        idxName <- emptyTempFile idxPath "objects-.idx"
        let ss = L.sort pieces
        UIO.withBinaryFileAtomic idxName WriteMode $ \wh -> do
          for_ ss $ \sha1 -> do
            let key   = coerce @_ @N.ByteString sha1
            let value = coerce @_ @N.ByteString tx
            -- notice $ pretty sha1 <+> pretty tx
            writeSection ( LBS.fromChunks [key,value] ) (LBS.hPutStr wh)

      getIndexBlockSize >>= lift . compactIndex

      liftIO do
        name <- emptyTempFile idxPath ".ref"
        UIO.withBinaryFileAtomic name WriteMode $ \wh -> do
          for_ (lefts sink) $ \(tx, refs) -> do
            for_ (mconcat refs) $ \(ts,gh,nm) -> do
              LBS8.hPutStrLn wh $ LBS8.pack $ show $
                  "R" <+> pretty tx
                      <+> pretty ts
                      <+> pretty gh
                      <+> pretty nm

