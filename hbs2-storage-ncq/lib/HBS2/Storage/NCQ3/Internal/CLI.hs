{-# Language ViewPatterns #-}
{-# Language MultiWayIf #-}
module HBS2.Storage.NCQ3.Internal.CLI where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Run
import HBS2.Storage.NCQ3.Internal.Class
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.Index
import HBS2.Storage.NCQ3.Internal.Fossil
import HBS2.Storage.NCQ3.Internal
import HBS2.Storage
import HBS2.Base58
import HBS2.Net.Auth.Credentials

import Data.Config.Suckless.Script

import Control.Monad.Trans.Cont
import Network.ByteOrder qualified as N
import Data.Fixed
import Data.Text.Encoding qualified as TE
import Data.List qualified as List
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import System.IO qualified as IO
import Data.HashMap.Strict (HashMap)
import System.Environment
import UnliftIO

newtype Instance = Instance FilePath
                   deriving newtype (Eq,Ord,Hashable)

type Instances =  TVar (HashMap Instance (NCQStorage, Async ()))

initInstances :: MonadUnliftIO m => m Instances
initInstances = newTVarIO mempty

finalizeInstances :: MonadUnliftIO m => Instances -> m ()
finalizeInstances ins = do
  (storages, threads) <- readTVarIO ins <&> unzip . HM.elems
  mapM_ ncqStorageStop storages
  debug "wait storages to finalize"
  mapM_ wait threads

closeInstance :: MonadUnliftIO m => Instances -> Instance -> m ()
closeInstance ins i = do
  readTVarIO ins <&> HM.lookup i >>= \case
    Nothing -> none
    Just (sto, thread) -> do
      atomically (modifyTVar ins (HM.delete i))
      ncqStorageStop sto
      wait thread


getInstance :: MonadUnliftIO m
            => Instances
            -> Instance
            -> m (NCQStorage, Async ())
getInstance ins i = do
  m <- readTVarIO ins
  case HM.lookup i m of
    Nothing -> newInstance
    Just (sto, th) -> poll th >>= \case
        Nothing -> pure (sto, th)
        Just _  -> do
          atomically $ modifyTVar ins (HM.delete i)
          newInstance

  where
    newInstance = do
      sto <- ncqStorageOpen (coerce i) id
      th  <- async (ncqStorageRun sto)
      atomically $ modifyTVar ins (HM.insert i (sto, th))
      pure (sto, th)

entries :: forall c m . ( MonadUnliftIO m
                        , IsContext c
                        , Exception (BadFormException c)
                        ) => Instances -> MakeDictM c m ()
entries instances = do

  entry $ bindMatch "ncq3:open" $ \case
   [ StringLike p ] -> do
     what <- getInstance instances (Instance p)
     mkOpaque (Instance p)

   e -> throwIO (BadFormException (mkList e))

  entry $ bindMatch "ncq3:close" $ nil_ \case
   [ isOpaqueOf @Instance -> Just inst ] -> do
     closeInstance instances inst

   e -> throwIO (BadFormException (mkList e))

  entry $ bindMatch "ncq3:put" $ \syn -> do
    (inst,bs) <- case syn of
      [ isOpaqueOf @Instance -> Just tcq, isOpaqueOf @ByteString -> Just bs ] -> lift do
        pure (tcq, LBS.fromStrict bs)

      [ isOpaqueOf @Instance -> Just tcq, TextLike s ] -> lift do
        pure (tcq, LBS.fromStrict (TE.encodeUtf8 s))

      e -> throwIO $ BadFormException (mkList e)

    lift do
      (ncq,_) <- getInstance instances inst
      r <- putBlock ncq bs
      pure $ maybe nil (mkSym . show . pretty) r


  entry $ bindMatch "ncq3:get" $ \case
    [ isOpaqueOf @Instance -> Just inst, HashLike hash ] -> lift do
      (ncq,_) <- getInstance instances inst
      getBlock ncq (coerce hash) >>= maybe (pure nil) mkOpaque

    e -> throwIO $ BadFormException (mkList e)

  entry $ bindMatch "ncq3:has" $ \case
    [ isOpaqueOf @Instance -> Just inst, HashLike hash ] -> lift do
      (ncq,_) <- getInstance instances inst
      hasBlock ncq (coerce hash) <&> maybe nil mkInt

    e -> throwIO $ BadFormException (mkList e)

  entry $ bindMatch "ncq3:del" $ nil_ \case
    [ isOpaqueOf @Instance -> Just inst, HashLike hash ] -> lift do
      (ncq,_) <- getInstance instances inst
      delBlock ncq (coerce hash)

    e -> throwIO $ BadFormException (mkList e)


  entry $ bindMatch "ncq3:set:ref" $ \case
    [ isOpaqueOf @Instance -> Just inst, HashLike ref, HashLike val ] -> lift do
      (ncq,_) <- getInstance instances inst
      updateRef ncq (RefAlias2 mempty ref) (coerce val)
      pure nil

    e -> throwIO $ BadFormException (mkList e)

  entry $ bindMatch "ncq3:del:ref" $ \case
    [ isOpaqueOf @Instance -> Just inst, HashLike ref ] -> lift do
      (ncq,_) <- getInstance instances inst
      delRef ncq (RefAlias2 mempty ref)
      pure nil

    e -> throwIO $ BadFormException (mkList e)

  entry $ bindMatch "ncq3:locate" $ \case
    [ isOpaqueOf @Instance -> Just inst, HashLike hash ] -> lift do
      (ncq,_) <- getInstance instances inst
      ncqLocate ncq hash >>= \case
        Just x -> parseSyntax (show $ pretty x) & either (error . show) (pure . fixContext)
        Nothing -> pure nil

    e -> throwIO $ BadFormException (mkList e)


  entry $ bindMatch "ncq3:get:ref" $ \case
    [ isOpaqueOf @Instance -> Just inst, HashLike w ] -> lift do
      (ncq,_) <- getInstance instances inst
      ref <- getRef ncq (RefAlias2 mempty w)
      pure $ maybe nil (mkSym . show . pretty) ref

    e -> throwIO $ BadFormException (mkList e)


  entry $ bindMatch "ncq3:nway:stats" $ \case
    [StringLike fn] -> liftIO do

      mt_    <- newTVarIO 0
      total_ <- newTVarIO 0

      (mmaped,meta@NWayHash{..}) <- nwayHashMMapReadOnly fn
                                     >>= orThrow (NWayHashInvalidMetaData fn)

      let emptyKey = BS.replicate nwayKeySize 0
      nwayHashScanAll meta mmaped $ \_ k _ -> do
        atomically do
          modifyTVar total_ succ
          when (k == emptyKey) do
            modifyTVar mt_ succ

      mt    <- readTVarIO mt_
      total <- readTVarIO total_
      let used  = total - mt
      let ratio = realToFrac @_ @(Fixed E3) (realToFrac used / realToFrac total)

      let stats = mkForm @c "stats"
                    [ mkForm "empty" [mkInt mt]
                    , mkForm "used"  [mkInt used]
                    , mkForm "total" [mkInt total]
                    , mkForm "ratio" [mkDouble ratio]
                    ]

      pure $ mkList [ mkForm "metadata" [mkSyntax meta]
                    , stats
                    ]

    e -> throwIO $ BadFormException (mkList e)

  entry $ bindMatch "ncq3:workdir" $ \syn -> lift do
    path <- case syn of
      [ isOpaqueOf @Instance -> Just inst ] -> pure (coerce inst)
      [ StringLike path ]                   -> pure path
      e -> throwIO $ BadFormException (mkList e)

    sto <- ncqStorageOpen path id
    pure (mkSym (ncqGetWorkDir sto))

  entry $ bindMatch "ncq3:states" $ \syn -> lift do
    path <- case syn of
      [ isOpaqueOf @Instance -> Just inst ] -> pure (coerce inst)
      [ StringLike path ]                   -> pure path
      e -> throwIO $ BadFormException (mkList e)

    sto <- ncqStorageOpen path id
    fs  <- dirFiles (ncqGetWorkDir sto) <&> filter (List.isPrefixOf "s-" . takeFileName)
    pure (mkList (fmap mkSym fs))


  entry $ bindMatch "ncq3:indexes" $ \syn -> lift do
    path <- case syn of
      [ isOpaqueOf @Instance -> Just inst ] -> pure (coerce inst)
      [ StringLike path ]                   -> pure path
      e -> throwIO $ BadFormException (mkList e)

    sto <- ncqStorageOpen path id
    fs  <- dirFiles (ncqGetWorkDir sto) <&> filter (List.isPrefixOf "i-" . takeFileName)
    pure (mkList (fmap mkSym fs))


  entry $ bindMatch "ncq3:datafiles" $ \syn -> lift do
    path <- case syn of
      [ isOpaqueOf @Instance -> Just inst ] -> pure (coerce inst)
      [ StringLike path ]                   -> pure path
      e -> throwIO $ BadFormException (mkList e)

    sto <- ncqStorageOpen path id
    fs  <- dirFiles (ncqGetWorkDir sto) <&> filter (List.isPrefixOf "f-" . takeFileName)
    pure (mkList (fmap mkSym fs))

  entry $ bindMatch "ncq3:index:scan" $ \syn -> liftIO do
    (sto, files) <- case syn of
      [ StringLike path, StringLike x ] -> do
        sto <- ncqStorageOpen path id
        pure (sto, [ncqGetFileName sto (takeFileName x)])

      [ StringLike path, LitIntVal idx ] -> do
        sto <- ncqStorageOpen path id
        let fn = ncqGetFileName sto (IndexFile (FileKey (fromIntegral idx)))
        pure (sto, [fn])

      [ StringLike path ] -> do
        sto <- ncqStorageOpen path id
        fs  <- dirFiles (ncqGetWorkDir sto)
        let idxs = filter (List.isPrefixOf "i-" . takeFileName) fs
        pure (sto, List.sortOn Down idxs)

      e -> throwIO $ BadFormException (mkList e)

    forM_ files \fn -> do
      mres <- nwayHashMMapReadOnly fn
      case mres of
        Nothing -> err ("can't mmap index " <> pretty fn)
        Just (bs,nw) -> do
          nwayHashScanAll nw bs $ \_ k v ->
            unless (k == emptyKey) do
              let IndexEntry fk off sz = unpackIndexEntry v
              print $      fill 6 (pretty (fromString @FileKey (takeBaseName fn)))
                       <+> fill 44 (pretty (coerce @_ @HashRef k))
                       <+> fill 4 (pretty fk)
                       <+> fill 8 (pretty off)
                       <+> pretty sz

    pure nil

  entry $ bindMatch "ncq3:index:lookup" $ \syn -> liftIO do
    (sto, h, files) <- case syn of

      [ StringLike path, LitIntVal idx, HashLike h ] -> do
        sto <- ncqStorageOpen path id
        let fn = ncqGetFileName sto (IndexFile (FileKey (fromIntegral idx)))
        pure (sto, h, [fn])

      [ StringLike path, StringLike fname, HashLike h ] -> do
        sto <- ncqStorageOpen path id
        pure (sto, h, [ncqGetFileName sto (takeFileName fname)])

      [ StringLike path, HashLike h ] -> do
        sto <- ncqStorageOpen path id
        fs  <- dirFiles (ncqGetWorkDir sto)
        let idxs = filter (List.isPrefixOf "i-" . takeFileName) fs
        pure (sto, h, List.sortOn Down idxs)

      e -> throwIO $ BadFormException (mkList e)

    forM_ files \fn -> do
      mres <- nwayHashMMapReadOnly fn
      case mres of
        Nothing -> err ("can't mmap index " <+> pretty fn)
        Just (bs,nw) -> do
          mval <- nwayHashLookup nw bs (coerce h)
          case mval of
            Nothing -> pure ()
            Just entryBs -> do
              let IndexEntry fk off sz = unpackIndexEntry entryBs
              print $
                   fill 44 (pretty h)
               <+> fill 6  (pretty fk)
               <+> fill 10 (pretty off)
               <+> pretty sz

    pure nil

  entry $ bindMatch "ncq3:index:find" $ \syn -> liftIO do
    (sto, hash, files) <- case syn of

      -- путь, хэш → все индексы
      [ StringLike path, HashLike h ] -> do
        sto <- ncqStorageOpen path id
        fs  <- dirFiles (ncqGetWorkDir sto)
        let idxs = filter (List.isPrefixOf "i-" . takeFileName) fs
        pure (sto, h, List.sortOn Down idxs)

      -- путь, хэш, имя файла
      [ StringLike path, HashLike h, StringLike x ] -> do
        sto <- ncqStorageOpen path id
        pure (sto, h, [ncqGetFileName sto (takeFileName x)])

      -- путь, хэш, индекс по номеру
      [ StringLike path, HashLike h, LitIntVal idx ] -> do
        sto <- ncqStorageOpen path id
        let fn = ncqGetFileName sto (IndexFile (FileKey (fromIntegral idx)))
        pure (sto, h, [fn])


      e -> throwIO $ BadFormException (mkList e)

    forM_ files \fn -> do
      mres <- nwayHashMMapReadOnly fn
      case mres of
        Nothing -> err ("can't mmap index " <+> pretty fn)
        Just (bs,nw) -> do
          nwayHashScanAll nw bs $ \_ k v ->
            unless (k == emptyKey) do
              when (coerce @_ @HashRef k == hash) do
                let IndexEntry fk off sz = unpackIndexEntry v
                print $      fill 6 (pretty (fromString @FileKey (takeBaseName fn)))
                         <+> fill 44 (pretty (coerce @_ @HashRef k))
                         <+> fill 4 (pretty fk)
                         <+> fill 8 (pretty off)
                         <+> pretty sz

    pure nil

  entry $ bindMatch "ncq3:data:scan" $ \syn -> liftIO do
    (sto, files) <- case syn of

      [ StringLike path, StringLike x ] -> do
        sto <- ncqStorageOpen path id
        pure (sto, [ncqGetFileName sto (takeFileName x)])

      [ StringLike path, LitIntVal idx ] -> do
        sto <- ncqStorageOpen path id
        let fn = ncqGetFileName sto (DataFile (FileKey (fromIntegral idx)))
        pure (sto, [fn])

      [ StringLike path ] -> do
        sto <- ncqStorageOpen path id
        fs  <- dirFiles (ncqGetWorkDir sto)
        let dfs = filter (List.isPrefixOf "f-" . takeFileName) fs
        pure (sto, List.sortOn Down dfs)

      e -> throwIO $ BadFormException (mkList e)

    forM_ files \fn -> do
      let fk = fromString @FileKey (takeBaseName fn)
      ncqStorageScanDataFile sto fn $ \offset w key val -> do
        print $
              fill 6  (pretty fk)
          <+> fill 10 (pretty offset)
          <+> fill 8  (pretty (w + ncqSLen))
          <+> fill 44 (pretty (coerce @_ @HashRef key))
          <+> prettyTag val

    pure nil

  entry $ bindMatch "ncq3:data:find" $ \syn -> liftIO do
    (sto, h, files) <- case syn of

      [ StringLike path, HashLike h, StringLike x ] -> do
        sto <- ncqStorageOpen path id
        pure (sto, h, [ncqGetFileName sto (takeFileName x)])

      [ StringLike path, HashLike h, LitIntVal idx ] -> do
        sto <- ncqStorageOpen path id
        let fn = ncqGetFileName sto (DataFile (FileKey (fromIntegral idx)))
        pure (sto, h, [fn])

      [ StringLike path, HashLike h ] -> do
        sto <- ncqStorageOpen path id
        fs  <- dirFiles (ncqGetWorkDir sto)
        let dfs = filter (List.isPrefixOf "f-" . takeFileName) fs
        pure (sto, h, List.sortOn Down dfs)

      e -> throwIO $ BadFormException (mkList e)

    forM_ files \fn -> do
      let fk = fromString @FileKey (takeBaseName fn)
      ncqStorageScanDataFile sto fn $ \offset w key val -> do
        when (coerce @_ @HashRef key == h) do
          print $
                fill 6  (pretty fk)
            <+> fill 10 (pretty offset)
            <+> fill 8  (pretty (w + ncqSLen))
            <+> fill 44 (pretty (coerce @_ @HashRef key))
            <+> prettyTag val

    pure nil


  entry $ bindMatch "ncq3:data:read" $ \syn -> liftIO do
    (fn, offset) <- case syn of

      -- путь + индекс + offset
      [ StringLike path, LitIntVal idx, LitIntVal off ] -> do
        sto <- ncqStorageOpen path id
        let fn = ncqGetFileName sto (DataFile (FileKey (fromIntegral idx)))
        pure (fn, fromIntegral off)

      -- путь + имя файла + offset
      [ StringLike path, StringLike fname, LitIntVal off ] -> do
        sto <- ncqStorageOpen path id
        let fn = ncqGetFileName sto (takeFileName fname)
        pure (fn, fromIntegral off)

      e -> throwIO $ BadFormException (mkList e)

    -- mmap файла
    bs <- mmapFileByteString fn Nothing

    -- читаем первые 4 байта для размера
    let beSize = BS.take 4 (BS.drop offset bs)
        size   = N.word32 beSize

    -- вырезаем всю запись целиком
    let record = BS.take (fromIntegral (size + ncqSLen)) (BS.drop offset bs)

    mkOpaque record

  entry $ bindMatch "ncq3:data:unpack" $ \syn -> lift do
    case syn of
      [ SymbolVal x, isOpaqueOf @ByteString -> Just bs ] -> do
        let raw = bs
        let (k, e) = ncqEntryUnwrap bs
        let val = either id snd e
        case x of
          "key"  -> pure (mkSym (show (pretty (coerce @_ @HashRef k))))

          "size" -> pure $ mkInt (BS.length raw)

          "tag"  -> pure (mkSym (show (prettyTag' e)))

          "prefix" -> do
            pure (mkStr $ BS8.unpack $ BS.take ncqPrefixLen $ BS.drop (ncqSLen + ncqKeyLen) raw)

          "value"      -> mkOpaque val
          "value:hex"  -> pure $ mkStr (show $ AsHex val)
          "value:b58"  -> pure $ mkStr (show $ AsBase58 val)

          _ -> pure nil

      e -> throwIO $ BadFormException (mkList e)


  entry $ bindMatch "ncq3:audit:scan" $ nil_ \case
    [ StringLike path ] -> flip runContT pure $ callCC \exit -> do
      sto <- ncqStorageOpen path id
      let fn = ncqGetFileName sto AuditFile
      here <- doesFileExist fn

      liftIO $ print $ "WYF?" <+> pretty fn

      let readField n bs = if BS.length bs < n then Left bs else Right (BS.take n bs, BS.drop n bs)

      unless here do
        err $ pretty (toFileName AuditFile) <+> "not found"
        exit ()

      mmaped <- liftIO $ mmapFileByteString fn Nothing

      void $ flip runContT pure $ callCC \stop -> do
        flip fix mmaped $ \next bs ->  do

          (s, rest) <- case readField 4 bs of
                         Left  r -> stop (Left r)
                         Right s -> pure s

          (k, rest1) <- case readField ncqKeyLen rest of
                         Left  r -> stop (Left r)
                         Right s -> pure s

          (p, rest2) <- case readField ncqPrefixLen rest1 of
                         Left  r -> stop (Left r)
                         Right s -> pure s

          let t = if | p == ncqBlockPrefix -> Just B
                     | p == ncqRefPrefix   -> Just R
                     | p == ncqTombPrefix  -> Just T
                     | otherwise           -> Nothing

          case t of
            Just B -> do
              liftIO $ IO.hPrint stdout ("B" <+> pretty (coerce @_ @HashRef k))
              next rest2

            Just T -> do
              liftIO $ IO.hPrint stdout ("T" <+> pretty (coerce @_ @HashRef k))
              next rest2

            Just R -> do

              (v, rest3) <- case readField ncqKeyLen rest2 of
                             Left  r -> stop (Left r)
                             Right s -> pure s

              liftIO $ IO.hPrint stdout ("R" <+> pretty (coerce @_ @HashRef k) <+> pretty (coerce @_ @HashRef v))
              next rest3

            _ -> do
              liftIO $ IO.hPrint stdout ("E" <+> "audit file damaged or incomplete")
              pure $ Left rest2

    e -> throwIO $ BadFormException (mkList e)


printDataEntry :: MonadUnliftIO m => NCQOffset -> NCQSize -> HashRef -> ByteString -> m ()
printDataEntry offset size key val = do
  liftIO $ print $
        fill 10 (pretty offset)
    <+> fill 8  (pretty size)
    <+> fill 44 (pretty key)
    <+> prettyTag val

prettyTag x = case ncqEntryUnwrapValue x of
    Left _              -> pretty ("E" :: String)
    Right (meta, _)     -> pretty meta

prettyTag' :: Either ByteString (NCQSectionType, ByteString) -> Doc a
prettyTag' = \case
    Left _              -> pretty ("E" :: String)
    Right (meta, _)     -> pretty meta

