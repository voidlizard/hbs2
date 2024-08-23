{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
{-# Language MultiWayIf #-}
module HBS2.Sync.State where

import HBS2.Sync.Prelude

import HBS2.System.Dir
import HBS2.Data.Types.SignedBox
import HBS2.Merkle
import HBS2.Data.Detect
import HBS2.Merkle.MetaData
import HBS2.Net.Auth.GroupKeySymm as Symm
import HBS2.Storage.Compact as Compact
import HBS2.Storage.Operations.Class
import HBS2.Storage.Operations.ByteString

import HBS2.Peer.Proto.RefChan
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.RefChan as Client

import HBS2.CLI.Run.MetaData (createTreeWithMetadata)

import DBPipe.SQLite
import Data.Config.Suckless.Script.File

import Control.Concurrent.STM (flushTQueue)
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.Fixed
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word
import Lens.Micro.Platform
import Streaming.Prelude qualified as S
import System.TimeIt
import System.Directory hiding (doesFileExist,doesDirectoryExist)
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO.IO.File qualified as UIO


{- HLINT ignore "Functor law" -}
{- HLINT ignore "Eta reduce" -}

data EntryType = File | Dir | Tomb
                 deriving stock (Eq,Ord,Show,Data,Generic)

data EntryDesc =
  EntryDesc
  { entryType        :: EntryType
  , entryTimestamp   :: Word64
  , entryRemoteHash  :: Maybe HashRef
  }
  deriving stock (Eq,Ord,Show,Data,Generic)

data Entry =
  DirEntry EntryDesc FilePath
  deriving stock (Eq,Ord,Show,Data,Generic)


instance Serialise Entry
instance Serialise EntryType
instance Serialise EntryDesc

instance IsContext c => ToSexp c EntryType where
  toSexp a = mkStr @c $ Text.toLower $ Text.pack $ show a

instance IsContext c => ToSexp c EntryDesc where
  toSexp EntryDesc{..} = case entryType of
    File -> mkForm @c  "F"  [mkInt entryTimestamp, hash]
    Dir  ->  mkForm @c "D " [mkInt entryTimestamp, hash]
    Tomb ->  mkForm @c "T " [mkInt entryTimestamp, hash]

    where
      hash = case entryRemoteHash of
        Nothing -> nil
        Just x  -> mkStr (show $ pretty x)

instance IsContext c => ToSexp c Entry where
  toSexp (DirEntry w p) = mkForm @c "entry" [toSexp w, mkStr p]

pattern WithRemoteHash :: Entry -> HashRef -> Entry
pattern WithRemoteHash e h <- e@(DirEntry (EntryDesc {entryRemoteHash = Just h}) _)

pattern TombEntry :: Entry -> Entry
pattern TombEntry e <- e@(DirEntry (EntryDesc { entryType = Tomb }) _)

pattern FileEntry :: Entry -> Entry
pattern FileEntry e <- e@(DirEntry (EntryDesc { entryType = File }) _)

pattern UpdatedFileEntry :: Word64 -> Entry -> Entry
pattern UpdatedFileEntry t e <- e@(DirEntry (EntryDesc { entryType = File
                                                       , entryRemoteHash = Nothing
                                                       , entryTimestamp = t }) _)



makeTomb :: Word64 -> FilePath -> Maybe HashRef -> Entry
makeTomb t n h = DirEntry (EntryDesc Tomb t h) n

entryPath :: Entry -> FilePath
entryPath (DirEntry _ p) = p

getEntryTimestamp :: Entry -> Word64
getEntryTimestamp (DirEntry d _) = entryTimestamp d

getEntryHash :: Entry -> Maybe HashRef
getEntryHash (DirEntry d _) = entryRemoteHash d

isFile :: Entry -> Bool
isFile = \case
  DirEntry (EntryDesc { entryType = File}) _ -> True
  _ -> False

isTomb :: Entry -> Bool
isTomb = \case
  DirEntry (EntryDesc { entryType = Tomb}) _ -> True
  _ -> False

isDir :: Entry -> Bool
isDir = \case
  DirEntry (EntryDesc { entryType = Dir}) _ -> True
  _ -> False

entriesFromLocalFile :: MonadUnliftIO m => FilePath -> FilePath -> m (Map FilePath Entry)
entriesFromLocalFile prefix fn' = do
  let fn0 = removePrefix prefix fn
  ts <- getFileTimestamp fn
  pure $ entriesFromFile Nothing ts fn0
  where
    fn = normalise fn'

entriesFromFile :: Maybe HashRef -> Word64 -> FilePath -> Map FilePath Entry
entriesFromFile h ts fn0 = do
  let dirs = splitDirectories (dropFileName fn0)
               & dropWhile (== ".")
  let es = flip L.unfoldr ("",dirs) $ \case
              (_,[])   -> Nothing
              (p,d:ds) -> Just (dirEntry (p </> d), (p </> d, ds) )

  Map.fromList [ (p, e)
               | e@(DirEntry _ p) <- fileEntry fn0 : es
               ]
  where
    dirEntry p   = DirEntry (EntryDesc Dir ts Nothing) p
    fileEntry p  = DirEntry (EntryDesc File ts h) p

-- NOTE: getStateFromDir
--  что бы устранить противоречия в "удалённом" стейте и
--  локальном, мы должны о них узнать
--
--  Основное противоречие это file <=> dir
--  Так как мы не сохраняем каталоги, а только файлы
--  Каталоги выводим из файлов (таким образом, пустые каталоги будут игнорироваться)
--
--  Допустим, у нас есть файл, совпадающий по имени с каталогом в remote state
--  Мы должны тогда вывести этот каталог из remote state и проверить,
--  чем он является тут (каталогом или файлом)
--
--  Тогда функция устранения противоречий сможет что-то с этим сделать
--  впоследствии
--


getStateFromDir0 :: ( MonadUnliftIO m
                   , HasClientAPI RefChanAPI UNIX m
                   , HasClientAPI StorageAPI UNIX m
                   , HasStorage m
                   , HasRunDir m
                   , HasCache m
                   )
                => Bool
                -> m [(FilePath, Entry)]
getStateFromDir0 seed = do

  dir <- getRunDir

  env <- getRunDirEnv dir >>= orThrow DirNotSet

  let excl = view dirSyncExclude env
  let incl = view dirSyncInclude env

  getStateFromDir seed dir incl excl

getStateFromDir :: ( MonadUnliftIO m
                   , HasClientAPI RefChanAPI UNIX m
                   , HasClientAPI StorageAPI UNIX m
                   , HasStorage m
                   , HasRunDir m
                   , HasCache m
                   )
                => Bool           -- ^ use remote state as seed
                -> FilePath       -- ^ dir
                -> [FilePattern]  -- ^ include pattern
                -> [FilePattern]  -- ^ exclude pattern
                -> m [(FilePath, Entry)]
getStateFromDir seed path incl excl = do
  es' <- S.toList_ $ do
    glob incl excl path  $ \fn -> do
      let fn0 = removePrefix path fn
      es <- liftIO (entriesFromLocalFile path fn)
      -- debug $ yellow "file" <+> viaShow ts <+> pretty fn0
      S.each es
      pure True

  let es0 = [ (entryPath e, e) | e <- es' ]

  if not seed then do
    pure es0
  else  do
   dir   <- getRunDir
   fromMaybe es0 <$> runMaybeT do
     env <- getRunDirEnv dir >>= toMPlus
     rchan <- view dirSyncRefChan env & toMPlus
     es2 <- lift $ getStateFromRefChan rchan

     S.toList_ do
         S.each es0
         for_ es2 $ \(p, e) -> do
           d <- doesDirectoryExist (path </> p)
           when d do
            ts <- liftIO $ getFileTimestamp  (path </> p)
            S.yield (p, DirEntry (EntryDesc Dir ts mzero) p)

           S.yield (p,e)


getStateFromRefChan :: forall m . ( MonadUnliftIO  m
                                  , HasClientAPI RefChanAPI UNIX m
                                  , HasClientAPI StorageAPI UNIX m
                                  , HasStorage m
                                  , HasRunDir m
                                  , HasCache m
                                  )
                    => MyRefChan
                    -> m [(FilePath, Entry)]
getStateFromRefChan rchan = do

  dir <- getRunDir

  sto <- getStorage

  rch <- Client.getRefChanHead @UNIX rchan
               >>= orThrow RefChanHeadNotFoundException

  let statePath = dir </> ".hbs2-sync" </> "state"

  db <- newDBPipeEnv dbPipeOptsDef (statePath </> "state.db")

  flip runContT pure do

    void $ ContT $ bracket (async (runPipe db)) cancel

    withDB db $ do
      ddl [qc|create table if not exists entry (txhash text not null primary key, s blob not null)|]
      ddl [qc|create table if not exists seen (txhash text not null primary key)|]
      commitAll

    debug $ red "getStateFromRefChan" <+> pretty (AsBase58 rchan)

    outq <- newTQueueIO
    seen <- newTQueueIO
    tss <- newTVarIO mempty

    let members = view refChanHeadReaders rch & HS.toList

    krl <- liftIO $ runKeymanClient $ loadKeyRingEntries members
                <&> L.sortOn (Down . fst)
                <&> fmap snd

    let krs = HM.fromList [ (pk,e) | e@(KeyringEntry pk _ _) <- krl ]

    let findKey gk = do
          r <- S.toList_ do
                forM_ (HM.toList $ recipients gk) $ \(pk,box) -> runMaybeT do
                  (KeyringEntry ppk ssk _) <- toMPlus $ HM.lookup pk krs
                  let s = Symm.lookupGroupKey @'HBS2Basic ssk ppk gk
                  for_ s $ lift . S.yield
          pure $ headMay r

    -- let check hx = pure True
    hseen <- withDB db (select_ [qc|select txhash from seen|])
               <&> fmap ((fromStringMay @HashRef) . fromOnly)
               <&> HS.fromList . catMaybes

    let check hx = pure $ not $ HS.member hx hseen

    -- FIXME: may-be-slow
    (a, _) <- timeItT do
      lift $ walkRefChanTx @UNIX check rchan $ \txh u -> do

        atomically $ writeTQueue seen txh

        case  u of

          A (AcceptTran ts _ what) -> do
            -- debug $ red "ACCEPT" <+> pretty ts <+> pretty what
            for_ ts $ \w -> do
              atomically $ modifyTVar tss (HM.insertWith max what (coerce @_ @Word64 w))

          P orig (ProposeTran _ box) -> void $ runMaybeT do
            (_, bs) <- unboxSignedBox0 box & toMPlus
            AnnotatedHashRef w href <- deserialiseOrFail @AnnotatedHashRef (LBS.fromStrict bs)
                                        & toMPlus . either (const Nothing) Just


            runExceptT (extractMetaData @'HBS2Basic findKey sto href)
             >>= either (const none) ( \meta -> atomically $ writeTQueue outq (orig, ((href, meta), txh)) )

    notice $ "walkRefChanTx complete in" <+> pretty (realToFrac a :: Fixed E6)

    trees <- atomically (flushTQueue outq)

    tsmap <- readTVarIO tss

    lift $ withDB db $ transactional do

      -- ess0 <- S.toList_ do
      for_ trees $ \(txh, ((tree, meta),txxx)) -> do
        let what = parseTop meta & fromRight mempty
        let loc  = headDef "" [ l | ListVal [StringLike "location:", StringLike l] <- what ]

        void $ runMaybeT do
          fn   <- toMPlus $ headMay [ l | ListVal [StringLike "file-name:", StringLike l] <- what ]
          ts   <- toMPlus $ HM.lookup txh tsmap
          let tomb = or [ True | TombLikeOpt <- what  ]
          let fullPath = loc </> fn

          trace $ red "META" <+> pretty what

          if tomb then do
            let r = Map.singleton fullPath (makeTomb ts fullPath (Just tree))
            withDB db do
              insert [qc| insert into entry (txhash,s) values(?,?)
                          on conflict (txhash) do update set s = excluded.s
                        |] (show $ pretty $ txxx, serialise r)

          else do
            let r = entriesFromFile (Just tree) ts fullPath

            withDB db do
              insert [qc| insert into entry (txhash,s) values(?,?)
                          on conflict (txhash) do update set s = excluded.s
                        |] (show $ pretty $ txxx, serialise r)

              -- lift $ S.yield r

      seenTx <- atomically $ flushTQueue seen
      for_ seenTx $ \txh -> do
        insert [qc| insert into seen (txhash)
                    values(?) on conflict do nothing
                  |] (Only (show $ pretty $ txh))

    ess0 <- withDB db do
           select_ [qc|select s from entry|]
             <&> fmap (deserialiseOrFail @(Map FilePath Entry) . LBS.fromStrict . fromOnly)
             <&> rights

    let r = Map.unionsWith merge ess0

    pure (Map.toList r)

  -- pure $ Map.toList $ Map.unionsWith merge ess0

findDeleted :: (MonadIO m, HasRunDir m, HasTombs m) => m [Merged]
findDeleted = do

  dir <- getRunDir

  now <- liftIO $ getPOSIXTime <&> round

  tombs <- getTombs
  -- TODO: check-if-non-latin-filenames-work
  --   resolved: ok
  seen <- Compact.keys tombs
            <&> fmap (deserialiseOrFail @FilePath . LBS.fromStrict)
            <&> rights

  S.toList_ do
    for_ seen $ \f0 -> do

      let path = dir </> f0

      here <- liftIO $ doesFileExist path

      n <- Compact.getValEither @Integer tombs f0
            <&> fromRight (Just 0)

      when (not here && isJust n) do
        S.yield (D (f0, makeTomb now f0 Nothing) n)
        trace $ "found deleted" <+> pretty n <+> pretty f0


postEntryTx :: ( MonadUnliftIO m
               , HasStorage m
               , HasRunDir m
               , HasClientAPI StorageAPI UNIX m
               , HasClientAPI RefChanAPI UNIX m
               )
            => Maybe (LBS.ByteString)
            -> Maybe (GroupKey 'Symm 'HBS2Basic)
            -> MyRefChan
            -> FilePath
            -> Entry
            -> m ()
postEntryTx nonce' mgk refchan path entry = do

  sto <- getStorage

  env <- getRunDirEnv path >>= orThrow DirNotSet

  creds <- view dirSyncCreds env & orThrow DirNotSet

  rch <- Client.getRefChanHead @UNIX refchan
               >>= orThrow RefChanHeadNotFoundException

  void $ runMaybeT do

    guard (isFile entry || isTomb entry)

    let p = entryPath entry
    lbs <- if isTomb entry then do pure mempty
           else
             -- FIXME: dangerous!
             liftIO (LBS.readFile (path </> p))

    let (dir,file) = splitFileName p

    let meta = HM.fromList [ ("file-name", fromString file)
                           ]
                <> case dir of
                     "./" -> mempty
                     d    -> HM.singleton "location" (fromString d)
                <> if not (isTomb entry) then HM.empty
                   else HM.singleton "tomb" "#t"

    let members = view refChanHeadReaders rch & HS.toList

    -- FIXME: support-unencrypted?
    when (L.null members) do
      throwIO EncryptionKeysNotDefined

    let rcpt    = maybe mempty (HM.keys . recipients) mgk

    gk <- case (members == rcpt, mgk)  of
            (True, Just g)  -> pure g
            (False,_)  -> do
               sec <- runMaybeT $
                         toMPlus mgk >>=  liftIO . runKeymanClient . extractGroupKeySecret >>= toMPlus

               case sec of
                Just s  -> Symm.generateGroupKey @'HBS2Basic (Just s) members
                Nothing -> Symm.generateGroupKey @'HBS2Basic Nothing members

            _ -> Symm.generateGroupKey @'HBS2Basic Nothing members

    -- FIXME: survive-this-error?
    href <- lift $ createTreeWithMetadata sto (Just gk) meta lbs
               >>= orThrowPassIO

    let tx = AnnotatedHashRef Nothing href
    let spk = view peerSignPk creds
    let ssk = view peerSignSk creds

    -- FIXME: remove-nonce
    --   пока что будем постить транзакцию всегда.
    --   в дальнейшем стоит избавиться от нонса
    nonce <- liftIO $ getPOSIXTime <&> round <&> BS.take 6 . coerce  . hashObject @HbSync . serialise
    let box = makeSignedBox @HBS2Basic spk ssk (LBS.toStrict $ serialise tx <> LBS.fromStrict nonce)

    notice $ red "post tree tx" <+> pretty p <+> pretty href

    lift $ postRefChanTx @UNIX refchan box


merge :: Entry -> Entry -> Entry
merge a b = do
  if | getEntryTimestamp a > getEntryTimestamp b -> a

     | isFile a && isDir b -> a

     | isFile b && isDir a -> b

     | getEntryTimestamp a == getEntryTimestamp b ->
        case (getEntryHash a, getEntryHash b) of
           (Nothing,Nothing) -> b
           (Just _,Nothing) -> a
           (Nothing,Just _) -> b
           (Just _, Just _) -> b

     | otherwise -> b


data Merged = N (FilePath, Entry)
            | E (FilePath, Entry)
            | D (FilePath, Entry) (Maybe Integer)
            | M (FilePath,FilePath,Entry)
{-# COMPLETE N,E,M,D #-}

pattern MergedEntryType :: EntryType -> Merged
pattern MergedEntryType t <- ( mergedEntryType -> t )

mergedEntryType :: Merged -> EntryType
mergedEntryType = \case
  N (_,DirEntry d _)   -> entryType d
  E (_,DirEntry d _)   -> entryType d
  D (_,DirEntry d _) _ -> entryType d
  M (_,_,DirEntry d _) -> entryType d

instance (IsContext c) => ToSexp c Integer where
  toSexp i = mkInt i

instance (IsContext c, ToSexp c a) => ToSexp c (Maybe a) where
  toSexp = \case
    Nothing -> nil
    Just x -> toSexp x

instance IsContext c => ToSexp c Merged where
  toSexp = \case
    N (_, e)     -> mkForm @c "N" [toSexp e]
    E (_, e)     -> mkForm @c "E" [toSexp e]
    D (_, e) i   -> mkForm @c "D" [toSexp e, toSexp i]
    M (o, t, e)  -> mkForm @c "M" [toSexp e,mkSym o,mkSym t]

mergeState :: MonadUnliftIO m
           => [Merged]
           -> [(FilePath, Entry)]
           -> m [Merged]

mergeState seed orig = do

  let deleted = [ (p,d) | d@(D (p,e) c) <- seed, isTomb e, c < Just 1 ] & Map.fromList

  let dirs = [ (p,e) | (p,e) <- orig, isDir e ] & Map.fromListWith merge

  let files  = [ (p, e) | D (p,e) _ <- Map.elems deleted]
               <> [ (p,e) | (p,e) <- orig, isFile e ]
               & Map.fromListWith merge
               -- & Map.filterWithKey (\k ( -> not (Map.member k deleted))

  let tombs  = [ (p,e) | (p,e) <- orig, isTomb e ] & Map.fromListWith merge

  let names = Map.keysSet (dirs <> files)

  now <- liftIO $ getPOSIXTime <&> round

  S.toList_ do
    for_ (Map.toList files) $ \(p,e@(DirEntry d _)) -> do
      if
       | Map.member p deleted -> do
           for_ (Map.lookup p deleted) S.yield

       | Map.member p dirs -> do
           let new = uniqName names p
           S.yield $ M (p, new, DirEntry d new)
           S.yield $ N (p, makeTomb now p Nothing)

       | Map.member p tombs -> do
          let tomb = Map.lookup p tombs
          case tomb of
            Just t | getEntryTimestamp t >= getEntryTimestamp e -> do
              S.yield $ E (p,t)

            _ -> S.yield $ E (p,e)

       | not (Map.member p deleted) -> do
          S.yield $ E (p,e)

       | otherwise -> none

  where
    uniqName names0 name = do

      flip fix (names0,0) $ \next (names,n) -> do
        let suff = hashObject @HbSync (serialise (names, name, n))
                    & pretty & show & drop 2 & take 4
        let new = name <> "~" <> suff
        if Set.member new names then
          next (Set.insert new names, succ n)
        else
          new

getTreeContents :: ( MonadUnliftIO m
                   , MonadError OperationError m
                   )
                => AnyStorage
                -> HashRef
                -> m LBS.ByteString

getTreeContents sto href = do

  blk <- getBlock sto (coerce href)
           >>= orThrowError MissedBlockError

  let q = tryDetect (coerce href) blk

  case q of

    MerkleAnn (MTreeAnn {_mtaCrypt = NullEncryption }) -> do
      readFromMerkle sto (SimpleKey (coerce href))

    MerkleAnn ann@(MTreeAnn {_mtaCrypt = EncryptGroupNaClSymm gkh _}) -> do

      rcpts  <- Symm.loadGroupKeyMaybe @'HBS2Basic sto (HashRef gkh)
                  >>= orThrowError (GroupKeyNotFound 11)
                  <&> HM.keys . Symm.recipients

      kre <- runKeymanClient do
                loadKeyRingEntries rcpts <&> fmap snd

      readFromMerkle sto (ToDecryptBS (coerce href) (findSecretDefault kre))

    _ -> throwError UnsupportedFormat


runDirectory :: ( IsContext c
                , SyncAppPerks m
                , HasClientAPI RefChanAPI UNIX m
                , HasClientAPI StorageAPI UNIX m
                , HasStorage m
                , HasRunDir m
                , HasTombs m
                , HasCache m
                , Exception (BadFormException c)
                ) =>  RunM c m ()
runDirectory = do

  path <- getRunDir

  runDir
    `catch` \case
      RefChanNotSetException -> do
        err $ "no refchan set for" <+> pretty path
      RefChanHeadNotFoundException -> do
        err $ "no refchan head found for" <+> pretty path
      EncryptionKeysNotDefined -> do
        err $ "no readers defined in the refchan for " <+> pretty path
      SignKeyNotSet -> do
        err $ "sign key not set or not found " <+> pretty path
      DirNotSet -> do
        err $ "directory not set"

    `catch` \case
      (e :: OperationError) -> do
        err $ viaShow e

    `finally` do
      closeTombs
      closeCache

  where


    writeEntry path e = do

      let p = entryPath e
      let filePath = path </> p

      sto <- getStorage

      tombs <- getTombs

      void $ runMaybeT do

        dir <- getRunDir
        backup <- getRunDirEnv dir
                     <&> fmap (view dirSyncBackup)
                     <&> fromMaybe False

        h <- getEntryHash e & toMPlus

        unless backup do

          notice $ green "write" <+> pretty h <+> pretty p

          lbs <- lift (runExceptT (getTreeContents sto h))
                   >>= toMPlus

          mkdir (dropFileName filePath)

          liftIO $ UIO.withBinaryFileAtomic filePath WriteMode $ \fh -> do
            LBS.hPutStr fh lbs >> hFlush fh

          let ts  = getEntryTimestamp e
          let timestamp = posixSecondsToUTCTime (fromIntegral ts)

          liftIO $ setModificationTime (path </> p) timestamp

        lift $ Compact.putVal tombs p (0 :: Integer)

    runDir = do

      sto <- getStorage

      path <- getRunDir

      env <- getRunDirEnv path >>= orThrow DirNotSet

      refchan <- view dirSyncRefChan env & orThrow RefChanNotSetException

      fetchRefChan @UNIX refchan

      -- FIXME: multiple-directory-scans

      local  <- getStateFromDir0 True

      let hasRemoteHash = [ (p, h) | (p, WithRemoteHash e h)  <- local]

      hasGK0 <- HM.fromList <$> S.toList_ do
                  for_ hasRemoteHash $ \(p,h) -> do
                     mgk0 <- lift $ loadGroupKeyForTree @HBS2Basic sto h
                     for_ mgk0 $ \gk0 -> S.yield (p,gk0)

      deleted <- findDeleted

      merged <- mergeState deleted local

      rch <- Client.getRefChanHead @UNIX refchan
                   >>= orThrow RefChanHeadNotFoundException

      let filesLast m = case mergedEntryType m of
           Tomb -> 0
           Dir  -> 1
           File -> 2

      for_ (L.sortOn filesLast merged) $ \w -> do
        case w of
          N (p,TombEntry e) -> do
            notice $ green "removed" <+> pretty p

          D (p,e) _ -> do

            tombs <- getTombs

            n <- Compact.getValEither @Integer tombs p
                   <&> fromRight (Just 0)

            notice $ "deleted locally" <+> pretty n <+> pretty p

            when (n < Just 1) do
              notice $ "post tomb tx" <+> pretty n <+> pretty p
              now  <- liftIO $ getPOSIXTime <&> round <&> LBS.take 6 . serialise
              postEntryTx (Just now) (HM.lookup p hasGK0) refchan path e
              Compact.putVal tombs p (maybe 1 succ n)

          N (p,_) -> do
            notice $ "?" <+> pretty p

          M (f,t,e) -> do
            notice $ green "move" <+> pretty f <+> pretty t
            mv (path </> f) (path </> t)
            notice $ green "post renamed entry tx" <+> pretty f
            postEntryTx Nothing (HM.lookup f hasGK0) refchan path e

          E (p,UpdatedFileEntry _ e) -> do
            let fullPath = path </> p
            here <- liftIO $ doesFileExist fullPath
            writeEntry path e
            notice $ red "updated" <+> pretty here <+> pretty p
            postEntryTx Nothing (HM.lookup p hasGK0) refchan path e

          E (p,e@(FileEntry _)) -> do
            let fullPath = path </> p
            here <- liftIO $ doesFileExist fullPath
            d    <- liftIO $ doesDirectoryExist fullPath

            older <- if here then do
                       s <- getFileTimestamp fullPath
                       pure $ s < getEntryTimestamp e
                     else
                       pure False

            when (not here || older) do
              writeEntry path e

            void $ runMaybeT do
              gk0 <- HM.lookup p hasGK0 & toMPlus
              let rcpt = recipients gk0 & HM.keys
              let members = view refChanHeadReaders rch & HS.toList
              when (rcpt /= members) do
                notice $ red "update group key" <+> pretty p
                lift $ postEntryTx Nothing (Just gk0) refchan path e

          E (p,TombEntry e) -> do
            let fullPath = path </> p
            here <- liftIO $ doesFileExist fullPath
            when here do

              tombs <- getTombs

              n <- Compact.getValEither @Integer tombs p
                    <&> fromRight (Just 0)


              when (n < Just 1) do
                notice $ "post tomb tx" <+> pretty n <+> pretty p
                postEntryTx Nothing (HM.lookup p hasGK0) refchan path e

              Compact.putVal tombs p (maybe 0 succ n)

              b <- backupMode

              unless b do
                notice $ red "deleted" <+> pretty p
                rm fullPath

          E (p,_) -> do
            notice $ "skip entry" <+> pretty p


