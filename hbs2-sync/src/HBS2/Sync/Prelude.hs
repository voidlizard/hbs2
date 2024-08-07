{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}
{-# Language MultiWayIf #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module HBS2.Sync.Prelude
  ( module HBS2.Sync.Prelude
  , module Exported
  ) where


import HBS2.Prelude.Plated as Exported
import HBS2.Clock
import HBS2.Base58
import HBS2.Data.Detect
import HBS2.Merkle
import HBS2.Merkle.MetaData
import HBS2.OrDie as Exported
import HBS2.Data.Types.Refs as Exported
import HBS2.Data.Types.SignedBox
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.GroupKeySymm as Symm
import HBS2.Net.Proto.Service
import HBS2.Storage
import HBS2.Storage.Compact as Compact
import HBS2.Storage.Operations.Class
import HBS2.Storage.Operations.ByteString
import HBS2.Peer.Proto.RefChan
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.Client.RefChan as Client
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.Storage
import HBS2.System.Logger.Simple.ANSI as Exported
import HBS2.System.Dir
import HBS2.Misc.PrettyStuff as Exported

import HBS2.CLI.Run hiding (PeerException(..))
import HBS2.CLI.Run.MetaData
-- import HBS2.CLI.Run.GroupKey

import HBS2.KeyMan.Keys.Direct

import Data.Config.Suckless as Exported
import Data.Config.Suckless.Script as Exported
import Data.Config.Suckless.Script.File

import Codec.Serialise as Exported
import Control.Applicative
import Control.Concurrent.STM (flushTQueue)
import Control.Monad.Reader as Exported
import Control.Monad.Trans.Cont as Exported
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Data.Ord
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce
import Data.Either
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as L
import Data.List (stripPrefix)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as Text
import Data.Set qualified as Set
import Data.Time.Clock.POSIX
import Data.Word
import Lens.Micro.Platform
import Streaming.Prelude qualified as S
import System.Directory (getModificationTime,setModificationTime,doesFileExist,listDirectory)
import System.Directory (XdgDirectory(..),getXdgDirectory)
import System.Exit qualified as Exit

import UnliftIO.IO.File qualified as UIO

{- HLINT ignore "Functor law" -}
{- HLINT ignore "Eta reduce" -}

type MyRefChan = PubKey 'Sign 'HBS2Basic

data DirSyncEnv =
  DirSyncEnv
  { _dirSyncPath     :: Maybe FilePath
  , _dirSyncRefChan  :: Maybe MyRefChan
  , _dirSyncCreds    :: Maybe (PeerCredentials 'HBS2Basic)
  , _dirSyncInclude  :: [FilePattern]
  , _dirSyncExclude  :: [FilePattern]
  }
  deriving stock (Generic)

makeLenses 'DirSyncEnv

instance Monoid DirSyncEnv where
  mempty = DirSyncEnv Nothing Nothing Nothing mempty ["**/*.hbs2-sync/state"]

instance Semigroup DirSyncEnv where
  (<>) a b = DirSyncEnv ( view dirSyncPath b <|> view dirSyncPath a )
                        ( view dirSyncRefChan b  <|> view dirSyncRefChan a )
                        ( view dirSyncCreds b  <|> view dirSyncCreds a )
                        (L.nub $  view dirSyncInclude a <> view dirSyncInclude b )
                        (L.nub $  view dirSyncExclude a <> view dirSyncExclude b )

instance Pretty DirSyncEnv where
  pretty e = do
    vcat $ catMaybes
         [ pure ("; path" <+> pretty (view dirSyncPath e))
         , view dirSyncRefChan e >>= \x -> pure $ pretty $ mkList  @C [mkSym "refchan", mkSym (show $ pretty (AsBase58 x))]
         , view dirSyncCreds e >>=
                 \x -> pure $ pretty
                            $ mkList @C [mkSym "sign", mkSym (show $ pretty $ AsBase58 $ view peerSignPk  x)]
         , pure $ vcat (fmap (mkPattern "include") (view dirSyncInclude e))
         , pure $ vcat (fmap (mkPattern "exclude") (view dirSyncExclude e))
         ]

    where
      mkPattern name p = pretty $ mkList @C [mkSym name, mkSym p]

data SyncEnv =
  SyncEnv
  { refchanAPI  :: ServiceCaller RefChanAPI UNIX
  , storageAPI  :: ServiceCaller StorageAPI UNIX
  , peerAPI     :: ServiceCaller PeerAPI UNIX
  , dirSyncEnv  :: TVar (Map FilePath DirSyncEnv)
  , dirThis     :: TVar (Maybe FilePath)
  , dirTombs    :: TVar (Map FilePath (CompactStorage HbSync))
  }

newtype SyncApp m a =
  SyncApp { fromSyncApp :: ReaderT (Maybe SyncEnv) m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadUnliftIO
                   , MonadIO
                   , MonadReader (Maybe SyncEnv))


type SyncAppPerks m = MonadUnliftIO m

class Monad m => HasTombs m where
  getTombs :: m (CompactStorage HbSync)
  closeTombs :: m ()

instance MonadUnliftIO m => HasTombs (SyncApp m) where
  getTombs = do
    SyncEnv{..} <- ask >>= orThrow PeerNotConnectedException
    path <- getRunDir

    mbTomb <- dirTombs & readTVarIO
               <&> Map.lookup path

    case mbTomb of
      Just tomb -> pure tomb
      Nothing -> do
        -- FIXME: path-hardcode
        let tombsPath = path </> ".hbs2-sync" </> "state" </> "tombs"
        mkdir (dropFileName tombsPath)
        stoTombs <- compactStorageOpen mempty tombsPath
        atomically (modifyTVar dirTombs (Map.insert path stoTombs))
        pure stoTombs

  closeTombs = do
    path <- getRunDir

    void $ runMaybeT do

      SyncEnv{..} <- lift ask >>= toMPlus

      tombs <- dirTombs & readTVarIO
                   <&> Map.lookup path
                   >>= toMPlus

      compactStorageClose tombs

instance MonadIO m => HasClientAPI StorageAPI UNIX (SyncApp m) where
  getClientAPI = ask >>= orThrow PeerNotConnectedException
                     <&> storageAPI

instance MonadIO m => HasClientAPI RefChanAPI UNIX (SyncApp m) where
  getClientAPI = ask >>= orThrow PeerNotConnectedException
                     <&> refchanAPI

instance MonadIO m => HasClientAPI PeerAPI UNIX (SyncApp m) where
  getClientAPI = ask >>= orThrow PeerNotConnectedException
                     <&> peerAPI

instance MonadIO m => HasStorage (SyncApp m) where
  getStorage = do
    api <- getClientAPI @StorageAPI @UNIX
    pure $ AnyStorage (StorageClient api)

withSyncApp :: SyncAppPerks m => Maybe SyncEnv -> SyncApp m a -> m a
withSyncApp env action = runReaderT (fromSyncApp action) env

runSyncApp :: SyncAppPerks m => SyncApp m a -> m a
runSyncApp m = do
  setupLogger
  withSyncApp Nothing m `finally` flushLoggers

recover :: SyncApp IO a -> SyncApp IO a
recover what = do
  catch what $ \case
    PeerNotConnectedException -> do

      soname <- detectRPC
                  `orDie` "can't locate hbs2-peer rpc"

      flip runContT pure do

        client <- lift $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
                    >>= orThrowUser ("can't connect to" <+> pretty soname)

        void $ ContT $ withAsync $ runMessagingUnix client

        peerAPI    <- makeServiceCaller @PeerAPI (fromString soname)
        refChanAPI <- makeServiceCaller @RefChanAPI (fromString soname)
        storageAPI <- makeServiceCaller @StorageAPI (fromString soname)

        -- let sto = AnyStorage (StorageClient storageAPI)

        let endpoints = [ Endpoint @UNIX  peerAPI
                        , Endpoint @UNIX  refChanAPI
                        , Endpoint @UNIX  storageAPI
                        ]

        void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

        dsync <- newTVarIO mempty
        this  <- newTVarIO Nothing
        tombs <- newTVarIO mempty

        let env = Just (SyncEnv refChanAPI storageAPI peerAPI dsync this tombs)

        liftIO $ withSyncApp env what

data PeerException =
  PeerNotConnectedException
  deriving stock (Show, Typeable)

instance Exception PeerException

data RunDirectoryException =
    RefChanNotSetException
  | RefChanHeadNotFoundException
  | EncryptionKeysNotDefined
  | SignKeyNotSet
  | DirNotSet
  deriving stock (Show,Typeable)

instance Exception RunDirectoryException

removePrefix :: FilePath -> FilePath -> FilePath
removePrefix prefix path =
  let prefixDirs = splitDirectories $ normalise prefix
      pathDirs = splitDirectories $ normalise path
  in joinPath $ fromMaybe pathDirs (stripPrefix prefixDirs pathDirs)

getFileTimestamp :: MonadUnliftIO m => FilePath -> m Word64
getFileTimestamp filePath = do
  t0 <- liftIO $ getModificationTime filePath
  pure (round $ utcTimeToPOSIXSeconds t0)

-- FIXME: move-to-suckless-conf
class IsContext c => ToSexp c a where
  toSexp :: a -> Syntax c


data EntryType = File | Dir | Tomb
                 deriving stock (Eq,Ord,Show,Data,Generic)

data EntryDesc =
  EntryDesc
  { entryType        :: EntryType
  , entryTimestamp   :: Word64
  , entryRemoteHash  :: Maybe HashRef
  }
  deriving stock (Eq,Ord,Show,Data,Generic)

newtype AsSexp c a = AsSexp a

pattern TombLikeOpt :: forall {c} . Syntax c
pattern TombLikeOpt <- ListVal [StringLike "tomb:", tombLikeValue -> True]

tombLikeValue :: Syntax c -> Bool
tombLikeValue = \case
  StringLike "#t" -> True
  StringLike "true" -> True
  StringLike "yes" -> True
  StringLike "tomb" -> True
  LitBoolVal True -> True
  _ -> False

pattern TombEntry :: Entry -> Entry
pattern TombEntry e <- e@(DirEntry (EntryDesc { entryType = Tomb }) _)

pattern FileEntry :: Entry -> Entry
pattern FileEntry e <- e@(DirEntry (EntryDesc { entryType = File }) _)

pattern UpdatedFileEntry :: Word64 -> Entry -> Entry
pattern UpdatedFileEntry t e <- e@(DirEntry (EntryDesc { entryType = File
                                                       , entryRemoteHash = Nothing
                                                       , entryTimestamp = t }) _)

instance (IsContext c, ToSexp c w) => Pretty (AsSexp c w) where
  pretty (AsSexp s) = pretty (toSexp @c s)

data Entry =
  DirEntry EntryDesc FilePath
  deriving stock (Eq,Ord,Show,Data,Generic)

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

runDirectory :: ( IsContext c
                , SyncAppPerks m
                , HasClientAPI RefChanAPI UNIX m
                , HasClientAPI StorageAPI UNIX m
                , HasStorage m
                , HasRunDir m
                , HasTombs m
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

  where


    writeEntry path e = do

      let p = entryPath e
      let filePath = path </> p

      sto <- getStorage

      tombs <- getTombs

      void $ runMaybeT do
        h <- getEntryHash e & toMPlus

        notice $ green "write  entry" <+> pretty h <+> pretty (path </> p)

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

      path <- getRunDir

      env <- getRunDirEnv path >>= orThrow DirNotSet

      refchan <- view dirSyncRefChan env & orThrow RefChanNotSetException

      fetchRefChan @UNIX refchan

      -- FIXME: multiple-directory-scans

      local  <- getStateFromDir0 True

      deleted <- findDeleted

      merged <- mergeState deleted local

      let filesLast m = case mergedEntryType m of
           Tomb -> 0
           Dir  -> 1
           File -> 2

      for_ (L.sortOn filesLast merged) $ \w -> do
        case w of
          N (p,TombEntry e) -> do
            notice $ green "removed entry" <+> pretty p

          D (p,e) _ -> do
            notice $ "locally deleted file" <+> pretty p

            tombs <- getTombs

            n <- Compact.getValEither @Integer tombs p
                   <&> fromRight (Just 0)

            when (n < Just 2) do
              postEntryTx refchan path e
              Compact.putVal tombs p (maybe 0 succ n)

          N (_,_) -> none

          M (f,t,e) -> do
            notice $ green "move entry" <+> pretty f <+> pretty t
            mv (path </> f) (path </> t)
            notice $ green "post renamed entry tx" <+> pretty f
            postEntryTx refchan path e

          E (p,UpdatedFileEntry _ e) -> do
            let fullPath = path </> p
            here <- liftIO $ doesFileExist fullPath
            writeEntry path e
            notice $ red "updated file entry" <+> pretty here <+> pretty p
            postEntryTx refchan path e

          E (p,e@(FileEntry _)) -> do
            let fullPath = path </> p
            here <- liftIO $ doesFileExist fullPath
            d    <- liftIO $ doesDirectoryExist fullPath

            -- getRef tombs (SomeRef (g

            older <- if here then do
                       s <- getFileTimestamp fullPath
                       pure $ s < getEntryTimestamp e
                     else
                       pure False

            when (not here || older) do
              writeEntry path e

          E (p,TombEntry e) -> do
            let fullPath = path </> p
            here <- liftIO $ doesFileExist fullPath
            when here do

              tombs <- getTombs
              postEntryTx refchan path e

              n <- Compact.getValEither @Integer tombs p
                    <&> fromRight (Just 0)

              Compact.putVal tombs p (maybe 0 succ n)

              notice $ red "tomb entry" <+> pretty (path </> p)
              rm fullPath

          E (p,_) -> do
            notice $ "skip entry" <+> pretty (path </> p)


findDeleted :: (MonadIO m, HasRunDir m, HasTombs m) => m [Merged]
findDeleted = do

  dir <- getRunDir

  now <- liftIO $ getPOSIXTime <&> round

  tombs <- getTombs
  -- TODO: check-if-non-latin-filenames-work
  --   resolved: ok
  seen <- Compact.keys tombs
            <&> fmap BS8.unpack

  S.toList_ do
    for_ seen $ \f0 -> do

      let path = dir </> f0

      here <- liftIO $ doesFileExist path

      n <- Compact.getValEither @Integer tombs f0
            <&> fromRight (Just 0)

      when (not here && isJust n) do
        S.yield (D (f0, makeTomb now f0 Nothing) n)
        debug $ "found deleted" <+> pretty n <+> pretty f0


postEntryTx :: ( MonadUnliftIO m
               , HasStorage m
               , HasRunDir m
               , HasClientAPI StorageAPI UNIX m
               , HasClientAPI RefChanAPI UNIX m
               )
            => MyRefChan
            -> FilePath
            -> Entry
            -> m ()
postEntryTx refchan path entry = do

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

    gk <- Symm.generateGroupKey @'HBS2Basic Nothing members

    -- FIXME: survive-this-error?
    href <- lift $ createTreeWithMetadata sto (Just gk) meta lbs
               >>= orThrowPassIO

    let tx = AnnotatedHashRef Nothing href
    let spk = view peerSignPk creds
    let ssk = view peerSignSk creds

    let box = makeSignedBox @HBS2Basic spk ssk (LBS.toStrict $ serialise tx)

    notice $ red "POST NEW REMOTE ENTRY" <+> pretty p <+> pretty href

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

getStateFromDir0 :: ( MonadIO m
                   , HasClientAPI RefChanAPI UNIX m
                   , HasClientAPI StorageAPI UNIX m
                   , HasStorage m
                   , HasRunDir m
                   )
                => Bool
                -> m [(FilePath, Entry)]
getStateFromDir0 seed = do

  dir <- getRunDir

  env <- getRunDirEnv dir >>= orThrow DirNotSet

  let excl = view dirSyncExclude env
  let incl = view dirSyncInclude env

  getStateFromDir seed dir incl excl

getStateFromDir :: ( MonadIO m
                   , HasClientAPI RefChanAPI UNIX m
                   , HasClientAPI StorageAPI UNIX m
                   , HasStorage m
                   , HasRunDir m
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
           d <- liftIO $ doesDirectoryExist (path </> p)
           when d do
            ts <- liftIO $ getFileTimestamp  (path </> p)
            S.yield (p, DirEntry (EntryDesc Dir ts mzero) p)

           S.yield (p,e)


getStateFromRefChan :: forall m . ( MonadIO m
                                  , HasClientAPI RefChanAPI UNIX m
                                  , HasClientAPI StorageAPI UNIX m
                                  , HasStorage m
                                  )
                    => MyRefChan
                    -> m [(FilePath, Entry)]
getStateFromRefChan rchan = do

  debug $ red "getStateFromRefChan" <+> pretty (AsBase58 rchan)

  sto <- getStorage

  outq <- newTQueueIO
  tss <- newTVarIO mempty

  walkRefChanTx @UNIX rchan $ \case
    A (AcceptTran ts _ what) -> do
      -- debug $ red "ACCEPT" <+> pretty ts <+> pretty what
      for_ ts $ \w -> do
        atomically $ modifyTVar tss (HM.insertWith max what (coerce @_ @Word64 w))

    P orig (ProposeTran _ box) -> void $ runMaybeT do
      (_, bs) <- unboxSignedBox0 box & toMPlus
      AnnotatedHashRef w href <- deserialiseOrFail @AnnotatedHashRef (LBS.fromStrict bs)
                                  & toMPlus . either (const Nothing) Just

      let findKey gk = liftIO (runKeymanClient (extractGroupKeySecret gk))

      runExceptT (extractMetaData @'HBS2Basic findKey sto href)
       >>= either (const none) ( \meta -> atomically $ writeTQueue outq (orig, (href, meta)) )

  trees <- atomically (flushTQueue outq)

  tsmap <- readTVarIO tss

  ess0 <- S.toList_ do
    for_ trees $ \(txh, (tree, meta)) -> do
      let what = parseTop meta & fromRight mempty
      let loc  = headDef "" [ l | ListVal [StringLike "location:", StringLike l] <- what ]

      void $ runMaybeT do
        fn   <- toMPlus $ headMay [ l | ListVal [StringLike "file-name:", StringLike l] <- what ]
        ts   <- toMPlus $ HM.lookup txh tsmap
        let tomb = or [ True | TombLikeOpt <- what  ]
        let fullPath = loc </> fn

        debug $ red "META" <+> pretty what

        if tomb then do
          lift $ S.yield $
            Map.singleton fullPath (makeTomb ts fullPath (Just tree))
        else do
          let r = entriesFromFile (Just tree) ts fullPath
          lift $ S.yield r

  pure $ Map.toList $ Map.unionsWith merge ess0


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

      rcpts  <- Symm.loadGroupKeyMaybe sto (HashRef gkh)
                  >>= orThrowError (GroupKeyNotFound 11)
                  <&> HM.keys . Symm.recipients

      kre <- runKeymanClient do
                loadKeyRingEntries rcpts <&> fmap snd

      readFromMerkle sto (ToDecryptBS kre (coerce href))

    _ -> throwError UnsupportedFormat

class MonadIO m => HasRunDir m where
  getRunDir       :: m FilePath
  getRunDirEnv    :: FilePath -> m (Maybe DirSyncEnv)
  alterRunDirEnv  :: FilePath -> ( Maybe DirSyncEnv -> Maybe DirSyncEnv ) -> m ()

instance (MonadUnliftIO m) => HasRunDir (SyncApp m) where
  getRunDir = ask >>= orThrow PeerNotConnectedException
                  >>= readTVarIO . dirThis
                  >>= orThrow DirNotSet

  getRunDirEnv dir =  do
    env <- ask >>= orThrow PeerNotConnectedException
               >>= readTVarIO . dirSyncEnv
    pure $ Map.lookup dir env

  alterRunDirEnv dir action = do
    tenv <- ask >>= orThrow PeerNotConnectedException
            <&> dirSyncEnv
    atomically $ modifyTVar tenv (Map.alter action dir)

instance HasRunDir m => HasRunDir (RunM c m) where
  getRunDir = lift getRunDir
  getRunDirEnv d = lift (getRunDirEnv d)
  alterRunDirEnv d a = lift (alterRunDirEnv d a)

instance HasRunDir m => HasRunDir (MaybeT m) where
  getRunDir = lift getRunDir
  getRunDirEnv d = lift (getRunDirEnv d)
  alterRunDirEnv d a = lift (alterRunDirEnv d a)

instance HasRunDir m => HasRunDir (ContT r m) where
  getRunDir = lift getRunDir
  getRunDirEnv d = lift (getRunDirEnv d)
  alterRunDirEnv d a = lift (alterRunDirEnv d a)


instance HasTombs m => HasTombs (ContT r m) where
  getTombs = lift getTombs
  closeTombs = lift closeTombs

instance HasTombs m => HasTombs (MaybeT m) where
  getTombs = lift getTombs
  closeTombs = lift closeTombs

instance (Monad m, HasTombs m) => HasTombs (RunM c m) where
  getTombs = lift getTombs
  closeTombs = lift closeTombs

syncEntries :: forall c m . ( MonadUnliftIO m
                            , IsContext c
                            , Exception (BadFormException c)
                            , HasClientAPI RefChanAPI UNIX m
                            , HasClientAPI StorageAPI UNIX m
                            , HasStorage m
                            , HasRunDir m
                            , HasTombs m
                            , MonadReader (Maybe SyncEnv) m
                            )
            => MakeDictM c m ()
syncEntries = do

  entry $ bindMatch "--debug" $ nil_ $ \case
    [SymbolVal "off"] -> do
      setLoggingOff @DEBUG

    _ -> do
      setLogging @DEBUG  debugPrefix

  entry $ bindMatch "init"  $ nil_ $ const do
    pure ()

  entry $ bindMatch "sync" $ nil_ $ \case
    [StringLike d] -> do

      void $ evalTop [ mkList [mkSym "dir", mkStr d]
                     , mkList [mkSym "run"]
                     ]

    [] -> do

      void $ evalTop [ mkList [mkSym "dir", mkStr "."]
                     , mkList [mkSym "run"]
                     ]

    _ -> pure ()

  brief "sets current directory"
   $ args [ arg "string"  "dir" ]
   $ desc "useful for debugging"
   $ entry $ bindMatch "dir"  $ nil_ $ \case
       [StringLike d] -> do
         debug $ "set current directory" <+> pretty d
         t <- lift ask >>= orThrow PeerNotConnectedException
         atomically $ writeTVar (dirThis t) (Just d)

         alterRunDirEnv d $ \case
           Nothing -> Just (mempty & set dirSyncPath (Just d))
           Just x  -> Just (x & set dirSyncPath (Just d))

         ins <- try @_ @IOError (liftIO $ readFile (d </> ".hbs2-sync/config"))
                 <&> fromRight mempty
                 <&> parseTop
                 <&> either mempty (fmap fixContext)

         void $ evalTop ins

       _ -> do
        err "current dir not set"

  entry $ bindMatch "refchan" $ nil_ $ \case
    [SignPubKeyLike puk] -> do
      dir <- getRunDir
      debug $ red "refchan" <+> pretty dir <+> pretty (AsBase58 puk)
      alterRunDirEnv dir $ \case
        Nothing -> Just (mempty & set dirSyncRefChan (Just puk))
        Just x  -> Just (x & set dirSyncRefChan (Just puk))

    x -> err $ "invalid refchan" <+> pretty (mkList x)

  entry $ bindMatch "exclude" $ nil_ $ \case
        [StringLike excl] -> do
          dir <- getRunDir
          debug $ red "exclude" <+> pretty dir <+> pretty excl
          alterRunDirEnv dir $ \case
            Nothing -> Just (mempty & set dirSyncExclude [excl])
            Just x  -> Just (x & over dirSyncExclude (mappend [excl]))

        _ -> pure ()

  entry $ bindMatch "include" $ nil_ $ \case
        [StringLike pat] -> do
          dir <- getRunDir
          debug $ red "include" <+> pretty dir <+> pretty pat
          alterRunDirEnv dir $ \case
            Nothing -> Just (mempty & set dirSyncInclude [pat])
            Just x  -> Just (x & over dirSyncInclude (mappend [pat]))

        _ -> pure ()

  entry $ bindMatch "sign" $ nil_ $ \case
        [SignPubKeyLike s] -> do
          dir <- getRunDir
          debug $ red "sign" <+> pretty (AsBase58 s)
          creds <- liftIO (runKeymanClient $ loadCredentials s)
          alterRunDirEnv dir $ \case
            Nothing -> Just (mempty & set dirSyncCreds creds)
            Just x  -> Just (x & set dirSyncCreds creds)

        w -> err $ "invalid sign key" <+> pretty (mkList w)


  entry $ bindMatch "dir:state:merged:show" $ nil_ $ \_ -> do
    state <- getStateFromDir0 True

    deleted <- findDeleted
    merged <- mergeState deleted state

    liftIO $ print $ vcat (fmap (pretty . AsSexp @C) merged)


  entry $ bindMatch "ls" $ nil_ $ \case
    (StringLikeList _) -> do
      state <- getStateFromDir0 False <&> Map.fromList

      for_ (Map.toList state) $ \(f,e)  -> do
        when (isFile e || isDir e ) do
          liftIO $ putStrLn f

    _ -> pure ()

  entry $ bindMatch "dir:state:local:show" $ nil_ $ \sy -> do

    let f = case sy of
              [StringLike "F"] -> isFile
              [StringLike "D"] -> isDir
              _                -> const True

    state <- getStateFromDir0 True

    liftIO $ print $ vcat (fmap (pretty . AsSexp @C . snd) (filter (f . snd)  state))

  entry $ bindMatch "dir:state:remote:show" $ nil_ $ \syn ->  do

    let f = case syn of
              [StringLike "F"] -> isFile
              [StringLike "D"] -> isDir
              _                -> const True

    dir <- getRunDir


    env <- getRunDirEnv dir >>= orThrow DirNotSet

    runMaybeT do

      rchan <- view dirSyncRefChan env
                & toMPlus

      state <- lift $ getStateFromRefChan rchan

      liftIO $ print $ vcat (fmap (pretty . AsSexp @C . snd) (filter (f.snd) state))


  entry $ bindMatch "dir:config:show" $ nil_ $ const do
    dir <- getRunDir

    void $ runMaybeT do
      env <- getRunDirEnv dir >>= toMPlus
      liftIO $ print $ pretty env

  entry $ bindMatch "run" $ nil_  \case
    _ -> runDirectory


  entry $ bindMatch "prune" $ nil_ \case
    [] -> do


      path <- getRunDir

      env <- getRunDirEnv path >>= orThrow DirNotSet

      let excl = view dirSyncExclude env

      let skip p = or [ i ?== p | i <- excl ]

      dirs <- S.toList_ do
                flip fix [path] $ \next -> \case
                  (d:ds) -> do
                    dirs <- liftIO (listDirectory d)
                    let es =  [ path </> d </> x | x <- dirs, not (skip x) ]
                    dd <- liftIO $ filterM doesDirectoryExist es
                    S.each dd
                    next (ds <> dd)

                  [] -> pure ()

      for_ (L.sortBy (comparing Down) dirs) $ \d -> do
        pu <- liftIO (listDirectory d) <&> L.null
        when pu do
          notice $ red "prune" <+> pretty d
          rm d

    _ -> pure ()


  brief "posts tomb transaction for the current dir"
    $ args [arg "string" "entry-path"]
    $ desc (    "working dir must be set first" <> line
             <> "see: dir, sync"
           )
    $ entry $ bindMatch "tomb" $ nil_ \case
      [StringLike p] -> do

        path <- getRunDir
        env <- getRunDirEnv path >>= orThrow DirNotSet

        void $ runMaybeT do

          let fullPath = path </> p

          rchan <- view dirSyncRefChan env
                     & toMPlus


          here <- liftIO (doesFileExist fullPath)
          guard here

          now <- liftIO getPOSIXTime <&> round

          notice $ red "ABOUT TO POST TOMB TX" <+> pretty p
          lift $ postEntryTx rchan path (makeTomb now p mzero)

      _ -> pure ()

  entry $ bindMatch "run-config" $ nil_ $ const do
    cpath <- liftIO $ getXdgDirectory XdgConfig "hbs2-sync" <&> (</> "config")
    debug $ "run-config" <+> pretty cpath
    try @_ @IOError (liftIO $ readFile cpath)
      <&> fromRight mempty
      <&> parseTop
      <&> either mempty (fmap fixContext)
      >>= evalTop

  entry $ bindMatch "timestamp" $ nil_ $ \case
    [StringLike fn] -> do
      liftIO (getFileTimestamp fn >>=  print)
    _ -> do
      liftIO $ getPOSIXTime <&> round >>= print

-- debugPrefix :: LoggerEntry -> LoggerEntry
debugPrefix = toStderr . logPrefix "[debug] "

setupLogger :: MonadIO m => m ()
setupLogger = do
  -- setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStdout . logPrefix ""
  pure ()

flushLoggers :: MonadIO m => m ()
flushLoggers = do
  silence

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE


quit :: forall m . MonadUnliftIO m => m ()
quit = liftIO Exit.exitSuccess

die :: forall a m . (MonadUnliftIO m, Pretty a) => a -> m ()
die what = liftIO do
  hPutDoc stderr (pretty what)
  Exit.exitFailure


