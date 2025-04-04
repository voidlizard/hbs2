module HBS2.Sync.Mount
  ( mountPath
  ) where

import HBS2.Sync.Prelude hiding (SyncEnv(..))
import HBS2.Sync.State

import HBS2.Actors.Peer (makeResponse, runProto)
import HBS2.CLI.Run.MetaData (getTreeContents)
import HBS2.KeyMan.Keys.Direct qualified as KE
import HBS2.Net.Messaging.Unix as Unix
import HBS2.Net.Proto.Service qualified as HBS2
import HBS2.Net.Proto.Notify (runNotifySink, makeNotifyClient, newNotifySink, runNotifyWorkerClient)
import HBS2.Peer.CLI.Detect (detectRPC)
import HBS2.Peer.Notify (NotifyData(RefChanUpdated), NotifyKey(..), RefChanEvents)
import HBS2.Peer.RPC.API.Peer qualified as Peer
import HBS2.Peer.RPC.API.RefChan qualified as RefChan
import HBS2.Peer.RPC.API.Storage qualified as Storage
import HBS2.Peer.RPC.Client qualified as Client
import HBS2.Peer.RPC.Client.StorageClient qualified as Client
import HBS2.Peer.RPC.Client.Unix (runServiceClientMulti, Endpoint(Endpoint))

import Control.Monad.Except (runExceptT)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Map qualified as Map
import System.Fuse (FuseOperations(..))
import System.Fuse qualified as Fuse
import System.Posix.Files qualified as Posix
import System.Posix.Types qualified as Posix
import Data.Int (Int64)

type FuseOp a = IO (Either Fuse.Errno a)

type Tree = Map.Map FilePath Entry

data MountEnv =
  MountEnv
    { peerAPI :: HBS2.ServiceCaller Peer.PeerAPI UNIX
    , refChanAPI :: HBS2.ServiceCaller RefChan.RefChanAPI UNIX
    , storageAPI :: HBS2.ServiceCaller Storage.StorageAPI UNIX
    , keymanClientEnv :: KeyManClientEnv
    }

newtype MountApp m a =
  MountApp { fromMountApp :: ReaderT MountEnv m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadUnliftIO
                   , MonadIO
                   , MonadReader MountEnv)

data State =
  State
    { refChan :: MyRefChan
    , tree :: Tree
    }

instance MonadUnliftIO m => HasKeyManClient (MountApp m) where
  getKeyManClientEnv = ask <&> keymanClientEnv

instance MonadIO m => Client.HasClientAPI Storage.StorageAPI UNIX (MountApp m) where
  getClientAPI = ask <&> storageAPI

instance MonadIO m => Client.HasClientAPI RefChan.RefChanAPI UNIX (MountApp m) where
  getClientAPI = ask <&> refChanAPI

instance MonadIO m => Client.HasClientAPI Peer.PeerAPI UNIX (MountApp m) where
  getClientAPI = ask <&> peerAPI

instance MonadIO m => HasStorage (MountApp m) where
  getStorage = do
    api <- Client.getClientAPI @Storage.StorageAPI @UNIX
    pure $ AnyStorage (Client.StorageClient api)

withEnv :: MonadUnliftIO m => MountApp IO a -> m a
withEnv action = do
  soname <- detectRPC >>= orThrowUser "could not detect RPC"
  flip runContT pure do
    client <- newMessagingUnix False 1.0 soname
    void $ ContT $ withAsync $ runMessagingUnix client

    peerAPI <- HBS2.makeServiceCaller @Peer.PeerAPI (fromString soname)
    refChanAPI <- HBS2.makeServiceCaller @RefChan.RefChanAPI (fromString soname)
    storageAPI <- HBS2.makeServiceCaller @Storage.StorageAPI (fromString soname)
    let endpoints = [ Endpoint @UNIX peerAPI
                    , Endpoint @UNIX refChanAPI
                    , Endpoint @UNIX storageAPI
                    ]
    void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

    keymanClientEnv <- liftIO $ KE.newKeymanClientEnv

    let env = MountEnv{..}
    liftIO $ runReaderT (fromMountApp action) env

rootPath :: FilePath
rootPath = "/"

buildTree :: [Entry] -> Map.Map FilePath Entry
buildTree entries =
  let
    addDirs entry =
      if isFile entry then
        entriesFromFile (getEntryHash entry) (getEntryTimestamp entry) (entryPath entry)
      else
        Map.empty

    prependSlash (DirEntry desc path) acc =
      let
        newPath = "/" <> path
      in
      Map.insert newPath (DirEntry desc newPath) acc
  in
  entries
    & List.sortOn getEntryTimestamp
    & foldl (\acc entry -> Map.insert (entryPath entry) entry acc) Map.empty
    & foldr (\entry acc -> Map.unionWith merge (addDirs entry) acc) Map.empty
    & Map.foldr prependSlash Map.empty

dirStat :: Fuse.FuseContext -> Fuse.FileStat
dirStat ctx =
  let
    statEntryType = Fuse.Directory
    statFileMode =
      foldr1 Posix.unionFileModes
        [ Posix.ownerReadMode
        , Posix.ownerExecuteMode
        , Posix.groupReadMode
        , Posix.groupExecuteMode
        , Posix.otherReadMode
        , Posix.otherExecuteMode
        ]
    statLinkCount = 2
    statFileOwner = Fuse.fuseCtxUserID ctx
    statFileGroup = Fuse.fuseCtxGroupID ctx
    statSpecialDeviceID = 0
    statFileSize = 4096
    statBlocks = 1
    statAccessTime = 0
    statModificationTime = 0
    statStatusChangeTime = 0
  in
  Fuse.FileStat { .. }

fileStat :: Int64 -> Fuse.FuseContext -> Fuse.FileStat
fileStat size ctx =
  let
    statEntryType = Fuse.RegularFile
    statFileMode =
      foldr1 Posix.unionFileModes
        [ Posix.ownerReadMode
        , Posix.groupReadMode
        , Posix.otherReadMode
        ]
    statLinkCount = 1
    statFileOwner = Fuse.fuseCtxUserID ctx
    statFileGroup = Fuse.fuseCtxGroupID ctx
    statSpecialDeviceID = 0
    statFileSize = fromIntegral size
    statBlocks = 1
    statAccessTime = 0
    statModificationTime = 0
    statStatusChangeTime = 0
  in
  Fuse.FileStat { .. }

onInit :: IORef (Maybe State) -> MyRefChan -> IO ()
onInit ref refChan = do
  rpcSockPath' <- detectRPC >>= orThrowUser "could not detect RPC"
  refChanNotifyClient <- newMessagingUnix False 1.0 rpcSockPath'
  sink <- newNotifySink

  async1 <- async $ runMessagingUnix refChanNotifyClient

  async2 <- async $ flip runReaderT refChanNotifyClient $ do
    runProto @UNIX
      [ makeResponse (makeNotifyClient @(RefChanEvents L4Proto) sink)
      ]

  async3 <- async $ runNotifySink sink (RefChanNotifyKey refChan) $ \case
    RefChanUpdated _ _ -> do
      accepted <- withEnv $ getAccepted refChan
      let tree = buildTree accepted
      writeIORef ref $ Just State{..}

    _ -> do
      return ()

  async4 <- async $ flip runReaderT refChanNotifyClient $ do
    runNotifyWorkerClient sink

  accepted <- withEnv $ getAccepted refChan
  let tree = buildTree accepted
  writeIORef ref $ Just State{..}

  return ()

onGetFileStat :: IORef (Maybe State) -> FilePath -> FuseOp Fuse.FileStat
onGetFileStat ref path
  | path == rootPath =
    Right . dirStat <$> Fuse.getFuseContext

  | otherwise = do
    Just State{..} <- readIORef ref
    case Map.lookup path tree of
      Just (DirEntry (EntryDesc { entryType = Dir }) _) ->
        Right . dirStat <$> Fuse.getFuseContext

      Just entry@(DirEntry (EntryDesc { entryType = File }) _) ->
        case getEntryHash entry of
          Just hash -> do
            size <- withEnv do
              storage <- getStorage
              eitherContent <- runExceptT (getTreeContents storage hash)
              case eitherContent of
                Right content ->
                  return $ (LBS.length content)

            Right . fileStat size <$> Fuse.getFuseContext

          _ ->
            return $ Left Fuse.eNOENT

      _ ->
        return $ Left Fuse.eNOENT

onOpen :: IORef (Maybe State) -> FilePath -> Fuse.OpenMode -> Fuse.OpenFileFlags -> FuseOp ()
onOpen ref path mode _flags = do
  Just State{..} <- readIORef ref
  case Map.lookup path tree of
    Just (DirEntry (EntryDesc { entryType = File }) _) ->
      case mode of
        Fuse.ReadOnly ->
          return $ Right ()

        _ ->
          return $ Left Fuse.eACCES

    _ ->
      return $ Left Fuse.eNOENT


onRead :: IORef (Maybe State) -> FilePath -> () -> Posix.ByteCount -> Posix.FileOffset -> FuseOp BS.ByteString
onRead ref path _ byteCount offset = do
  Just State{..} <- readIORef ref
  withEnv do
    case Map.lookup path tree of
      Just entry@(DirEntry (EntryDesc { entryType = File }) _) ->
        case getEntryHash entry of
          Just hash -> do
            storage <- getStorage
            eitherContent <- runExceptT (getTreeContents storage hash)
            case eitherContent of
              Right content ->
                content
                  & LBS.drop (fromIntegral offset)
                  & LBS.take (fromIntegral byteCount)
                  & LBS.toStrict
                  & Right
                  & return

              _ ->
                return $ Left Fuse.eNOENT

          _ ->
            return $ Left Fuse.eNOENT

      _ ->
        return $ Left Fuse.eNOENT


onOpenDirectory :: IORef (Maybe State) -> String -> IO Fuse.Errno
onOpenDirectory ref path
  | path == rootPath =
    return Fuse.eOK

  | otherwise = do
    Just State{..} <- readIORef ref
    case Map.lookup path tree of
      Just (DirEntry (EntryDesc { entryType = Dir }) _) ->
        return Fuse.eOK

      _ ->
        return Fuse.eNOENT

stat :: Fuse.FuseContext -> Map.Map FilePath Entry -> FilePath -> FilePath -> [(FilePath, Fuse.FileStat)]
stat context tree prefix path =
  case Map.lookup (prefix <> path) tree of
    Just (DirEntry (EntryDesc { entryType = Dir }) _) ->
      [(path, dirStat context)]

    Just (DirEntry (EntryDesc { entryType = File }) _) ->
      [(path, fileStat 4096 context)]

    _ ->
      []

makeEntries :: Fuse.FuseContext -> Map.Map FilePath Entry -> FilePath -> [(FilePath, Fuse.FileStat)]
makeEntries context tree prefix =
  Map.keys tree
    & filter (List.isPrefixOf prefix)
    & map (\path -> takeWhile (/= '/') $ fromMaybe path $ List.stripPrefix prefix path)
    & List.nub
    & concatMap (stat context tree prefix)

onReadDirectory :: IORef (Maybe State) -> FilePath -> FuseOp [(FilePath, Fuse.FileStat)]
onReadDirectory ref path =
  let
    prefix =
      if path == rootPath then
        rootPath
      else
        path <> "/"
  in do
    Just State{..} <- readIORef ref
    context <- Fuse.getFuseContext
    let entries = makeEntries context tree prefix

    return $ Right $
      [ (".", dirStat context)
      , ("..", dirStat context)
      ] <> entries

onGetFileSystemStats :: String -> FuseOp Fuse.FileSystemStats
onGetFileSystemStats _ =
  return $ Right $ Fuse.FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }

operations :: IORef (Maybe State) -> MyRefChan -> Fuse.FuseOperations ()
operations ref refChan =
  Fuse.defaultFuseOps
    { fuseGetFileStat = onGetFileStat ref
    , fuseGetFileSystemStats = onGetFileSystemStats
    , fuseInit = onInit ref refChan
    , fuseOpen = onOpen ref
    , fuseOpenDirectory = onOpenDirectory ref
    , fuseRead = onRead ref
    , fuseReadDirectory = onReadDirectory ref
    }

mountPath :: MyRefChan -> FilePath -> IO ()
mountPath refChan path = do
  ref <- newIORef Nothing
  Fuse.fuseRun "sync mount" [path] (operations ref refChan) Fuse.defaultExceptionHandler
