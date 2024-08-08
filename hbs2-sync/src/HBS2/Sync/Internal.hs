module HBS2.Sync.Internal
  ( syncEntries
  ) where

import HBS2.Sync.Prelude
import HBS2.Sync.State

import HBS2.Merkle.MetaData
import HBS2.System.Dir
import HBS2.Net.Auth.GroupKeySymm
import HBS2.Storage.Operations.Class
import HBS2.Storage.Compact as Compact
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.RefChan as Client

import HBS2.CLI.Run.Internal hiding (PeerNotConnectedException)

import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Time.Clock.POSIX


import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as L
import Data.Map qualified as Map
import Lens.Micro.Platform
import System.Directory (setModificationTime,listDirectory)
import System.Directory (XdgDirectory(..),getXdgDirectory)
import Control.Monad.Except
import Data.Ord

import Streaming.Prelude qualified as S
import UnliftIO.IO.File qualified as UIO


syncEntries :: forall c m . ( MonadUnliftIO m
                            , IsContext c
                            , Exception (BadFormException c)
                            , HasClientAPI RefChanAPI UNIX m
                            , HasClientAPI StorageAPI UNIX m
                            , HasStorage m
                            , HasRunDir m
                            , HasTombs m
                            , HasCache m
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

  entry $ bindMatch "backup-mode" $ nil_ $ \case
        [] -> do
          dir <- getRunDir
          debug $ red "backup-mode" <+> pretty dir
          alterRunDirEnv dir $ \case
            Nothing -> Just (mempty & set dirSyncBackup True)
            Just x  -> Just (x & set dirSyncBackup True)

        _ -> pure ()

  entry $ bindMatch "follow-symlinks" $ nil_ $ \case
        [] -> do
          dir <- getRunDir
          debug $ red "follow-symlinks" <+> pretty dir
          alterRunDirEnv dir $ \case
            Nothing -> Just (mempty & set dirSyncFollowSymlinks True)
            Just x  -> Just (x & set dirSyncFollowSymlinks True)

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


  brief "output file from remote state"
    $ args [arg "string" "refchan", arg "string" "file"]
    $ entry $ bindMatch "cat" $ nil_ $ \case
      [SignPubKeyLike rchan, StringLike fn] -> do
        sto <- getStorage
        void $ runMaybeT do
          h <- lift (getStateFromRefChan rchan)
                 <&> Map.fromList
                 <&> Map.lookup fn
                 >>= toMPlus
                 <&> getEntryHash
                 >>= toMPlus

          lbs <- lift $ runExceptT (getTreeContents sto h)
                   >>= orThrowPassIO

          liftIO $ LBS.putStr lbs

      _ -> none

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
          lift $ postEntryTx Nothing Nothing rchan path (makeTomb now p mzero)

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

