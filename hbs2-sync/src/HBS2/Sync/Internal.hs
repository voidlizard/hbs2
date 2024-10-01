module HBS2.Sync.Internal
  ( configPath, syncEntries
  ) where

import HBS2.Sync.Prelude
import HBS2.Sync.State

import HBS2.System.Dir
import HBS2.Storage.Operations.ByteString
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Peer.RPC.Client

import HBS2.CLI.Run.MetaData (getTreeContents)

import HBS2.Data.Types.SignedBox
import HBS2.CLI.Run.Internal.KeyMan

import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as L
import Data.Map qualified as Map
import Lens.Micro.Platform
import System.Directory (XdgDirectory(..),createDirectoryIfMissing,getXdgDirectory,listDirectory)
import Control.Monad.Except
import Data.Ord

import Streaming.Prelude qualified as S

data ConfigException
  = ConfigAlreadyExists String
  deriving (Show)

instance Exception ConfigException

configPath :: FilePath -> FilePath
configPath directory =
  directory </> ".hbs2-sync/config"

findConfig :: (MonadIO m) => m (Maybe FilePath)
findConfig =
  findConfig' =<< pwd
  where
    findConfig' level = do
      let path = configPath level
      exists <- liftIO $ doesFileExist path
      if exists
        then return $ Just path
        else
          let parent = takeDirectory level
           in if parent == level -- we've reached the root directory
                then return Nothing
                else findConfig' parent

syncInit ::
  forall c m.
  ( MonadUnliftIO m,
    IsContext c,
    Exception ConfigException,
    HasClientAPI PeerAPI UNIX m,
    HasClientAPI RefChanAPI UNIX m,
    HasClientAPI StorageAPI UNIX m,
    HasStorage m,
    HasKeyManClient m
  ) =>
  PubKey 'Sign HBS2Basic ->
  PubKey 'Encrypt HBS2Basic ->
  RunM c m ()
syncInit authorKey readerKey = do
  findConfig >>= maybe (pure ()) (throwIO . ConfigAlreadyExists)

  peerApi <- getClientAPI @PeerAPI @UNIX
  rchanApi <- getClientAPI @RefChanAPI @UNIX
  storage <- getStorage

  poked <-
    callService @RpcPoke peerApi ()
      >>= orThrowUser "can't poke hbs2-peer"
      <&> parseTop
      >>= orThrowUser "invalid hbs2-peer attributes"

  peerKey <-
    [ x
    | ListVal [SymbolVal "peer-key:", SignPubKeyLike x] <- poked
    ]
    & headMay
    & orThrowUser "hbs2-peer key not found"

  let chanData =
        refChanHeadDefault @L4Proto
          & set refChanHeadPeers (HM.singleton peerKey 1)
          & set refChanHeadAuthors (HS.singleton authorKey)
          & set refChanHeadReaders (HS.singleton readerKey)

  refchan <- keymanNewCredentials (Just "refchan") 0

  creds <-
    runKeymanClient $
      loadCredentials refchan
        >>= orThrowUser "can't load credentials"

  let box = makeSignedBox @'HBS2Basic (view peerSignPk creds) (view peerSignSk creds) chanData

  href <- writeAsMerkle storage (serialise box)

  callService @RpcPollAdd peerApi (refchan, "refchan", 17)
    >>= orThrowUser "can't subscribe to refchan"

  callService @RpcRefChanHeadPost rchanApi (HashRef href)
    >>= orThrowUser "can't post refchan head"

  let authorString = show $ pretty $ AsBase58 authorKey
  let refchanString = show $ pretty $ AsBase58 refchan
  let configForms :: [Syntax c] =
        [ mkList [mkSym "exclude", mkStr "**/.*"]
        , mkList [mkSym "include", mkStr "**"]
        , mkList [mkSym "sign", mkStr authorString]
        , mkList [mkSym "refchan", mkStr refchanString]
        ]
  let config = unlines $ map (show . pretty) configForms
  display config

  liftIO $ do
    path <- configPath <$> pwd
    createDirectoryIfMissing True $ takeDirectory path
    writeFile path config

  pure ()

syncEntries :: forall c m . ( MonadUnliftIO m
                            , IsContext c
                            , Exception (BadFormException c)
                            , Exception ConfigException
                            , HasClientAPI PeerAPI UNIX m
                            , HasClientAPI RefChanAPI UNIX m
                            , HasClientAPI StorageAPI UNIX m
                            , HasStorage m
                            , HasRunDir m
                            , HasTombs m
                            , HasCache m
                            , HasKeyManClient m
                            , MonadReader (Maybe SyncEnv) m
                            )
            => MakeDictM c m ()
syncEntries = do

  entry $ bindMatch "--debug" $ nil_ $ \case
    [SymbolVal "off"] -> do
      setLoggingOff @DEBUG

    _ -> do
      setLogging @DEBUG  debugPrefix

  entry $ bindMatch "init" $ nil_ $ \case
    [StringLike authorString, StringLike readerString] -> do
      authorKey <- fromStringMay @(PubKey 'Sign HBS2Basic) authorString & orThrowUser "author not found"
      readerKey <- fromStringMay @(PubKey 'Encrypt HBS2Basic) readerString & orThrowUser "reader not found"

      syncInit authorKey readerKey

    [] -> do
      authorKey <- keymanNewCredentials (Just "sync") 1

      creds <-
        runKeymanClient $
          loadCredentials authorKey
            >>= orThrowUser "can't load credentials"

      readerKeyring <-
        view peerKeyring creds
          & headMay
          & orThrowUser "reader key not found"

      let readerKey = view krPk readerKeyring

      syncInit authorKey readerKey

    _ -> do
      err "bad form"

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

         ins <- try @_ @IOError (liftIO $ readFile (configPath d))
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
