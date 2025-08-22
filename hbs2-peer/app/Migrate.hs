module Migrate where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Misc.PrettyStuff
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.OrDie
import HBS2.Defaults
import HBS2.Storage
import HBS2.Storage.NCQ3
import HBS2.Peer.Proto.RefLog
import HBS2.Peer.Proto.RefChan
import HBS2.Peer.Proto.LWWRef

import HBS2.Net.Proto.Types
import Log
import PeerConfig
import Brains

import HBS2.Peer.NCQ3.Migrate.NCQ qualified as N
import HBS2.Peer.NCQ3.Migrate.NCQ (WrapRef(..))

import Data.Config.Suckless.Script hiding (optional)
import Data.Config.Suckless.Script.File (glob)
import Data.Config.Suckless.System as Sy
import Data.Config.Suckless.Almost.RPC

import Data.Char
import Data.Coerce
import Data.Maybe
import System.FilePath
import System.Directory
import System.IO as IO
import System.IO.Temp as Temp
import System.Exit
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Control.Exception
import Control.Monad.Trans.Cont
import Control.Monad.Reader

import UnliftIO
-- import UnliftIO.Temporary

import Streaming.Prelude qualified as S

migrate :: [Syntax C]-> IO ()
migrate syn = flip runContT pure $ callCC \exit -> do

  let (opts, argz) = splitOpts [ ("-c",1)
                               -- , ("--dry",0)
                               -- , ("--no-refs",0)
                               -- , ("--help",0)
                               ] syn

  -- FIXME: migrate-simple-storage!
  --  KISS. just import block/remove block.
  --  let the user backup it.
  --
  --
  --

  conf@(PeerConfig se)  <- peerConfigRead (headMay [p | ListVal [StringLike "-c", StringLike p] <- opts] )

  brains <- newBasicBrains  conf

  bProbe <- newSimpleProbe "Brains"
  brainsThread <- ContT $ withAsync $ runBasicBrains conf brains

  xdg <- liftIO $ getXdgDirectory XdgData defStorePath <&> fromString

  let prefix = fromMaybe xdg $ runReader (cfgValue @PeerStorageKey @(Maybe FilePath)) se

  let store = prefix

  let ncqDir = store </> "ncq"
  let ncqDirBackup = store </> ".ncq.backup"
  let ncq3Dir = store </> "ncq3"

  ncqHere <- Sy.doesDirectoryExist ncqDir

  unless ncqHere $ exit ()

  liftIO $ hPrint stderr $
       "Migrate" <+> pretty ncqDir <> line
       <> "you may remove" <+> pretty ncqDir
       <+> "when migration successfully done"
       <+> "or you may back  it up"  <> line

  flip fix 10 \next i -> do
    liftIO $ hPrint stderr $ pretty i <> "..."
    pause @'Seconds 1
    when (i > 0) $ next (pred i)

  notice "Go!"

  notice "Seek for polled references"

  refs <- listPolledRefs @L4Proto brains Nothing

  rrefs <- S.toList_ <$> for refs $ \(pk, s, _) -> case s of
             "reflog"  -> S.yield (WrapRef $ RefLogKey @'HBS2Basic pk)
             "refchan" -> do
              S.yield (WrapRef $ RefChanLogKey @'HBS2Basic pk)
              S.yield (WrapRef $ RefChanHeadKey @'HBS2Basic pk)
             "lwwref"  -> S.yield (WrapRef $ LWWRefKey @'HBS2Basic pk)
             _         -> none


  notice $ "got references" <+> vcat (pretty <$> rrefs)

  lift $ N.migrateNCQ1 notice rrefs ncqDir ncq3Dir

  notice $ "move" <+> pretty ncqDir <+> pretty ncqDirBackup
     <> line <> "you may remove it if you want"

  mv ncqDir ncqDirBackup


  -- let (opts, argz) = splitOpts [ ("-n",0)
  --                              , ("--dry",0)
  --                              , ("--no-refs",0)
  --                              , ("--help",0)
  --                              ] syn

  -- prefix <- headMay [ p | StringLike p  <- argz ]
  --           & orThrowUser ( "Storage dir not specified" <+> parens ("typically" <+> pretty xdg) <> line
  --                           <> line
  --                           <> "run hbs2-peer migrate"  <+> pretty xdg <> line
  --                           <> "if this is it"
  --                         )


  -- let dry = or [ True | ListVal [StringLike s] <- opts, s `elem` ["--dry","-n"]]

  -- let norefs = or [ True | ListVal [StringLike "--no-refs"] <- opts ]


  -- let store = prefix

  -- let migrateDir = store </> "migrate"
  -- let ncqDir = store </> "ncq"

  -- liftIO $ IO.hSetBuffering stdin  NoBuffering
  -- liftIO $ IO.hSetBuffering stdout LineBuffering

  -- already <- Sy.doesDirectoryExist migrateDir

  -- when already do
  --   liftIO $ hPutDoc stdout $ yellow "Found migration WIP" <+> pretty migrateDir <> "," <+> "continue" <> line

  -- liftIO $ hPutDoc stdout $
  --   yellow "Storage migration process is about to start" <> line
  --    <> "It will convert the current storage structure to a new one (NCQ storage)" <> line
  --    <> "to use with hbs2 0.25.2 and newer" <> line
  --    <> "hbs2-peer 0.25.1 and earlier versions  don't work with the new storage." <> line
  --    <> "If you want to backup your data first just for in case" <> line
  --    <> "You may store the contents of directory" <> line
  --    <> line
  --    <> pretty prefix
  --    <> line <> line
  --    <> "specifically" <+> pretty (prefix </> "blocks") <+> "and" <+> pretty (prefix</> "refs")
  --    <> line
  --    <> "to roll back to the older version --- just restore them"
  --    <> line

  -- liftIO do
  --   IO.hFlush stdout
  --   IO.hFlush stderr
  --   putStr  "Start the migration process? [y]: "
  --   IO.hFlush stdout

  -- y <- liftIO getChar

  -- unless ( toUpper y == 'Y' ) $ exit ()

  -- liftIO do
  --   putStrLn ""

  -- info $ "migration started" <+> pretty opts

  -- info $ "create dir" <+> pretty migrateDir

  -- mkdir migrateDir

  -- wip <- Sy.doesDirectoryExist migrateDir

  -- source <- if dry && not wip then do
  --             pure store
  --           else do
  --             info $ yellow "Real migration," <+> "fasten the sit belts!"
  --             let srcDir = migrateDir </> "source"
  --             mkdir srcDir
  --             let inBlk = store </> "blocks"
  --             let inRefs = store </> "refs"

  --             e1 <- Sy.doesDirectoryExist inBlk

  --             when e1 $ mv  inBlk  (srcDir </> "blocks")

  --             e2 <- Sy.doesDirectoryExist inRefs

  --             when e2 $ mv  inRefs (srcDir </> "refs")

  --             pure srcDir

  -- tmp <- ContT $ Temp.withTempDirectory migrateDir "run"

  -- info $ "create dir" <+> pretty tmp

  -- let blkz  = source </> "blocks"
  -- let refz  = source </> "refs"

  -- let b = tmp </> "blocks"
  -- let r = tmp </> "refs"

  -- info $ "create directory links"
  -- info $ pretty blkz <+> pretty b
  -- liftIO $ createDirectoryLink blkz b

  -- info $ pretty refz <+> pretty r
  -- liftIO $ createDirectoryLink refz r

  -- ncq <- ContT $ withNCQ id ncqDir

  -- let nameToHash fn =
  --       fromString @HashRef $ mconcat $ reverse $ take 2 $ reverse $ splitDirectories fn

  -- let hashToPath ha = do
  --       let (p,r) = splitAt 1 (show $ pretty ha)
  --       p </> r

  -- checkQ <- newTQueueIO
  -- checkN <- newTVarIO 0

  -- errors <- newTVarIO 0

  -- rmp <- liftIO $ async $ fix \next -> do
  --   atomically (readTQueue checkQ) >>= \case
  --     Nothing -> none
  --     Just what -> do

  --       toWipe <- ncqLocate ncq what >>= \case
  --         Just (InCurrent{})    -> do
  --           atomically $ modifyTVar checkN pred
  --           pure True

  --         Just (InFossil{})     -> do
  --           atomically $ modifyTVar checkN pred
  --           pure True

  --         Just (InWriteQueue{}) -> do
  --           atomically $ unGetTQueue checkQ (Just what)
  --           pure False

  --         Nothing               -> do
  --           atomically $ modifyTVar errors succ
  --           pure False

  --       when toWipe do
  --         let path = b </> hashToPath what
  --         info $ yellow "d" <+> pretty what

  --         unless dry do
  --           rm path

  --       next

  -- cnt <- newTVarIO 0

  -- glob ["**/*"] [] b $ \fn -> flip runContT pure $ callCC \next -> do
  --   sz <- liftIO $ getFileSize fn

  --   when (sz >= 1024^3 ) do
  --     err $ red "Block is too large; skipping" <+> pretty fn
  --     next True

  --   when (sz >= 1024^2 ) do
  --     warn $ yellow "Block is too large; but okay" <+> pretty fn

  --   let hs = nameToHash fn

  --   bs <- liftIO $ BS.copy <$> BS.readFile fn
  --   let h = HashRef $ hashObject @HbSync bs

  --   unless ( h == hs ) do
  --     err $ red "Hash doesn't match content" <+> pretty fn
  --     next True

  --   placed <- liftIO $ ncqStoragePutBlock ncq (LBS.fromStrict bs)

  --   flush <- atomically do
  --     n <- readTVar cnt
  --     if n > 1000 then do
  --       writeTVar cnt 0
  --       pure True
  --     else do
  --       modifyTVar cnt succ
  --       pure False

  --   unless ( placed == Just hs ) do
  --     err $ red "NCQ write error" <+> pretty fn
  --     next True

  --   when flush do
  --     liftIO (ncqStorageFlush ncq)

  --   for_ placed $ \hx -> atomically do
  --     writeTQueue checkQ (Just hx)
  --     modifyTVar checkN succ

  --   info $ green "ok" <+> "B" <+> fill 44 (pretty placed) <+> pretty sz

  --   pure True

  -- unless norefs do
  --   glob ["**/*"] [] r $ \fn -> flip runContT pure $ callCC \next -> do

  --     let ref = nameToHash fn

  --     ncqRef <- liftIO $ ncqStorageGetRef ncq ref

  --     when (isJust ncqRef) do
  --       info $ yellow "keep" <+> "R" <+> pretty ref
  --       next True

  --     refTo <- liftIO (readFile fn)
  --               <&> coerce @_ @HashRef . fromString @(Hash HbSync)

  --     here <- liftIO (ncqLocate ncq refTo)

  --     if isJust here then         do
  --       liftIO $ ncqStorageSetRef ncq ref refTo
  --       info $ green "ok" <+> "R" <+> pretty ref <+> pretty refTo
  --     else do
  --       warn $ red "Missed block for ref" <+> pretty ref <+> pretty refTo

  --     pure True

  -- liftIO $ ncqIndexRightNow ncq

  -- info $ "check migration / wait to complete"

  -- atomically $ writeTQueue checkQ Nothing

  -- wait rmp

  -- num <- readTVarIO checkN

  -- when (num == 0) $ exit ()

  -- ee <- readTVarIO errors
  -- rest <- readTVarIO checkN

  -- liftIO $ hPutDoc stdout $ "errors" <+> pretty ee <+> "leftovers" <+> pretty rest

  -- liftIO do
  --   if ee == 0 && rest == 0 then do

  --     unless dry do
  --       rm migrateDir

  --     exitSuccess

  --   else
  --     exitFailure

