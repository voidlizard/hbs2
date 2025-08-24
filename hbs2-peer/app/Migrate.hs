{-# Language MultiWayIf #-}
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
import Data.Config.Suckless.Script.File (glob,globSafer)
import Data.Config.Suckless.System as Sy
import Data.Config.Suckless.Almost.RPC

import Data.List qualified as List
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
import Control.Exception as E
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Either
import Data.Fixed

import Graphics.Vty qualified as Vty
import Graphics.Vty.Input qualified as Vty
import Graphics.Vty.Input hiding (Event)
import Graphics.Vty (Mode(..),setMode,outputIface,inputIface)
import Graphics.Vty.Platform.Unix qualified as Vty

import UnliftIO as U
-- import UnliftIO.Temporary

import Streaming.Prelude qualified as S

data E = B FilePath | R FilePath

migrateSS :: MonadUnliftIO m
          => FilePath
          -> FilePath
          -> [WrapRef]
          -> m ()
migrateSS prefix target refs = flip runContT pure $ callCC \exit -> do
  notice "migrate from simple-storage to ncq3"

  let blocksDir = prefix </> "blocks"
  let refsDir   = prefix </> "refs"
  here <- Sy.doesDirectoryExist blocksDir

  unless here $ exit ()

  sto <- ContT $ ncqWithStorage target

  let mlog = ncqGetFileName sto "migrate-ss.log"
  touch mlog

  refs   <- newTVarIO ( mempty :: HashSet HashRef )
  blocks <- newTVarIO ( mempty :: HashSet HashRef )
  blkDone <- newTVarIO 0
  refDone <- newTVarIO 0
  terr    <- newTVarIO 0

  t0 <- getTimeCoarse

  entries <- liftIO (readFile mlog) <&> lines

  liftIO $ print $ "read log" <+> pretty (List.length entries)

  -- cfg <- pure $ Vty.defaultConfig
  -- vty <- ContT $ U.bracket (liftIO (Vty.mkVty cfg)) (liftIO . Vty.shutdown)

  for_ entries $ \e -> do
    wtf <- parseTop (e <> "\n") & either (error.show) pure
    case wtf of
      [ListVal [SymbolVal "block", HashLike x]] -> atomically $ modifyTVar blocks (HS.insert x)

      [ListVal [SymbolVal "ref",  HashLike x]] -> atomically $ modifyTVar refs (HS.insert x)

      [ListVal [SymbolVal "finished"]] -> do
        liftIO $ hPutDoc  stderr $
           red "migration is already done"
            <> "remove" <+> pretty blocksDir <+> "and" <+> pretty refsDir
            <> line
            <> "directories if they still here"

        exit ()

      _ -> none

  n <- readTVarIO blocks   <&> HS.size

  liftIO $ print $ "already imported" <+> pretty n

  importQ <- newTBQueueIO 10000

  ContT $ withAsync do
    globSafer ["**/*"] [] blocksDir $ \fn -> do

      atomically do
        writeTBQueue importQ (Just (B fn))

      pure True

    globSafer ["**/*"] [] refsDir $ \fn -> do
      atomically $ writeTBQueue importQ (Just (R fn))
      pure True

    atomically $ writeTBQueue importQ Nothing

  tr1 <- ContT $ withAsync $ flip runContT pure $ callCC \stop ->  fix \next -> do

    atomically (readTBQueue importQ) >>= \case
      Nothing -> do
        liftIO $ hPutStrLn stderr "done"
        stop ()

      Just (R fn) -> (>> next) $ void $ runMaybeT do

        atomically $ modifyTVar refDone succ

        h' <- lift $ lift $ nameToHash fn

        when (isLeft h') do
          liftIO $ hPrint stderr $ "invalid ref" <+> pretty fn
          atomically $ modifyTVar terr succ
          rm fn

        h <- toMPlus h'

        co <- liftIO $ E.try  @SomeException do
                 s <- readFile fn
                 E.evaluate (coerce @_ @HashRef $ fromString @HashRef s)

        when (isLeft co) do
          liftIO $ hPrint stderr $ "invalid ref value" <+> pretty fn
          atomically $ modifyTVar terr succ
          rm fn

        val <- toMPlus co

        liftIO $ ncqStorageSetRef sto h val
        liftIO $ appendFile mlog (show $ "ref" <+> pretty h <> line)

      Just (B fn) -> (>> next) $ void $ runMaybeT do

        atomically $ modifyTVar blkDone succ

        h' <- lift $ lift $ nameToHash fn

        when (isLeft h') do
          liftIO $ hPrint stderr $ "invalid block" <+> pretty fn
          atomically $ modifyTVar terr succ
          rm fn

        h <- toMPlus h'

        here <- readTVarIO blocks <&> HS.member h
        there <- liftIO $ hasBlock sto (coerce h) <&> isJust

        unless ( here || there ) do
          -- liftIO $ hPrint stderr $ pretty fn <+> pretty h
          bs <- liftIO (BS.readFile fn)
          let h0 = HashRef (hashObject @HbSync bs)

          if h0 /= h then do
            liftIO $ hPrint stderr $ "invalid block" <+> pretty fn
            atomically $ modifyTVar terr succ
            rm fn

          else do
            h1 <- fmap HashRef <$> liftIO (putBlock sto (LBS.fromStrict bs))
            when (h1 == Just h0) do
              liftIO $ appendFile mlog (show $ "block" <+> pretty h0 <> line)


  fix \again -> do
      enum <- readTVarIO terr
      n <- readTVarIO blkDone
      r <- readTVarIO refDone
      liftIO $ hPutStr stderr $ show
        $ "blocks" <+> pretty n
        <+> "refs" <+> pretty r
        <+> "errors" <+> pretty enum
        <> "                                             \r"

      done <- poll tr1 <&> isJust
      unless done do
        pause @'Seconds 2
        again

  e <- liftIO $ U.try @_ @SomeException do
        touch (prefix </> "ss-ncq3-done")
        mv blocksDir (prefix </> ".blocks.backup")
        mv refsDir   (prefix </> ".refs.backup")

  case e of
    Right{} -> do
      none
    Left e -> do
      liftIO $ hPutStrLn stderr "can't rename storage directories blocks and refs ; move/delete/backup them on your own"

  liftIO $ appendFile mlog "finished"
  touch (prefix </> "ss-ncq3-done")

  where

  nameToHash fn =
    U.try @_ @SomeException (U.evaluate (fromString @HashRef $ mconcat $ reverse $ take 2 $ reverse $ splitDirectories (dropExtension fn)))

migrate :: [Syntax C]-> IO ()
migrate syn = flip runContT pure $ callCC \exit -> do

  let (opts, argz) = splitOpts [ ("-c",1)
                               , ("--okay",0)
                               , ("--delete",0)
                               , ("--help",0)
                               ] syn

  -- FIXME: migrate-simple-storage!
  --  KISS. just import block/remove block.
  --  let the user backup it.

  let okay   = or [ True | StringLike "--okay" <- opts ]
  let delete = or [ True | StringLike "--delete" <- opts ]

  conf@(PeerConfig se)  <- peerConfigRead (headMay [p | ListVal [StringLike "-c", StringLike p] <- opts] )

  brains <- newBasicBrains  conf

  brainsThread <- ContT $ withAsync $ runBasicBrains conf brains

  xdg <- liftIO $ getXdgDirectory XdgData defStorePath <&> fromString

  let prefix = fromMaybe xdg $ runReader (cfgValue @PeerStorageKey @(Maybe FilePath)) se

  let store = prefix

  let blkDir = store </> "blocks"
  let ncqDir = store </> "ncq"
  let ncqDirBackup = store </> ".ncq.backup"
  let ncq3Dir = store </> "ncq3"


  liftIO $ hPutDoc stderr $ line
    <> "This is a storage migration procedure" <> line
    <> "Storage is located in" <+> pretty store <> line <> line
    <> "You may backup it first (ncq, blocks, refs)" <> line
    <> "You may also run it with parameter" <+> ul "--okay" <+> "to skip warnings" <> line
    <> "and with" <+> ul "--delete" <+> "if you want to remove obsolete storage files ASAP" <> line
    <> "which is usefull when you running out of storage, but there is a risk of loosing some data" <> line
    <> "in case if something goes wrong"
    <> line

  unless okay $ flip fix 5 \next i -> do
    liftIO $ hPrint stderr $ pretty i <> "..."
    pause @'Seconds 1
    when (i > 1) $ next (pred i)

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


  blkHere <- Sy.doesDirectoryExist blkDir

  when blkHere do
    lift $ migrateSS prefix ncq3Dir rrefs

  ncqHere <- Sy.doesDirectoryExist ncqDir

  when ncqHere do

    lift $ N.migrateNCQ1 notice rrefs ncqDir ncq3Dir

    notice $ "move" <+> pretty ncqDir <+> pretty ncqDirBackup
       <> line <> "you may remove it if you want"

    touch (prefix </> "ncq-ncq3-done")

    mv ncqDir ncqDirBackup

