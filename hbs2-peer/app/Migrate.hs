module Migrate where

import HBS2.Prelude.Plated
import HBS2.Misc.PrettyStuff
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.OrDie
import HBS2.Defaults
import HBS2.Storage.NCQ
import Log
import PeerConfig

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
import Control.Monad.Cont

import UnliftIO
-- import UnliftIO.Temporary

migrate :: [Syntax C]-> IO ()
migrate syn = flip runContT pure $ callCC \exit -> do

  xdg <- liftIO $ getXdgDirectory XdgData defStorePath <&> fromString


  let (opts, argz) = splitOpts [ ("-n",0)
                               , ("--dry",0)
                               , ("--no-refs",0)
                               , ("--help",0)
                               ] syn

  prefix <- headMay [ p | StringLike p  <- argz ]
            & orThrowUser ( "Storage dir not specified" <+> parens ("typically" <+> pretty xdg) <> line
                            <> line
                            <> "run hbs2-peer migrate"  <+> pretty xdg <> line
                            <> "if this is it"
                          )


  let dry = or [ True | ListVal [StringLike s] <- opts, s `elem` ["--dry","-n"]]

  let norefs = or [ True | ListVal [StringLike "--no-refs"] <- opts ]


  let store = prefix

  let migrateDir = store </> "migrate"
  let ncqDir = store </> "ncq"

  liftIO $ IO.hSetBuffering stdin  NoBuffering
  liftIO $ IO.hSetBuffering stdout LineBuffering

  already <- Sy.doesDirectoryExist migrateDir

  when already do
    liftIO $ hPutDoc stdout $ yellow "Found migration WIP" <+> pretty migrateDir <> "," <+> "continue" <> line

  liftIO $ hPutDoc stdout $
    yellow "Storage migration process is about to start" <> line
     <> "It will convert the current storage structure to a new one (NCQ storage)" <> line
     <> "to use with hbs2 0.25.2 and newer" <> line
     <> "hbs2-peer 0.25.1 and earlier versions  don't work with the new storage." <> line
     <> "If you want to backup your data first just for in case" <> line
     <> "You may store the contents of directory" <> line
     <> line
     <> pretty prefix
     <> line <> line
     <> "specifically" <+> pretty (prefix </> "blocks") <+> "and" <+> pretty (prefix</> "refs")
     <> line
     <> "to roll back to the older version --- just restore them"
     <> line

  liftIO do
    IO.hFlush stdout
    IO.hFlush stderr
    putStr  "Start the migration process? [y]: "
    IO.hFlush stdout

  y <- liftIO getChar

  unless ( toUpper y == 'Y' ) $ exit ()

  liftIO do
    putStrLn ""

  info $ "migration started" <+> pretty opts

  info $ "create dir" <+> pretty migrateDir

  mkdir migrateDir

  wip <- Sy.doesDirectoryExist migrateDir

  source <- if dry && not wip then do
              pure store
            else do
              info $ yellow "Real migration," <+> "fasten the sit belts!"
              let srcDir = migrateDir </> "source"
              mkdir srcDir
              let inBlk = store </> "blocks"
              let inRefs = store </> "refs"

              e1 <- Sy.doesDirectoryExist inBlk

              when e1 $ mv  inBlk  (srcDir </> "blocks")

              e2 <- Sy.doesDirectoryExist inRefs

              when e2 $ mv  inRefs (srcDir </> "refs")

              pure srcDir

  tmp <- ContT $ Temp.withTempDirectory migrateDir "run"

  info $ "create dir" <+> pretty tmp

  let blkz  = source </> "blocks"
  let refz  = source </> "refs"

  let b = tmp </> "blocks"
  let r = tmp </> "refs"

  info $ "create directory links"
  info $ pretty blkz <+> pretty b
  liftIO $ createDirectoryLink blkz b

  info $ pretty refz <+> pretty r
  liftIO $ createDirectoryLink refz r

  ncq <- ContT $ withNCQ id ncqDir

  let nameToHash fn =
        fromString @HashRef $ mconcat $ reverse $ take 2 $ reverse $ splitDirectories fn

  let hashToPath ha = do
        let (p,r) = splitAt 1 (show $ pretty ha)
        p </> r

  checkQ <- newTQueueIO
  checkN <- newTVarIO 0

  errors <- newTVarIO 0

  rmp <- liftIO $ async $ fix \next -> do
    atomically (readTQueue checkQ) >>= \case
      Nothing -> none
      Just what -> do

        toWipe <- ncqLocate ncq what >>= \case
          Just (InCurrent{})    -> do
            atomically $ modifyTVar checkN pred
            pure True

          Just (InFossil{})     -> do
            atomically $ modifyTVar checkN pred
            pure True

          Just (InWriteQueue{}) -> do
            atomically $ unGetTQueue checkQ (Just what)
            pure False

          Nothing               -> do
            atomically $ modifyTVar errors succ
            pure False

        when toWipe do
          let path = b </> hashToPath what
          info $ yellow "d" <+> pretty what

          unless dry do
            rm path

        next

  glob ["**/*"] [] b $ \fn -> flip runContT pure $ callCC \next -> do
    sz <- liftIO $ getFileSize fn

    when (sz >= 1024^3 ) do
      err $ red "Block is too large; skipping" <+> pretty fn
      next True

    when (sz >= 1024^2 ) do
      warn $ yellow "Block is too large; but okay" <+> pretty fn

    let hs = nameToHash fn

    bs <- liftIO $ BS.readFile fn
    let h = HashRef $ hashObject @HbSync bs

    unless ( h == hs ) do
      err $ red "Hash doesn't match content" <+> pretty fn
      next True

    placed <- liftIO $ ncqStoragePutBlock ncq (LBS.fromStrict bs)

    unless ( placed == Just hs ) do
      err $ red "NCQ write error" <+> pretty fn
      next True

    for_ placed $ \hx -> atomically do
      writeTQueue checkQ (Just hx)
      modifyTVar checkN succ

    info $ green "ok" <+> "B" <+> fill 44 (pretty placed) <+> pretty sz

    pure True

  unless norefs do
    glob ["**/*"] [] r $ \fn -> flip runContT pure $ callCC \next -> do

      let ref = nameToHash fn

      ncqRef <- liftIO $ ncqStorageGetRef ncq ref

      when (isJust ncqRef) do
        info $ yellow "keep" <+> "R" <+> pretty ref
        next True

      refTo <- liftIO (readFile fn)
                <&> coerce @_ @HashRef . fromString @(Hash HbSync)

      here <- ncqLocate ncq refTo

      if isJust here then         do
        liftIO $ ncqStorageSetRef ncq ref refTo
        info $ green "ok" <+> "R" <+> pretty ref <+> pretty refTo
      else do
        warn $ red "Missed block for ref" <+> pretty ref <+> pretty refTo

      pure True

  liftIO $ ncqIndexRightNow ncq

  info $ "check migration / wait to complete"

  atomically $ writeTQueue checkQ Nothing

  wait rmp

  num <- readTVarIO checkN

  when (num == 0) $ exit ()

  ee <- readTVarIO errors
  rest <- readTVarIO checkN

  liftIO $ hPutDoc stdout $ "errors" <+> pretty ee <+> "leftovers" <+> pretty rest

  liftIO do
    if ee == 0 && rest == 0 then do

      unless dry do
        rm migrateDir

      exitSuccess

    else
      exitFailure

