{-# Language AllowAmbiguousTypes #-}
module DownloadQ where

import HBS2.Prelude
import HBS2.Clock
import HBS2.Hash
import HBS2.Events
import HBS2.Data.Types.Refs
import HBS2.Actors.Peer
import HBS2.Net.PeerLocator
import HBS2.Storage
import HBS2.Merkle
import HBS2.System.Logger.Simple

import PeerTypes
import PeerConfig
import BlockDownload (processBlock)

import Data.Map qualified as Map
import Data.Foldable
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.ByteString.Char8 qualified as B8
import Data.List (nub)
import Data.Maybe
import Data.Functor
import Data.Function
import Control.Exception
import Control.Monad
import Control.Concurrent.Async
import System.IO


downloadLogAppend :: forall e m . ( MonadIO m
                                  , EventEmitter e (DownloadReq e) m
                                  , DownloadFromPeerStuff e m
                                  ) => Hash HbSync -> m ()
downloadLogAppend h = do
  emit @e DownloadReqKey (DownloadReqData h)

noLogFile :: MonadIO m => m ()
noLogFile = err "download log not defined"

downloadQueue :: forall e m . ( MyPeer e
                              , DownloadFromPeerStuff e m
                              , HasPeerLocator e (BlockDownloadM e m)
                              , HasPeerLocator e m
                              , EventListener e (DownloadReq e) m
                              ) => PeerConfig -> DownloadEnv e -> m ()

downloadQueue conf denv = do

  sto <- getStorage
  hq <- liftIO newTQueueIO
  fsem <- liftIO $ atomically $ newTSem 1

  pause @'Seconds 2

  let qfile' = cfgValue @PeerDownloadLogKey conf :: Maybe String

  subscribe @e DownloadReqKey $ \(DownloadReqData h) -> do
    liftIO $ atomically $ writeTQueue hq h

  maybe1 qfile' noLogFile $ \fn -> do
    void $ liftIO $ async $ forever $ do
      pause @'Seconds 10
      fromq <- liftIO $ atomically $ flushTQueue hq
      unless (null fromq) do
        atomically $ waitTSem fsem
        catchAny ( B8.appendFile fn ( B8.unlines (fmap (B8.pack.show.pretty) fromq) ) )
                 whimper
        atomically $ signalTSem fsem

  maybe1 qfile' noLogFile $ \fn -> forever do

    debug $ "downloadQueue" <+> pretty fn

    lo <- liftIO do

      -- FIXME: will-crash-on-big-logs
      atomically $ waitTSem fsem
      r <- catchAny (B8.readFile fn) (\e -> whimper e >> pure "")
      atomically $ signalTSem fsem

      let hashes = B8.lines r & mapMaybe (fromStringMay . B8.unpack) & nub :: [Hash HbSync]

      fromq <- liftIO $ atomically $ flushTQueue hq
      let hashesWip = nub ( hashes <> fromq )

      errnum <- newTQueueIO

      let walk h = walkMerkle h (getBlock sto) $ \(hr :: Either (Hash HbSync) [HashRef]) -> do
            case hr of
              Left{} -> atomically $ writeTQueue errnum (h,True)
              Right (hrr :: [HashRef]) -> do
                 forM_ hrr $ \(HashRef hx) -> do
                     mblk <- hasBlock sto hx
                     case mblk of
                       Nothing  -> atomically $ writeTQueue errnum (h,True)
                       _ -> pure ()

      for_ hashesWip walk

      loosers <- atomically $ flushTQueue errnum <&> Map.fromListWith (||) <&> Map.filter id

      -- debug $ vcat (fmap pretty (Map.toList loosers))

      let leftovers = [ x | x <- hashesWip , Map.member x loosers ]


      atomically $ waitTSem fsem
      catchAny ( B8.writeFile fn ( B8.unlines (fmap (B8.pack.show.pretty) leftovers) ) )
               whimper
      atomically $ signalTSem fsem

      pure leftovers

    for_ lo $ withDownload denv . processBlock

    debug "downloadQueue okay"

    -- TODO: remove-downloadQueue-pause-hardcode
    pause @'Seconds 150
    -- FIXME: only-debug-20-sec

  where
    whimper e = err (pretty $ show e)

    catchAny :: IO a -> (SomeException -> IO a) -> IO a
    catchAny = Control.Exception.catch


