{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}
module RefChan (
    RefChanWorkerEnv(..)
  , refChanWorkerEnvHeadQ
  , refChaWorkerEnvDownload
  , refChanOnHead
  , refChanWorker
  , refChanWorkerEnv
  ) where

import HBS2.Prelude.Plated

import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Clock
import HBS2.Data.Detect
import HBS2.Data.Types.Refs
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto
import HBS2.Net.Proto.RefChan
import HBS2.Net.Proto.Types
import HBS2.Storage

import HBS2.System.Logger.Simple

import PeerTypes
import PeerConfig
import BlockDownload

import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import UnliftIO
import Lens.Micro.Platform
import Control.Monad.Except (throwError, runExceptT)
import Data.Maybe
import Control.Exception ()
import Control.Monad.Trans.Maybe

import Streaming.Prelude qualified as S
import Streaming qualified as S

{- HLINT ignore "Use newtype instead of data" -}

data DataNotReady = DataNotReady deriving (Show)

instance Exception DataNotReady

data RefChanWorkerEnv e =
  RefChanWorkerEnv
  { _refChanWorkerEnvDownload :: DownloadEnv e
  , _refChanWorkerEnvHeadQ    :: TQueue (RefChanHeadBlockTran e)
  , _refChaWorkerEnvDownload  :: TVar (HashMap HashRef ()) -- таймстемп можно
  }

makeLenses 'RefChanWorkerEnv

refChanWorkerEnv :: forall m e . MonadIO m
                 => PeerConfig
                 -> DownloadEnv e
                 -> m (RefChanWorkerEnv e)

refChanWorkerEnv _ de = liftIO $ RefChanWorkerEnv @e de <$> newTQueueIO
                                                        <*> newTVarIO mempty


refChanOnHead :: MonadIO m => RefChanWorkerEnv e -> RefChanHeadBlockTran e -> m ()
refChanOnHead env tran = do
  atomically $ writeTQueue (view refChanWorkerEnvHeadQ env) tran

refChanAddDownload :: forall e m . ( m ~ PeerM e IO
                                   , MyPeer e
                                   , Block ByteString ~ ByteString
                                   )
                   => RefChanWorkerEnv e -> HashRef -> m ()
refChanAddDownload env r = do
  penv <- ask
  withPeerM penv $ withDownload (_refChanWorkerEnvDownload env)
                 $ processBlock @e (fromHashRef r)

  atomically $ modifyTVar (view refChaWorkerEnvDownload env) (HashMap.insert r ())

checkDownloaded :: forall m . (MonadIO m, HasStorage m, Block ByteString ~ ByteString) => HashRef -> m Bool
checkDownloaded hr = do
  sto <- getStorage
  let readBlock h = liftIO $ getBlock sto h
  result <- runExceptT $ deepScan ScanDeep (const $ throwError DataNotReady) (fromHashRef hr) readBlock dontHandle
  pure $ either (const False) (const True) result


-- FIXME: move-to-library
readBlob :: forall m . ( MonadIO m
                       , HasStorage m
                       , Block ByteString ~ ByteString
                       )
         => HashRef
         -> m (Maybe ByteString)

readBlob hr = do
  sto <- getStorage
  let readBlock h = liftIO $ getBlock sto h

  chunks <- S.toList_ $
              deepScan ScanDeep (const $ S.yield Nothing) (fromHashRef hr) readBlock $ \ha -> do
                unless (fromHashRef hr == ha) do
                  readBlock ha >>= S.yield

  pure $ LBS.concat <$> sequence chunks

refChanWorker :: forall e s m . ( MonadIO m
                                , MonadUnliftIO m
                                , MyPeer e
                                , HasStorage m
                                , Signatures s
                                , s ~ Encryption e
                                , IsRefPubKey s
                                , Pretty (AsBase58 (PubKey 'Sign s))
                                , Block ByteString ~ ByteString
                                , ForRefChans e
                                , m ~ PeerM e IO
                                )
             => RefChanWorkerEnv e
             -> m ()

refChanWorker env = do

  hw <- async refChanHeadMon
  downloads <- async monitorDownloads

  forever do
   pause @'Seconds 10
   debug "I'm refchan worker"

  mapM_ wait [hw,downloads]

  where

    monitorDownloads = forever do
      pause @'Seconds 2
      all <- atomically $ readTVar (view refChaWorkerEnvDownload env) <&> HashMap.keys

      -- FIXME: consider-timeouts-or-leak-is-possible
      rest <- forM all $ \r -> do
                here <- checkDownloaded r
                if here then do
                  refChanOnHead env (RefChanHeadBlockTran r)
                  pure mempty
                else do
                  pure [(r,())]

      atomically $ writeTVar (view refChaWorkerEnvDownload env) (HashMap.fromList (mconcat rest))

    -- FIXME: in-parallel?
    refChanHeadMon = do
      forever do
        RefChanHeadBlockTran hr <- atomically $ readTQueue (view refChanWorkerEnvHeadQ env)
        -- debug $ "DROP HEAD UPDATE" <+> pretty (fromRefChanHeadBlockTran tran)

        here <- checkDownloaded hr

        if not here then do
          refChanAddDownload env hr
          trace $ "BLOCK IS NOT HERE" <+> pretty hr
        else do
          trace $ "BLOCK IS HERE" <+> pretty hr
          -- читаем блок
          lbs <- readBlob hr <&> fromMaybe mempty
          let what = unboxSignedBox @(RefChanHeadBlock e) @e lbs

          case what of
            Nothing  -> err $ "malformed head block" <+> pretty hr

            Just (pk,blk) ->  do
              let rkey = RefChanHeadKey @s pk

              sto <- getStorage

              debug $ "Good head block" <+> pretty hr <+> "processing..."

              ourVersion <- runMaybeT do


                cur <- MaybeT $ liftIO $ getRef sto rkey

                lbss <- MaybeT $ readBlob (HashRef cur)

                (_, blkOur) <- MaybeT $ pure $ unboxSignedBox @(RefChanHeadBlock e) @e lbss

                pure $ view refChanHeadVersion blkOur

              let v0 = fromMaybe 0 ourVersion
              let v1 = view refChanHeadVersion blk

              if v1 > v0 then do
                debug $ "UPDATING HEAD BLOCK" <+> pretty (v1, v0)
                liftIO $ updateRef sto rkey (fromHashRef hr)
              else do
                debug $ "LEAVING HEAD BLOCK" <+> pretty (v1, v0)

          pure ()
          -- распаковываем блок
          -- вытаскиваем ключ из блока?

        pure ()

      -- если всё скачано --- то обрабатываем.
      -- если не скачано -- то говорим качать и ждём. как ждём?
      -- помещаем в фигню, которая download запускает, и время от времени ждёт,
      -- пока скачается. как скачается -- убирает из своего локального стейта,
      -- и пихает транзу обратно в эту очередь, допустим.


