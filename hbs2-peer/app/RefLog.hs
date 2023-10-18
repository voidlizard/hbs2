{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module RefLog where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Actors.Peer
import HBS2.Events
import HBS2.Data.Types.Refs
import HBS2.Data.Detect
import HBS2.Net.PeerLocator
import HBS2.Net.Proto
import HBS2.Base58
import HBS2.Storage
import HBS2.Storage.Operations.Missed
import HBS2.Hash
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.RefLog
import HBS2.Net.Proto.Sessions
import HBS2.Net.Auth.Credentials
import HBS2.Merkle

import HBS2.System.Logger.Simple

import Brains
import PeerConfig
import PeerTypes
import CheckBlockAnnounce (acceptAnnouncesFromPeer)

import Data.Function(fix)
import Data.Maybe
import Data.Foldable(for_)
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Codec.Serialise
import Data.HashSet qualified as HashSet
import Data.HashSet (HashSet)
import Control.Concurrent.Async
import Control.Monad.Trans.Maybe
import Lens.Micro.Platform

doRefLogBroadCast :: forall e m . ( MonadIO m
                                  , MyPeer e
                                  , HasPeerLocator e m
                                  , Request e (RefLogUpdate e) m
                                  , Sessions e (KnownPeer e) m
                                  )
                   => RefLogUpdate e -> m ()

doRefLogBroadCast msg = do
  -- TODO: broadcast-reflog-update
  trace "doRefLogBroadCast"
  forKnownPeers $ \pip _ -> do
    trace $ "send msg to peer" <+> pretty pip
    request @e pip msg


mkRefLogRequestAdapter :: forall e s m . ( MonadIO m
                                         , HasPeerLocator e m
                                         , MyPeer e
                                         , HasStorage m
                                         , IsRefPubKey s
                                         , Pretty (AsBase58 (PubKey 'Sign s))
                                         , s ~ Encryption e
                                         )
          => m (RefLogRequestI e (ResponseM e m ))
mkRefLogRequestAdapter = do
  sto <- getStorage
  pure $ RefLogRequestI (doOnRefLogRequest sto) dontHandle


doOnRefLogRequest :: forall e s m . ( MonadIO m
                                    , MyPeer e
                                    , s ~ Encryption e
                                    , IsRefPubKey s
                                    )
                   =>  AnyStorage -> (Peer e, PubKey 'Sign s) -> m (Maybe (Hash HbSync))

doOnRefLogRequest sto (_,pk) = liftIO $ getRef sto (RefLogKey @s pk)


data RefLogWorkerAdapter e =
 RefLogWorkerAdapter
 { reflogDownload :: Hash HbSync -> IO ()
 , reflogFetch    :: PubKey 'Sign (Encryption e) -> IO ()
 }

reflogWorker :: forall e s m . ( e ~ L4Proto
                               , MonadIO m, MyPeer e
                               , EventListener e (RefLogUpdateEv e) m
                               , EventListener e (RefLogRequestAnswer e) m
                               , HasStorage m
                               , Nonce (RefLogUpdate e) ~ BS.ByteString
                               , Serialise (RefLogUpdate e)
                               , EventEmitter e (RefLogUpdateEv e) m -- (PeerM e m)
                               , Sessions L4Proto (KnownPeer L4Proto) m
                               , Signatures s
                               , s ~ Encryption e
                               , IsRefPubKey s
                               , IsPeerAddr e m
                               , Pretty (AsBase58 (PubKey 'Sign s))
                              )
             => PeerConfig
             -> SomeBrains e
             -> RefLogWorkerAdapter e
             -> m ()

reflogWorker conf brains adapter = do

  sto <- getStorage

  pQ <- liftIO newTQueueIO

  let reflogTimeout puk h = do
        -- FIXME: fix-time-hardcode-again
        pause @'Seconds 1200
        err $ "reflog dowload timeout" <+> pretty (AsBase58 puk) <+> pretty h

  let reflogUpdate reflog _ tran = do
        signed <- verifyRefLogUpdate tran

        when signed do

          liftIO $ atomically $ writeTQueue pQ (reflog, [tran])

          -- FIXME: fix-this-copypaste
          let bss = view refLogUpdData tran
          let what = tryDetect (hashObject bss) (LBS.fromStrict bss)
          case what of
            SeqRef (SequentialRef _ (AnnotatedHashRef ann ref)) -> do
              liftIO $ reflogDownload adapter (fromHashRef ref)
              liftIO $ forM_ ann (reflogDownload adapter . fromHashRef)

            -- TODO: asap-download-annotation-as-well
            AnnRef (AnnotatedHashRef ann  ref) -> do
              liftIO $ reflogDownload adapter (fromHashRef ref)
              liftIO $ forM_ ann (reflogDownload adapter . fromHashRef)

            -- TODO: support-other-data-structures
            _ -> pure ()

  subscribe @e RefLogUpdateEvKey $ \(RefLogUpdateEvData (reflog,v, mpip)) -> do
    trace $ "reflog worker.got refupdate" <+> pretty (AsBase58 reflog)

    polled <- isPolledRef @e brains reflog
    buddy <- maybe1 mpip (pure False) $ \pip -> do
                pa <- toPeerAddr @e pip
                acceptAnnouncesFromPeer @e conf pa

    when (buddy || polled) $ liftIO do
      reflogUpdate reflog Nothing v
      atomically $ writeTQueue pQ (reflog, [v])

  reflogMon <- liftIO $ newTVarIO (mempty :: HashSet (Hash HbSync))

  subscribe @e RefLogReqAnswerKey $ \(RefLogReqAnswerData reflog h) -> do
    -- TODO: ASAP-only-process-link-if-we-subscribed
    -- TODO: ASAP-start-only-one-instance-for-link-monitor
    -- TODO: ASAP-dont-do-if-already-done

    here <- liftIO $ readTVarIO reflogMon <&> HashSet.member h
    unless here do
      liftIO $ atomically $ modifyTVar' reflogMon (HashSet.insert h)
      void $ liftIO $ async $ do
        timeout <-  async (reflogTimeout reflog h)
        work <- async $ do
          trace $ "reflog worker. GOT REFLOG ANSWER" <+> pretty (AsBase58 reflog) <+> pretty h
          reflogDownload adapter h
          fix \next -> do
            missed <- findMissedBlocks sto (HashRef h)
            if not (null missed) then do
              for_ missed $ reflogDownload adapter . fromHashRef
              pause @'Seconds 1
              debug $ "reflogWorker: MISSED REFS FOR" <+> pretty h <+> pretty missed
              next
            else do
              trace $ "block" <+> pretty h <+> "is downloaded"
              hashes <- readHashesFromBlock sto (Just h)
              for_ hashes $ \ha -> runMaybeT do
                bss <- liftIO $ getBlock sto (fromHashRef ha)

                when (isNothing bss) do
                  trace $ "missed block for " <+> pretty ha

                liftIO $ reflogDownload adapter (fromHashRef ha)

                bs <- MaybeT $ pure bss

                tran <- MaybeT $ pure $ deserialiseOrFail @(RefLogUpdate e) bs & either (const Nothing) Just
                liftIO $ reflogUpdate reflog (Just ha) tran

        void $ waitAnyCatchCancel [timeout,work]
        atomically $ modifyTVar' reflogMon (HashSet.delete h)

  let (PeerConfig syn) = conf

  poller <- liftIO $ async do
               let listRefs = listPolledRefs @e brains (Just "reflog")
                               <&> fmap (\(a,_,b) -> (a,b))
                               <&> fmap (over _2 ( (*60) . fromIntegral) )

               polling (Polling 5 5) listRefs $ \ref -> do
                debug $ "POLLING REFLOG" <+> pretty (AsBase58 ref)
                reflogFetch adapter ref

  w1 <- liftIO $ async $ forever $ replicateConcurrently_ 4 do

          -- TODO: reflog-process-period-to-config
          -- pause @'Seconds 10

          _ <- liftIO $ atomically $ peekTQueue pQ

          els <- liftIO $ atomically $ flushTQueue pQ
          let byRef = HashMap.fromListWith (<>) els

          for_ (HashMap.toList byRef) $ \(r,x) -> do
            let reflogkey = RefLogKey @s r

            h' <- liftIO $! getRef sto (RefLogKey @s r)

            hashes <- liftIO $ readHashesFromBlock sto h' <&> HashSet.fromList

            -- save new transaction, must be idempotent
            newHashes <- liftIO $ mapM (putBlock sto . serialise) x <&> catMaybes
                                                                    <&> fmap HashRef
                                                                    <&> HashSet.fromList

            let already = newHashes `HashSet.isSubsetOf` hashes


            unless already do
              -- TODO: needs-very-fast-sort-and-dedupe
              let hashesNew = (hashes <> newHashes) & HashSet.toList

              -- FIXME: remove-chunk-num-hardcode
              let pt = toPTree (MaxSize 256) (MaxNum 256) hashesNew

              newRoot <- liftIO do
                nref <- makeMerkle 0 pt $ \(_,_,bss) -> do
                  void $ putBlock sto bss

                updateRef sto reflogkey nref
                pure nref

              -- TODO: old-root-to-delete

              trace $ "new reflog value" <+> pretty (AsBase58 r) <+> pretty (hashObject @HbSync reflogkey) <+> pretty newRoot

          -- trace  "I'm a reflog update worker"

  void $ liftIO $ waitAnyCatchCancel [w1, poller]

  where

    readHashesFromBlock _ Nothing = pure mempty
    readHashesFromBlock sto (Just h) = do
      treeQ <- liftIO newTQueueIO
      walkMerkle h (getBlock sto) $ \hr -> do
        case hr of
          Left{} -> pure ()
          Right (hrr :: [HashRef]) -> atomically $ writeTQueue treeQ hrr
      re <- liftIO $ atomically $ flushTQueue treeQ
      pure $ mconcat re



