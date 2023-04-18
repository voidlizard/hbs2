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
import HBS2.Hash
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.RefLog
import HBS2.Net.Proto.Sessions
import HBS2.Net.Auth.Credentials
import HBS2.Merkle

import HBS2.System.Logger.Simple

import PeerConfig
import PeerTypes

import Data.Functor
import Data.Function(fix)
import Data.Maybe
import Data.Foldable(for_)
import Data.Text qualified as Text
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

mkAdapter :: forall e s m . ( MonadIO m
                           , HasPeerLocator e m
                           , Sessions e (KnownPeer e) m
                           , Request e (RefLogUpdate e) m
                           , MyPeer e
                           -- , Pretty (AsBase58 (PubKey 'Sign s))
                           , s ~ Encryption e
                           )
          => m (RefLogUpdateI e (ResponseM e m ))

mkAdapter = do
  let bcast = lift . doRefLogBroadCast @e
  pure $ RefLogUpdateI bcast


data RefLogWorkerAdapter e =
 RefLogWorkerAdapter
 { reflogDownload :: Hash HbSync -> IO ()
 , reflogFetch    :: PubKey 'Sign (Encryption e) -> IO ()
 }

reflogWorker :: forall e s m . ( MonadIO m, MyPeer e
                               , EventListener e (RefLogUpdateEv e) m
                               , EventListener e (RefLogRequestAnswer e) m
                             -- , Request e (RefLogRequest e) (Peerm
                               , HasStorage m
                               , Nonce (RefLogUpdate e) ~ BS.ByteString
                               , Serialise (RefLogUpdate e)
                               , EventEmitter e (RefLogUpdateEv e) m -- (PeerM e m)
                               , Signatures s
                               , s ~ Encryption e
                               , IsRefPubKey s
                               , Pretty (AsBase58 (PubKey 'Sign s))
                              )
             => PeerConfig
             -> RefLogWorkerAdapter e
             -> m ()

reflogWorker conf adapter = do

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
            SeqRef (SequentialRef _ (AnnotatedHashRef _ ref)) -> do
              liftIO $ reflogDownload adapter (fromHashRef ref)

            -- TODO: asap-download-annotation-as-well
            AnnRef (AnnotatedHashRef _ ref) -> do
              liftIO $ reflogDownload adapter (fromHashRef ref)

            -- TODO: support-other-data-structures
            _ -> pure ()

  subscribe @e RefLogUpdateEvKey $ \(RefLogUpdateEvData (reflog,v)) -> do
    trace $ "reflog worker.got refupdate" <+> pretty (AsBase58 reflog)
    liftIO $ reflogUpdate reflog Nothing v
    liftIO $ atomically $ writeTQueue pQ (reflog, [v])


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
            missed <- missedEntries sto h
            if not (null missed) then do
              for_ missed $ reflogDownload adapter
              pause @'Seconds 1
              trace $ "reflogWorker: missed refs for" <+> pretty h <+> pretty missed
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

  let mkRef = fromStringMay . Text.unpack :: (Text -> Maybe (PubKey 'Sign s))

  let defPoll = lastDef 10 [ x
                           | ListVal @C (Key "poll-default" [SymbolVal "reflog", LitIntVal x]) <- syn
                           ]

  let polls = HashMap.fromListWith min $ catMaybes (
       [ (,x) <$> mkRef ref
       | ListVal @C (Key "poll" [SymbolVal "reflog", LitIntVal x, LitStrVal ref]) <- syn
       ]
       <>
       [ (,defPoll) <$> mkRef ref
       | ListVal @C (Key "subscribe" [SymbolVal "reflog", LitStrVal ref]) <- syn
       ] )

  let pollIntervals = HashMap.fromListWith (<>) [ (i, [r]) | (r,i) <- HashMap.toList polls ]
                            & HashMap.toList


  pollers' <- liftIO $ async $ do
                pause @'Seconds 10
                forM pollIntervals  $ \(i,refs) -> liftIO do
                  async $ forever $ do
                    for_ refs $ \r -> do
                      trace $ "POLL REFERENCE" <+> pretty (AsBase58 r) <+> pretty i <> "m"
                      reflogFetch adapter r

                    pause (fromIntegral i :: Timeout 'Minutes)

  w1 <- liftIO $ async $ forever $ do

          -- TODO: reflog-process-period-to-config
          pause @'Seconds 10

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

              trace $ "new reflog value" <+> pretty (AsBase58 r) <+> pretty newRoot

          trace  "I'm a reflog update worker"

  pollers <- liftIO $ wait pollers'
  void $ liftIO $ waitAnyCatchCancel $ w1 : pollers

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

    missedEntries sto h = do
      missed <- liftIO $ newTVarIO mempty
      walkMerkle h (getBlock sto) $ \hr -> do
        case hr of
          Left ha -> do
            atomically $ modifyTVar missed (ha:)
          Right (hs :: [HashRef]) -> do
            w <- mapM ( hasBlock sto . fromHashRef ) hs <&> fmap isJust
            let mi = [ hx | (False,hx) <- zip w hs ]
            for_ mi $ \hx -> liftIO $ atomically $ modifyTVar missed (fromHashRef hx:)

      liftIO $ readTVarIO missed


