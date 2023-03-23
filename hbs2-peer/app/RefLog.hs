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
import Data.List qualified as List
import Data.Text qualified as Text
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Codec.Serialise
import Data.HashSet qualified as HashSet
import Control.Concurrent.Async
import Control.Monad.Trans.Maybe
import Lens.Micro.Platform

doRefLogUpdate :: forall e m . ( MonadIO m
                               , Pretty (AsBase58 (PubKey 'Sign e))
                               )
               => (PubKey 'Sign e, RefLogUpdate e) -> m ()

doRefLogUpdate (reflog, _) = do
  trace $ "doRefLogUpdate" <+> pretty (AsBase58 reflog)
  pure ()

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


mkRefLogRequestAdapter :: forall e m . ( MonadIO m
                                       , HasPeerLocator e m
                                       , MyPeer e
                                       , HasStorage m
                                       , Pretty (AsBase58 (PubKey 'Sign e))
                                       )
          => m (RefLogRequestI e (ResponseM e m ))
mkRefLogRequestAdapter = do
  sto <- getStorage
  pure $ RefLogRequestI (doOnRefLogRequest sto) dontHandle


doOnRefLogRequest :: forall e m . ( MonadIO m
                                  , MyPeer e
                                  )
                   =>  AnyStorage -> (Peer e, PubKey 'Sign e) -> m (Maybe (Hash HbSync))

doOnRefLogRequest sto (_,pk) = do
  r <- liftIO $ getRef sto (RefLogKey pk)
  trace $ "doOnRefLogRequest" <+> pretty (AsBase58 pk) <+> pretty r
  pure r


mkAdapter :: forall e m . ( MonadIO m
                          , HasPeerLocator e m
                          , Sessions e (KnownPeer e) m
                          , Request e (RefLogUpdate e) m
                          , MyPeer e
                          , Pretty (AsBase58 (PubKey 'Sign e))
                          )
          => m (RefLogUpdateI e (ResponseM e m ))

mkAdapter = do
  let bcast = lift . doRefLogBroadCast @e
  let upd   = lift . doRefLogUpdate @e
  pure $ RefLogUpdateI upd bcast


data RefLogWorkerAdapter e =
 RefLogWorkerAdapter
 { reflogDownload :: Hash HbSync -> IO ()
 , reflogFetch    :: PubKey 'Sign e -> IO ()
 }

reflogWorker :: forall e m . ( MonadIO m, MyPeer e
                             , EventListener e (RefLogUpdateEv e) m
                             , EventListener e (RefLogRequestAnswer e) m
                             -- , Request e (RefLogRequest e) (Peerm
                             , HasStorage m
                             , Nonce (RefLogUpdate e) ~ BS.ByteString
                             , Signatures e
                             , Serialise (RefLogUpdate e)
                             , EventEmitter e (RefLogUpdateEv e) m -- (PeerM e m)
                             )
             => PeerConfig
             -> RefLogWorkerAdapter e
             -> m ()

reflogWorker conf adapter = do

  sto <- getStorage

  q <- liftIO newTQueueIO

  let reflogUpdate reflog ha tran = do
        signed <- verifyRefLogUpdate tran
        when signed do
          -- trace $ "GOT PRETTY VALID REFLOG UPDATE TRANSACTION" <+> pretty ha

          liftIO $ atomically $ writeTQueue q (reflog, [tran])

          -- FIXME: fix-this-copypaste
          let bss = view refLogUpdData tran
          let what = tryDetect (hashObject bss) (LBS.fromStrict bss)
          case what of
            SeqRef (SequentialRef _ (AnnotatedHashRef _ ref)) -> do
              liftIO $ reflogDownload adapter (fromHashRef ref)

            AnnRef ref -> do
              liftIO $ reflogDownload adapter ref

            _ -> pure ()

  subscribe @e RefLogUpdateEvKey $ \(RefLogUpdateEvData (reflog,v)) -> do
    trace $ "reflog worker.got refupdate" <+> pretty (AsBase58 reflog)
    liftIO $ reflogUpdate reflog Nothing v
    liftIO $ atomically $ writeTQueue q (reflog, [v])

  subscribe @e RefLogReqAnswerKey $ \(RefLogReqAnswerData reflog h) -> do
    trace $ "reflog worker. GOT REFLOG ANSWER" <+> pretty (AsBase58 reflog) <+> pretty h
    -- TODO: ASAP-only-process-link-if-we-subscribed
    -- TODO: ASAP-start-only-one-instance-for-link-monitor
    -- TODO: periodically-check-if-reflog-is-done
    -- TODO: ASAP-when-done-delete-monitor
    -- TODO: ASAP-dont-do-if-already-done
    void $ liftIO $ race (pause @'Seconds 3600) do
    -- FIXME: log-this-situation
    -- FIXME: fix-time-hardcode-again
      reflogDownload adapter h
      fix \next -> do
        missed <- missedEntries sto h
        if missed /= 0 then do
          pause @'Seconds 1
          trace $ "reflogWorker: missed refs for" <+> pretty h <+> pretty missed
          next
        else do
          trace $ "block" <+> pretty h <+> "is downloaded"
          hashes <- readHashesFromBlock sto (Just h)
          for_ hashes $ \ha -> runMaybeT do
            bss <- liftIO $ getBlock sto (fromHashRef ha)

            when (isNothing bss) do
              liftIO $ reflogDownload adapter (fromHashRef ha)

            bs <- MaybeT $ pure bss

            tran <- MaybeT $ pure $ deserialiseOrFail @(RefLogUpdate e) bs & either (const Nothing) Just
            liftIO $ reflogUpdate reflog (Just ha) tran

  let (PeerConfig syn) = conf

  let mkRef = fromStringMay . Text.unpack :: (Text -> Maybe (PubKey 'Sign e))

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
          el0 <- liftIO $ atomically $ readTQueue q
          els <- liftIO $ atomically $ flushTQueue q

          let byRef = HashMap.fromListWith (<>) (el0 : els)

          for_ (HashMap.toList byRef) $ \(r,x) -> do
            let reflogkey = RefLogKey r
            -- trace $ "UPDATE REFLOG" <+> pretty (hashObject @HbSync reflogkey) <+> pretty (fmap AsBase58 x)
            h' <- liftIO $! getRef sto (RefLogKey r)
            -- trace $ "UPDATE REGLOG OKAY" <+> pretty (isJust h')

            hashes <- liftIO $ readHashesFromBlock sto h'

            -- save new transaction, must be idempotent
            newHashes <- liftIO $ mapM (putBlock sto . serialise) x <&> catMaybes <&> fmap HashRef

            -- TODO: needs-very-fast-sort-and-dedupe
            let hashesNew = HashSet.fromList (hashes <> newHashes) & HashSet.toList

            -- FIXME: remove-chunk-num-hardcode
            let pt = toPTree (MaxSize 256) (MaxNum 256) hashesNew

            newRoot <- liftIO do
              nref <- makeMerkle 0 pt $ \(_,_,bss) -> do
                void $ putBlock sto bss

              updateRef sto reflogkey nref
              pure nref

            -- TODO: old-root-to-delete

            trace $ "new reflog value" <+> pretty (AsBase58 r) <+> pretty newRoot

          -- TODO: read-reflog-value
          -- TODO: read-reflog-hashes
          -- TODO: store-all-values
          -- TODO: get all hashes

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
      missed <- liftIO $ newTVarIO 0
      walkMerkle h (getBlock sto) $ \hr -> do
        case hr of
          Left{} -> atomically $ modifyTVar missed succ
          Right (_ :: [HashRef]) -> pure ()
      liftIO $ readTVarIO missed


