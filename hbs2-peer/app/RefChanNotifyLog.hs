{-# LANGUAGE AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module RefChanNotifyLog
  ( refChanNotifyLogWorker
  ) where

import HBS2.Prelude.Plated

import HBS2.Actors.Peer
import HBS2.Hash
import HBS2.Base58
import HBS2.Events
import HBS2.Polling
import HBS2.Data.Detect
import HBS2.Merkle
import HBS2.Storage
import HBS2.Data.Types.Refs
import HBS2.Net.Auth.Credentials
import HBS2.Net.Messaging.Unix
import HBS2.Peer.Proto.Peer
import HBS2.Peer.Proto.RefChan
import HBS2.Net.Proto.Sessions

import HBS2.Peer.RefChanNotifyLog

import PeerTypes hiding (downloads)
import PeerConfig
import Brains

import Data.Time.Clock (NominalDiffTime)
import Data.List qualified as List
import Control.Concurrent.STM (flushTQueue)

import Data.Hashable
import Control.Exception ()
import Control.Monad.Except ()
import Data.Maybe
import Data.Text qualified as Text
import UnliftIO
import Data.Function (on)

import Streaming()


data ToListen e = ToListen
  { listenChan      :: RefChanId e
  , listenRefKey    :: SomeRefKey (String, RefChanId e)
  , listenWriteTime :: NominalDiffTime
  , listenTrim      :: Int
  }

data MyPoll e = MyPoll (RefChanId e) (ToListen e) (TQueue HashRef)

instance ForRefChans e => Eq (MyPoll e) where
  (==) (MyPoll a _ _) (MyPoll b _ _) = a == b

instance ForRefChans e => Hashable (MyPoll e) where
  hashWithSalt salt (MyPoll a _ _) = hashWithSalt salt a

refChanNotifyLogWorker :: forall e s m .
                          ( MonadIO m
                          , MonadUnliftIO m
                          , MyPeer e
                          , HasStorage m
                          , Sessions e (KnownPeer e) m
                          , Signatures s
                          , s ~ Encryption e
                          , IsRefPubKey s
                          , ForRefChans e
                          , EventListener e (RefChanNotify e) m
                          )
             => PeerConfig
             -> SomeBrains e
             -> m ()

refChanNotifyLogWorker conf brains = do

  sto <- getStorage

  let refchans = parseConf conf

  qs <- for refchans $ \l -> do
    let r = listenChan @e l

    q <- newTQueueIO

    subscribe @e (RefChanNotifyEventKey r) $ \(RefChanNotifyEvent h _) -> do
      seen <- isSeen @e brains h
      unless seen do
        trace $ "GOT REFCHAN_NOTIFY TX!" <+> pretty h
        atomically $ writeTQueue q h
        -- FIXME: time-hardcode
        setSeen @e brains h 86400

    pure (l, q)

  polling (Polling 1 1) (toPolling qs) $ \(MyPoll r l q) -> do
    xs <- atomically $ flushTQueue q

    unless (List.null xs) do

      let ref = listenRefKey l

      v <- getRef sto ref <&> fmap HashRef
      hashes <- maybe1 v (pure mempty) (readLog (getBlock sto))

      -- TODO: ACTUALLY-MAKE-IT-NOT-SLOW
      -- TODO: faster-log-merge
      let newHashes = List.nub $ reverse $ take (listenTrim l) (reverse (hashes <> xs))

      let pt = toPTree (MaxSize 512) (MaxNum 512) newHashes

      v1 <- makeMerkle 0 pt $ \(_,_,bss) -> do
              void $ putBlock sto bss

      updateRef sto ref v1

      debug $ "REFCHAN_NOTIFY_LOG:" <+> pretty (AsBase58 r)
                                    <+> pretty (hashObject @HbSync ref)
                                    <+> pretty v1
                                    <+> pretty (length newHashes)

  where

    parseConf (PeerConfig syn)  = rcs
      where rcs = [ ToListen <$> getRefChan rc
                             <*> (makeRefChanNotifyLogKey @e <$> getRefChan rc)
                             <*> getWriteTime args
                             <*> getTrim args

                  | ListVal (  SymbolVal "refchan-notify-log"
                             : LitStrVal rc
                             : args
                           ) <- syn
                  ] & catMaybes
                    & List.nubBy ( (==) `on` listenChan )

    getRefChan rc = fromStringMay @(RefChanId e) (Text.unpack rc)

    getWriteTime syn = Just $
      headDef 1 [ fromIntegral n | ListVal [SymbolVal "write-time", LitIntVal n] <- syn ]

    getTrim syn = Just $
      headDef 10000 [ fromIntegral n | ListVal [SymbolVal "trim", LitIntVal n] <- syn ]

    toPolling qs = pure $ fmap (\(l,q) -> (MyPoll (listenChan l) l q, listenWriteTime l)) qs

