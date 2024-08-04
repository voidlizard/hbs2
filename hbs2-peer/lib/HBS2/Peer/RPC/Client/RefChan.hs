{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.RPC.Client.RefChan where

import HBS2.OrDie
import HBS2.Storage
import HBS2.Merkle
import HBS2.Storage.Operations.ByteString
import HBS2.Data.Types.SignedBox

import HBS2.Peer.Proto.RefChan
import HBS2.Peer.Prelude
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.Internal
import HBS2.Peer.RPC.Client.StorageClient

import Data.ByteString (ByteString)
import Data.Coerce
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Codec.Serialise
import UnliftIO


getRefChanHeadHash :: forall proto m . ( MonadUnliftIO m
                                       , HasClientAPI RefChanAPI proto m
                                       , HasProtocol proto (ServiceProto RefChanAPI proto)
                                       )
                   => PubKey 'Sign 'HBS2Basic
                   -> m (Maybe HashRef)
getRefChanHeadHash puk = do
  api <- getClientAPI @RefChanAPI @proto
  callRpcWaitMay  @RpcRefChanHeadGet (TimeoutSec 1) api puk >>= \case
    Nothing -> throwIO RpcTimeoutError
    Just e  -> pure e


getRefChanHead :: forall proto m . ( MonadUnliftIO m
                                   , HasClientAPI RefChanAPI proto m
                                   , HasClientAPI StorageAPI proto m
                                   , HasProtocol proto (ServiceProto RefChanAPI proto)
                                   , HasProtocol proto (ServiceProto StorageAPI proto)
                                   )
                   => PubKey 'Sign 'HBS2Basic
                   -> m (Maybe (RefChanHeadBlock L4Proto))
getRefChanHead puk = do

  sto <- getClientAPI @StorageAPI @proto <&> AnyStorage . StorageClient

  runMaybeT do
    hx  <- lift (getRefChanHeadHash @proto puk) >>= toMPlus
    lbs <- runExceptT (readFromMerkle sto (SimpleKey (coerce hx)))
              >>= orThrowPassIO

    -- FIXME: error-on-bad-signature
    (_, hdblk) <- unboxSignedBox @(RefChanHeadBlock L4Proto) @'HBS2Basic lbs
                    & toMPlus

    pure hdblk

postRefChanTx :: forall proto s  m . ( MonadUnliftIO m
                                     , HasClientAPI RefChanAPI proto m
                                     , HasClientAPI StorageAPI proto m
                                     , HasProtocol proto (ServiceProto RefChanAPI proto)
                                     , HasProtocol proto (ServiceProto StorageAPI proto)
                                     , ForSignedBox s
                                     , s ~ HBS2Basic
                                     )
                   => PubKey 'Sign s
                   -> SignedBox ByteString s
                   -> m ()
postRefChanTx puk box = do
  api <- getClientAPI @RefChanAPI @proto
  callRpcWaitMay  @RpcRefChanPropose (TimeoutSec 1) api (puk, box) >>= \case
    Nothing -> throwIO RpcTimeoutError
    Just e  -> pure e

fetchRefChanHead :: forall proto m . ( MonadUnliftIO m
                                    , HasClientAPI RefChanAPI proto m
                                    , HasProtocol proto (ServiceProto RefChanAPI proto)
                                    )
                   => PubKey 'Sign 'HBS2Basic
                   -> m ()
fetchRefChanHead puk = do
  api <- getClientAPI @RefChanAPI @proto
  callRpcWaitMay  @RpcRefChanHeadFetch (TimeoutSec 1) api puk >>= \case
    Nothing -> throwIO RpcTimeoutError
    _ -> pure ()

fetchRefChan :: forall proto m . ( MonadUnliftIO m
                                    , HasClientAPI RefChanAPI proto m
                                    , HasProtocol proto (ServiceProto RefChanAPI proto)
                                    )
                   => PubKey 'Sign 'HBS2Basic
                   -> m ()
fetchRefChan puk = do
  api <- getClientAPI @RefChanAPI @proto
  callRpcWaitMay  @RpcRefChanFetch (TimeoutSec 1) api puk >>= \case
    Nothing -> throwIO RpcTimeoutError
    _ -> pure ()


getRefChanValue :: forall proto m . ( MonadUnliftIO m
                                    , HasClientAPI RefChanAPI proto m
                                    , HasProtocol proto (ServiceProto RefChanAPI proto)
                                    )
                   => PubKey 'Sign 'HBS2Basic
                   -> m (Maybe HashRef)
getRefChanValue puk = do
  api <- getClientAPI @RefChanAPI @proto
  callRpcWaitMay  @RpcRefChanGet (TimeoutSec 1) api puk >>= \case
    Nothing -> throwIO RpcTimeoutError
    Just e -> pure e




-- this is not MonadUnliftIO to be compatible with
-- streaming
--

data RScanEnv proto =
  RScanEnv {
    rchanAPI :: ServiceCaller RefChanAPI proto
  }


instance Monad m => HasClientAPI RefChanAPI proto (ReaderT (RScanEnv proto) m) where
  getClientAPI = asks rchanAPI

data RefChanUpdateUnpacked e =
  A (AcceptTran e) | P HashRef (ProposeTran e)
  deriving stock (Generic)

{-# COMPLETE A,P #-}

walkRefChanTx :: forall proto m . ( MonadIO m
                                  , HasClientAPI RefChanAPI proto m
                                  , HasProtocol proto (ServiceProto RefChanAPI proto)
                                  , HasStorage m
                                  )
            => PubKey 'Sign 'HBS2Basic
            -> (RefChanUpdateUnpacked L4Proto -> m ())
            -> m ()
walkRefChanTx puk action = do
  sto <- getStorage
  api <- getClientAPI @RefChanAPI @proto

  let env = RScanEnv api

  flip runContT pure $ callCC $ \exit -> do

    rcv' <- liftIO (runReaderT (getRefChanValue @proto puk) env)

    rcv <- ContT $ maybe1 rcv' none

    walkMerkle (coerce rcv) (getBlock sto) $ \case
      -- FIXME: error-handling
      Left _ -> exit ()

      Right (hs :: [HashRef]) -> do
        for_ hs $ \h -> do
          lbs' <- getBlock sto (coerce h)
          lbs  <- ContT $ maybe1 lbs' none

          let txraw = deserialiseOrFail @(RefChanUpdate L4Proto) lbs
                        & either (const Nothing) Just

          tx <- ContT $ maybe1 txraw none

          case tx of

            Accept _ box  -> do
              (_, txx) <- ContT $ maybe1 (unboxSignedBox0 box) none
              lift $ action (A txx)

            Propose _ box -> do
              (_, txx) <- ContT $ maybe1 (unboxSignedBox0 box) none
              lift $ action (P h txx)

