{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.RPC.Client.RefLog where

import HBS2.OrDie
import HBS2.Storage
import HBS2.Merkle
import HBS2.Storage.Operations.ByteString
import HBS2.Data.Types.SignedBox

import HBS2.Peer.Proto.RefLog
import HBS2.Peer.Prelude
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.Internal
import HBS2.Peer.RPC.Client.StorageClient

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString (ByteString)
import Data.Coerce
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.Identity
import Codec.Serialise
import UnliftIO


data RefLogClientException =
  RefLogClientRpcTimeout
  deriving (Typeable, Show)

instance Exception RefLogClientException

getRefLogValue  :: forall proto m . ( MonadUnliftIO m
                                    , HasClientAPI RefLogAPI proto m
                                    , HasProtocol proto (ServiceProto RefLogAPI proto)
                                    , HasStorage m
                                    )
                => PubKey Sign HBS2Basic
                -> m (Maybe HashRef)

getRefLogValue pk = do
  api <- getClientAPI @RefLogAPI @proto
  callRpcWaitMay @RpcRefLogGet (TimeoutSec 1) api pk
    >>= orThrow RefLogClientRpcTimeout

walkRefLogTx :: forall proto m .  ( MonadUnliftIO m
                                  , HasClientAPI RefLogAPI proto m
                                  , HasProtocol proto (ServiceProto RefLogAPI proto)
                                  , HasStorage m
                                  , Signatures (Encryption L4Proto)
                                  , IsRefPubKey (Encryption L4Proto)
                                  , Serialise (Nonce (RefLogUpdate L4Proto))
                                  , Serialise (Signature (Encryption L4Proto))
                                  )
            => (HashRef -> m Bool)
            -> PubKey 'Sign 'HBS2Basic
            -> (HashRef -> ByteString -> m ())
            -> m ()
walkRefLogTx filt puk action = do
  sto <- getStorage

  flip runContT pure $ callCC $ \exit -> do

    rcv' <- lift $ getRefLogValue @proto puk

    rcv <- ContT $ maybe1 rcv' none

    walkMerkle (coerce rcv) (getBlock sto) $ \case
      -- FIXME: error-handling
      Left _ -> exit ()

      Right (hs :: [HashRef]) -> do
        for_ hs $ \h -> do
          -- want <- lift (filt h)
          -- when want do
          lbs' <- getBlock sto (coerce h)
          lbs  <- ContT $ maybe1 lbs' none
          RefLogUpdate{..} <- ContT $ maybe1 (unpackRU h lbs) none
          lift $ action h _refLogUpdData

  where
    unpackRU h lbs = do
      deserialiseOrFail @(RefLogUpdate L4Proto) lbs
       & either (const Nothing) Just



