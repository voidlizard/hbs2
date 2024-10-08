{-# LANGUAGE StrictData #-}

module RefChanQBLF.RPCServer where

import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Data.Types.SignedBox
import HBS2.Hash
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.QBLF
import HBS2.Net.Proto.Service
import HBS2.System.Logger.Simple

import Codec.Serialise
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Data.Function
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Prettyprinter
import UnliftIO

import RefChanQBLF.Common
import RefChanQBLF.Impl
import RefChanQBLF.Transactions

data PingRPC
data PostTxRPC

type QBLFAppRPC =
    '[ PingRPC
     , PostTxRPC
     ]

instance HasProtocol UNIX (ServiceProto QBLFAppRPC UNIX) where
    type ProtocolId (ServiceProto QBLFAppRPC UNIX) = 0x0B1F0B1F
    type Encoded UNIX = ByteString
    decode = either (const Nothing) Just . deserialiseOrFail
    encode = serialise

type instance Input PingRPC = ()
type instance Output PingRPC = Text

type instance Input PostTxRPC = QBLFDemoToken 'HBS2Basic
type instance Output PostTxRPC = Either RPCServerError (Either MyError Text)

data QRPCEnv = QRPCEnv
    { qrpcenvQConsensus :: QBLF ConsensusQBLF
    , qrpcenvRefchanId :: PubKey 'Sign 'HBS2Basic
    , qrpcenvFabriq :: Fabriq UNIX
    , qrpcenvOwnPeer :: Peer UNIX
    }

newtype QRPCAppT m a = QRPCAppT {fromQRPCAppT :: ReaderT QRPCEnv m a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadUnliftIO
        , MonadReader QRPCEnv
        , MonadTrans
        )

instance (Monad m) => HasFabriq UNIX (QRPCAppT m) where
    getFabriq = asks qrpcenvFabriq

instance (Monad m) => HasOwnPeer UNIX (QRPCAppT m) where
    ownPeer = asks qrpcenvOwnPeer

instance (Monad m) => HasQBLFEnv (ResponseM UNIX (QRPCAppT m)) where
    getQBLFEnv = lift ask

runQRPCT
    :: (MonadIO m, PeerMessaging UNIX)
    => QRPCEnv
    -> QRPCAppT m a
    -> m a
runQRPCT env m = runReaderT (fromQRPCAppT m) env

class HasQBLFEnv m where
    getQBLFEnv :: m QRPCEnv

data RPCServerError = RPCServerError Text
    deriving (Generic, Show)
instance Serialise RPCServerError

wrapErrors :: (MonadUnliftIO m) => m a -> m (Either RPCServerError a)
wrapErrors =
    UnliftIO.tryAny >=> flip either (pure . Right) \e -> do
        debug $ "RPC ServerError" <+> viaShow e
        pure $ (Left . RPCServerError . T.pack . show) e

instance (MonadIO m, HasQBLFEnv m) => HandleMethod m PingRPC where
    handleMethod _ = do
        debug $ "RPC PING"
        pure "pong"

instance
    ( MonadUnliftIO m
    , HasQBLFEnv m
    )
    => HandleMethod m PostTxRPC
    where
    handleMethod tok = wrapErrors $ UnliftIO.try do
        let txhash = (hashObject @HbSync . serialise) tok
            ptok = pretty tok

        debug $ "RPC got post tx" <+> pretty txhash <+> ptok

        refchanId <- qrpcenvRefchanId <$> getQBLFEnv
        validateQBLFToken refchanId tok
            & either throwIO pure

        qblf <- qrpcenvQConsensus <$> getQBLFEnv
        qblfEnqueue qblf tok

        debug $ "TX ENQUEUED OK" <+> ptok
        pure $ "Enqueued: " <> (cs . show) ptok

validateQBLFToken
    :: (MonadError MyError m)
    => PubKey 'Sign 'HBS2Basic
    -> QBLFDemoToken 'HBS2Basic
    -> m ()
validateQBLFToken chan = \case
    Emit box -> do
        (signer, _tx) <- orE SignatureError $ unboxSignedBox0 box
        unless (signer == chan) do
            throwError
                ( SignerDoesNotMatchRefchan
                    ((cs . show . pretty . AsBase58) signer)
                    ((cs . show . pretty . AsBase58) chan)
                )
    Move box -> do
        (_sign, _tx) <- orE SignatureError $ unboxSignedBox0 box
        pure ()
