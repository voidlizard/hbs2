{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.CLI.Run.Internal
  ( module HBS2.CLI.Run.Internal
  , module SC
  ) where

import HBS2.CLI.Prelude

import HBS2.OrDie
import HBS2.Base58
import HBS2.Data.Types.Refs
import HBS2.Storage
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.Client.StorageClient

import Data.Config.Suckless.Script qualified as SC
import Data.Config.Suckless.Script hiding (internalEntries)

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as Text
import Lens.Micro.Platform


data HBS2CliEnv =
  HBS2CliEnv
  { _peerSocket      :: FilePath
  , _peerRefChanAPI  :: ServiceCaller RefChanAPI UNIX
  , _peerRefLogAPI   :: ServiceCaller RefLogAPI UNIX
  , _peerLwwRefAPI   :: ServiceCaller LWWRefAPI UNIX
  , _peerPeerAPI     :: ServiceCaller PeerAPI UNIX
  , _peerStorageAPI  :: ServiceCaller StorageAPI UNIX
  }

makeLenses 'HBS2CliEnv

newtype HBS2Cli m a = HBS2Cli { fromHBS2Cli :: ReaderT (TVar (Maybe HBS2CliEnv)) m a }
                      deriving newtype ( Applicative
                                       , Functor
                                       , Monad
                                       , MonadIO
                                       , MonadUnliftIO
                                       , MonadReader (TVar (Maybe HBS2CliEnv))
                                       )

withHBS2Cli :: TVar (Maybe HBS2CliEnv) -> HBS2Cli m a -> m a
withHBS2Cli env action = runReaderT (fromHBS2Cli action) env

recover :: HBS2Cli IO a -> HBS2Cli IO a
recover what = do
  catch what $ \case
    PeerNotConnectedException -> do

      soname <- detectRPC
                  `orDie` "can't locate hbs2-peer rpc"

      flip runContT pure do

        client <- lift $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
                    >>= orThrowUser ("can't connect to" <+> pretty soname)

        void $ ContT $ withAsync $ runMessagingUnix client

        peerAPI    <- makeServiceCaller @PeerAPI (fromString soname)
        refLogAPI  <- makeServiceCaller @RefLogAPI (fromString soname)
        refChanAPI <- makeServiceCaller @RefChanAPI (fromString soname)
        storageAPI <- makeServiceCaller @StorageAPI (fromString soname)
        lwwAPI     <- makeServiceCaller @LWWRefAPI (fromString soname)

        -- let sto = AnyStorage (StorageClient storageAPI)

        let endpoints = [ Endpoint @UNIX  peerAPI
                        , Endpoint @UNIX  refLogAPI
                        , Endpoint @UNIX  refChanAPI
                        , Endpoint @UNIX  lwwAPI
                        , Endpoint @UNIX  storageAPI
                        ]

        void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

        let env = Just (HBS2CliEnv soname refChanAPI refLogAPI lwwAPI peerAPI storageAPI)
        tv <- newTVarIO env

        liftIO $ withHBS2Cli tv what


runHBS2Cli :: MonadUnliftIO m => HBS2Cli m a -> m a
runHBS2Cli action = do
   noenv <- newTVarIO Nothing
   withHBS2Cli  noenv action

data PeerException =
  PeerNotConnectedException
  deriving stock (Show, Typeable)

instance Exception PeerException


instance (MonadUnliftIO m, HasClientAPI api proto m) => HasClientAPI api proto (RunM c m) where
  getClientAPI = lift (getClientAPI @api @proto)

instance (MonadUnliftIO m, HasStorage m) => HasStorage  (RunM c m) where
  getStorage = lift getStorage

instance (MonadUnliftIO m, HasClientAPI StorageAPI UNIX m, HasStorage m) => HasStorage (ContT a (RunM c m)) where
  getStorage = lift getStorage

instance (MonadUnliftIO m, HasClientAPI api proto m) => HasClientAPI api proto (ContT a (RunM c m)) where
  getClientAPI = lift $ getClientAPI @api @proto

instance MonadUnliftIO m => HasClientAPI RefChanAPI UNIX (HBS2Cli m) where
  getClientAPI = do
    what <- ask >>= readTVarIO >>= orThrow PeerNotConnectedException
    pure $ view peerRefChanAPI what

instance MonadUnliftIO m => HasClientAPI RefLogAPI UNIX (HBS2Cli m) where
  getClientAPI = do
    what <- ask >>= readTVarIO >>= orThrow PeerNotConnectedException
    pure $ view peerRefLogAPI what

instance MonadUnliftIO m => HasClientAPI PeerAPI UNIX (HBS2Cli m) where
  getClientAPI = do
    what <- ask >>= readTVarIO >>= orThrow PeerNotConnectedException
    pure $ view peerPeerAPI what

instance MonadUnliftIO m => HasClientAPI StorageAPI UNIX (HBS2Cli m) where
  getClientAPI = do
    what <- ask >>= readTVarIO >>= orThrow PeerNotConnectedException
    pure $ view peerStorageAPI what

instance MonadUnliftIO m => HasClientAPI LWWRefAPI UNIX (HBS2Cli m) where
  getClientAPI = do
    what <- ask >>= readTVarIO >>= orThrow PeerNotConnectedException
    pure $ view peerLwwRefAPI what

instance MonadUnliftIO m => HasStorage (HBS2Cli m) where
  getStorage = getClientAPI @StorageAPI @UNIX  <&> AnyStorage . StorageClient

internalEntries :: forall c m . (IsContext c, Exception (BadFormException c), MonadUnliftIO m) => MakeDictM c m ()
internalEntries = do
    SC.internalEntries

    entry $ bindMatch "blob:base58" $ \case
      [LitStrVal t] -> do
        bs <- pure (Text.unpack t & BS8.pack & fromBase58)
               `orDie` "invalid base58"
              <&> BS8.unpack

        pure (mkForm "blob" [mkStr @c bs])

      _ -> throwIO (BadFormException @c nil)


    let decodeB58 t = do
          pure (Text.unpack t & BS8.pack & fromBase58)
            `orDie` "invalid base58"

    let decodeAndOut t = do
          liftIO $ BS8.putStr =<< decodeB58 t

    entry $ bindMatch "base58:encode" $ \case
      [LitStrVal t] -> do
        let s = Text.unpack t & BS8.pack & toBase58 & BS8.unpack
        pure (mkForm "blob:base58" [mkStr @c s])

      [ListVal [SymbolVal "blob", LitStrVal t]] -> do
        let s = Text.unpack t & BS8.pack & toBase58 & BS8.unpack
        pure (mkForm "blob:base58" [mkStr @c s])

      e -> throwIO (BadFormException @c nil)

    entry $ bindMatch "base58:decode" $ \case

      [ListVal [SymbolVal "blob:base58", LitStrVal t]] -> do
        s <- decodeB58 t <&> BS8.unpack
        pure $ mkForm "blob" [mkStr @c s]

      e -> throwIO (BadFormException @c nil)

    entry $ bindMatch "base58:put" $ nil_ $ \case
      [ListVal [SymbolVal "blob:base58", LitStrVal t]] ->
        decodeAndOut t

      [LitStrVal t] -> decodeAndOut t

      e -> throwIO (BadFormException @c nil)

    entry $ bindMatch "test:opaque" $ \case
      [ LitIntVal n ] -> mkOpaque n
      [ StringLike s ] -> mkOpaque s

      _ -> mkOpaque ()

