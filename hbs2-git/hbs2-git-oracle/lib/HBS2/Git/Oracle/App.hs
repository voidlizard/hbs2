module HBS2.Git.Oracle.App where

import HBS2.Git.Oracle.Prelude

import HBS2.Peer.CLI.Detect

data OracleEnv =
  OracleEnv
  { _peerAPI   :: ServiceCaller PeerAPI UNIX
  , _reflogAPI :: ServiceCaller RefLogAPI UNIX
  , _lwwAPI    :: ServiceCaller LWWRefAPI UNIX
  }
  deriving stock (Generic)

newtype Oracle m a =
  Oracle { fromOracle :: ReaderT OracleEnv m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadTrans
                   , MonadReader OracleEnv
                   , MonadIO
                   , MonadUnliftIO
                   )

newOracleEnv :: MonadUnliftIO m => m OracleEnv
newOracleEnv = do

  soname <- detectRPC
               `orDie` "can't locate rpc"

  flip runContT pure do

    client <- lift $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
                >>= orThrowUser ("can't connect to" <+> pretty soname)

    void $ ContT $ withAsync $ runMessagingUnix client

    peerAPI    <- makeServiceCaller @PeerAPI (fromString soname)
    reflogAPI  <- makeServiceCaller @RefLogAPI (fromString soname)
    storageAPI <- makeServiceCaller @StorageAPI (fromString soname)
    lwwAPI     <- makeServiceCaller @LWWRefAPI (fromString soname)

    let sto = AnyStorage (StorageClient storageAPI)

    let endpoints = [ Endpoint @UNIX  peerAPI
                    , Endpoint @UNIX  reflogAPI
                    , Endpoint @UNIX  lwwAPI
                    , Endpoint @UNIX  storageAPI
                    ]

    void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

    pure $ OracleEnv  peerAPI
                      reflogAPI
                      lwwAPI

withOracleEnv ::  MonadIO m => OracleEnv -> Oracle m a -> m a
withOracleEnv env m = runReaderT (fromOracle m) env



