{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
module HBS2.Git.Oracle.App
  ( OracleEnv(..)
  , Oracle(..)
  , runWithOracleEnv
  ) where

import HBS2.Git.Oracle.Prelude

import HBS2.Peer.CLI.Detect

import GHC.TypeLits
import Codec.Serialise

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

runWithOracleEnv ::  MonadUnliftIO m => Oracle m () -> m ()
runWithOracleEnv m = do

  soname <- detectRPC
               `orDie` "can't locate rpc"

  client <- race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
              >>= orThrowUser ("can't connect to" <+> pretty soname)

  peerAPI    <- makeServiceCaller @PeerAPI (fromString soname)
  reflogAPI  <- makeServiceCaller @RefLogAPI (fromString soname)
  lwwAPI     <- makeServiceCaller @LWWRefAPI (fromString soname)
  storageAPI <- makeServiceCaller @StorageAPI (fromString soname)

  env <- pure $ OracleEnv  peerAPI
                           reflogAPI
                           lwwAPI

  let endpoints = [ Endpoint @UNIX  peerAPI
                  , Endpoint @UNIX  reflogAPI
                  , Endpoint @UNIX  lwwAPI
                  , Endpoint @UNIX  storageAPI
                  ]

  flip runContT pure do

    void $ ContT $ withAsync $ runMessagingUnix client

    void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

    lift $ runReaderT (fromOracle m) env

