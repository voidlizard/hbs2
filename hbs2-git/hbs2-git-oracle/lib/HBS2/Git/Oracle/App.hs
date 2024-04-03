{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
module HBS2.Git.Oracle.App
  ( OracleEnv(..)
  , Oracle(..)
  , runWithOracleEnv
  , withOracleEnv
  , withState
  ) where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.State

import HBS2.Peer.CLI.Detect

import HBS2.System.Dir

import DBPipe.SQLite

import System.Directory

myself :: FilePath
myself = "hbs2-git-oracle"

data OracleEnv =
  OracleEnv
  { _refchanId     :: RefChanId L4Proto
  -- , _refchanAuthor :: RefChanAuthor L4Proto
  , _peerAPI       :: ServiceCaller PeerAPI UNIX
  , _reflogAPI     :: ServiceCaller RefLogAPI UNIX
  , _refchanAPI    :: ServiceCaller RefChanAPI UNIX
  , _lwwAPI        :: ServiceCaller LWWRefAPI UNIX
  , _storage       :: AnyStorage
  , _db            :: DBPipeEnv
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

runWithOracleEnv ::  MonadUnliftIO m
                 => RefChanId L4Proto
                 -- -> RefChanAuthor L4Proto
                 -> Oracle m ()
                 -> m ()
runWithOracleEnv rchan m = do

  soname <- detectRPC
               `orDie` "can't locate rpc"

  client <- race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
              >>= orThrowUser ("can't connect to" <+> pretty soname)

  peerAPI    <- makeServiceCaller @PeerAPI (fromString soname)
  reflogAPI  <- makeServiceCaller @RefLogAPI (fromString soname)
  refchanAPI <- makeServiceCaller @RefChanAPI (fromString soname)
  lwwAPI     <- makeServiceCaller @LWWRefAPI (fromString soname)
  storageAPI <- makeServiceCaller @StorageAPI (fromString soname)
  let sto = AnyStorage (StorageClient storageAPI)

  let dbname = show (pretty (AsBase58 rchan))

  dbpath <- liftIO (getXdgDirectory XdgData  myself)

  let dbfile = dbpath </> dbname <> ".db"

  mkdir dbpath

  debug $ red "DBPATH" <+> pretty dbfile

  db <- newDBPipeEnv (dbPipeOptsDef { dbLogger = err . viaShow } ) dbfile

  env <- pure $ OracleEnv rchan
                          peerAPI
                          reflogAPI
                          refchanAPI
                          lwwAPI
                          sto
                          db

  let endpoints = [ Endpoint @UNIX  peerAPI
                  , Endpoint @UNIX  reflogAPI
                  , Endpoint @UNIX  refchanAPI
                  , Endpoint @UNIX  lwwAPI
                  , Endpoint @UNIX  storageAPI
                  ]

  flip runContT pure do

    void $ ContT $ withAsync $ runMessagingUnix client

    void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

    lift $ withOracleEnv env (withState evolveDB >> m)

withOracleEnv :: MonadUnliftIO m => OracleEnv -> Oracle m a -> m a
withOracleEnv env action = do
 runReaderT (fromOracle action) env

class Monad m => HasDB m where
  getDB :: m DBPipeEnv

instance Monad m => HasDB (Oracle m) where
  getDB = asks _db


withState :: forall m a . (MonadUnliftIO m, HasDB m)
          => DBPipeM m a
          -> m a
withState dbAction = do
  db <- getDB
  withDB db dbAction

