{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import HBS2.Git.Client.Prelude hiding (info)
import HBS2.Git.Client.App hiding (_progress, _storage, _peerAPI, _lwwAPI, _refLogAPI)
import HBS2.Git.Client.Progress
import HBS2.Git.Client.Import
import HBS2.Git.Client.RefLog
import HBS2.Peer.CLI.Detect

import Options.Applicative
import Data.Semigroup ((<>))

main :: IO ()
main = do
  let parser = subscribe
                <$> optional (strOption
                    ( long "socket"
                   <> short 's'
                   <> metavar "SOCKET"
                   <> help "Socket file path"))
                <*> argument pLww (metavar "LWWREF")
  join $ execParser (info (parser <**> helper)
              ( fullDesc
             <> progDesc "Parse command line arguments"
             <> header "Command line arguments parsing example"))


  where
    pLww :: ReadM (LWWRefKey HBS2Basic)
    pLww = maybeReader fromStringMay


data MyStuff =
  MyStuff
  { _peerAPI   :: ServiceCaller PeerAPI UNIX
  , _lwwAPI    :: ServiceCaller LWWRefAPI UNIX
  , _refLogAPI :: ServiceCaller RefLogAPI UNIX
  , _storage   :: AnyStorage
  , _progress  :: AnyProgress
  }

newtype MyApp m a = MyApp {  fromMyApp :: ReaderT MyStuff m a }
                    deriving newtype ( Functor
                                     , Applicative
                                     , Monad
                                     , MonadIO
                                     , MonadUnliftIO
                                     , MonadThrow
                                     , MonadReader MyStuff
                                     )

instance Monad m => HasProgressIndicator  (MyApp m) where
  getProgressIndicator = asks _progress

instance Monad m => HasStorage (MyApp m) where
  getStorage = asks _storage

instance Monad m => HasAPI PeerAPI UNIX (MyApp m) where
  getAPI = asks _peerAPI

instance Monad m => HasAPI LWWRefAPI UNIX (MyApp m) where
  getAPI = asks _lwwAPI

instance Monad m => HasAPI RefLogAPI UNIX (MyApp m) where
  getAPI = asks _refLogAPI

subscribe :: forall m . MonadUnliftIO m => Maybe String -> LWWRefKey HBS2Basic  -> m ()
subscribe soname' ref = do

  soname <- maybe1 soname' detectRPC (pure.Just) `orDie` "can't locate rpc"

  flip runContT pure do

    client <- lift $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
                >>= orThrowUser ("can't connect to" <+> pretty soname)

    q <- lift newProgressQ
    let ip = AnyProgress q

    void $ ContT $ withAsync $ runMessagingUnix client
    void $ ContT $ withAsync $ drawProgress q

    peerAPI    <- makeServiceCaller @PeerAPI (fromString soname)
    refLogAPI  <- makeServiceCaller @RefLogAPI (fromString soname)
    storageAPI <- makeServiceCaller @StorageAPI (fromString soname)
    lwwAPI     <- makeServiceCaller @LWWRefAPI (fromString soname)

    let sto = AnyStorage (StorageClient storageAPI)

    let endpoints = [ Endpoint @UNIX  peerAPI
                    , Endpoint @UNIX  refLogAPI
                    , Endpoint @UNIX  lwwAPI
                    , Endpoint @UNIX  storageAPI
                    ]

    void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

    let app = MyStuff peerAPI lwwAPI refLogAPI sto ip

    lift $ flip runReaderT app  $ fromMyApp do
      merelySubscribeRepo ref

      onProgress ip ImportAllDone

      hFlush stdout
      hFlush stderr

  pure ()

