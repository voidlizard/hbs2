{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language RankNTypes #-}
{-# Language AllowAmbiguousTypes #-}
module Main where

import HBS2.Actors
import HBS2.Actors.ChunkWriter
import HBS2.Clock
import HBS2.Defaults
import HBS2.Hash
import HBS2.Net.Messaging
import HBS2.Net.Messaging.Fake
import HBS2.Net.Proto
import HBS2.Net.Proto.Sessions
import HBS2.Net.Proto.BlockChunks
import HBS2.Net.Proto.BlockInfo
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Storage.Simple.Extra

import Test.Tasty.HUnit

import Codec.Serialise hiding (encode,decode)
import Control.Concurrent.Async
import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as B8
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Default
import Data.Dynamic
import Data.Foldable hiding (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Word
import GHC.TypeLits
import Lens.Micro.Platform
import Prettyprinter hiding (pipe)
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.IO

debug :: (MonadIO m) => Doc ann -> m ()
debug p = liftIO $ hPrint stderr p

data Fake

data BlockDownload =
  BlockDownload
  { _sBlockHash      :: Hash HbSync
  , _sBlockSize      :: Size
  , _sBlockChunkSize :: ChunkSize
  , _sBlockOffset    :: Offset
  , _sBlockWritten   :: Size
  }

makeLenses 'BlockDownload


instance HasPeer Fake where
  newtype instance Peer Fake = FakePeer Word8
                               deriving newtype (Hashable,Num,Enum,Real,Integral)
                               deriving stock (Eq,Ord,Show)


instance Pretty (Peer Fake) where
  pretty (FakePeer n) = parens ("peer" <+> pretty n)


instance HasProtocol Fake (BlockSize Fake) where
  type instance ProtocolId (BlockSize Fake) = 1
  type instance Encoded Fake = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

instance HasProtocol Fake (BlockChunks Fake) where
  type instance ProtocolId (BlockChunks Fake) = 2
  type instance Encoded Fake = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise


type instance SessionData e (BlockSize e) = BlockSizeSession e
type instance SessionData Fake (BlockChunks Fake) = BlockDownload

newtype instance SessionKey Fake (BlockChunks Fake) =
  DownloadSessionKey (Peer Fake, Cookie Fake)
  deriving newtype (Eq, Hashable)
  deriving stock (Generic)

newtype BlockSizeSession e =
  BlockSizeSession
  { _bsBlockSizes :: Map (Peer e) Size
  }

makeLenses 'BlockSizeSession

instance Ord (Peer e) => Default (BlockSizeSession e) where
  def = BlockSizeSession mempty

deriving stock instance Show (BlockSizeSession Fake)


class Monad m => HasOwnPeer e m where
  ownPeer :: m (Peer e)


data AnyStorage = forall zu . Storage zu HbSync ByteString IO => AnyStorage zu

instance (IsKey HbSync, Key HbSync ~ Hash HbSync) => Storage AnyStorage HbSync ByteString IO where

  putBlock (AnyStorage s) = putBlock s
  enqueueBlock (AnyStorage s) = enqueueBlock s
  getBlock (AnyStorage s) = getBlock s
  getChunk (AnyStorage s) = getChunk s
  hasBlock (AnyStorage s) = hasBlock s

class HasStorage m where
  getStorage :: m AnyStorage

data Fabriq e = forall bus . (Serialise (Encoded e), Messaging bus e ByteString) => Fabriq bus

class HasFabriq e m where
  getFabriq :: m (Fabriq e)

instance HasPeer e => Messaging (Fabriq e) e ByteString where
  sendTo (Fabriq bus) = sendTo bus
  receive (Fabriq bus) = receive bus

data AnyMessage e = AnyMessage Integer (Encoded e)
                    deriving stock (Generic)

instance Serialise (Encoded e) => Serialise (AnyMessage e)


data AnyProtocol e m = forall p  . ( HasProtocol e p
                                   , Response e p m
                                   ) =>
  AnyProtocol
  { myProtoId   :: Integer
  , protoDecode :: Encoded e -> Maybe p
  , protoEncode :: p -> Encoded e
  , handle      :: p -> m ()
  }

makeResponse :: forall e p  m . ( MonadIO m
                                , Response e p m
                                , HasProtocol e p
                                )
            => (p ->  m ()) -> AnyProtocol e m

makeResponse h = AnyProtocol { myProtoId  = natVal (Proxy @(ProtocolId p))
                             , protoDecode = decode
                             , protoEncode = encode
                             , handle = h
                             }

data PeerEnv e =
  PeerEnv
  { _envSelf     :: Peer e
  , _envFab      :: Fabriq e
  , _envStorage  :: AnyStorage
  , _envDeferred :: Pipeline IO ()
  , _envSessions :: Cache SKey Dynamic
  }

newtype PeerM e m a = PeerM { fromPeerM :: ReaderT (PeerEnv e) m a }
                      deriving newtype ( Functor
                                       , Applicative
                                       , Monad
                                       , MonadReader (PeerEnv e)
                                       , MonadIO
                                       )


newtype  ResponseM e m a = ResponseM { fromResponse :: ReaderT (ResponseEnv e) m a }
                           deriving newtype ( Functor
                                            , Applicative
                                            , Monad
                                            , MonadReader (ResponseEnv e)
                                            , MonadIO
                                            , MonadTrans
                                            )

newtype ResponseEnv e =
  ResponseEnv
  {  _answTo :: Peer e
  }

makeLenses 'PeerEnv

makeLenses 'ResponseEnv


runResponseM :: forall e m . (Monad m)
             => Peer e
             -> ResponseM e m ()
             -> m ()

runResponseM peer f = runReaderT (fromResponse f) (ResponseEnv peer)

instance Monad m => HasOwnPeer e (PeerM e m) where
  ownPeer = asks (view envSelf)

instance Monad m => HasFabriq e (PeerM e m) where
  getFabriq = asks (view envFab)

instance Monad m => HasStorage (PeerM e m) where
  getStorage = asks (view envStorage)


instance ( MonadIO m
         , HasProtocol e p
         , Eq (SessionKey e p)
         , Typeable (SessionKey e p)
         , Typeable (SessionData e p)
         , Hashable (SessionKey e p)
         ) => Sessions e p (PeerM e m) where


  find k f = do
    se <- asks (view envSessions)
    let sk = newSKey @(SessionKey e p) k
    r <- liftIO $ Cache.lookup se sk
    case fromDynamic @(SessionData e p) <$> r of
      Just v  -> pure $ f <$> v
      Nothing -> pure Nothing

  fetch upd de k fn  = do
    se <- asks (view envSessions)
    let sk = newSKey @(SessionKey e p) k
    let ddef = toDyn de

    r <- liftIO $ Cache.lookup se sk

    case r of
     Just v  -> pure $ fn $ fromMaybe de (fromDynamic @(SessionData e p) v )
     Nothing -> do
       when upd $ liftIO $ Cache.insert se sk ddef
       pure (fn de)

  update de k f = do
    se <- asks (view envSessions)
    val <- fetch @e @p True de k id
    liftIO $ Cache.insert se (newSKey @(SessionKey e p) k) (toDyn (f val))

  expire k = do
    se <- asks (view envSessions)
    liftIO $ Cache.delete se (newSKey @(SessionKey e p) k)


instance ( MonadIO m
         , HasProtocol e p
         , HasFabriq e (PeerM e m)
         , Serialise (Encoded e)
         ) => Request e p (PeerM e m) where
  request p msg = do
    let proto = protoId @e @p (Proxy @p)
    pipe <- getFabriq @e
    me <- ownPeer @e
    let bs = serialise (AnyMessage @e proto (encode msg))
    sendTo pipe (To p) (From me) bs

runPeerM :: MonadIO m => AnyStorage -> Fabriq e -> Peer e  -> PeerM e m a -> m ()
runPeerM s bus p f  = do

  env <- PeerEnv p bus s <$> newPipeline defProtoPipelineSize
                         <*> liftIO (Cache.newCache (Just defCookieTimeout))

  let de = view envDeferred env
  as <- liftIO $ async $ runPipeline de
  void $ runReaderT (fromPeerM f) env
  void $ liftIO $ stopPipeline de
  liftIO $ cancel as

withPeerM :: MonadIO m => PeerEnv e -> PeerM e m a -> m ()
withPeerM env action = void $ runReaderT (fromPeerM action) env

runProto :: forall e m  . ( MonadIO m
                          , HasOwnPeer e m
                          , HasFabriq e m
                          , HasPeer e
                          , Serialise (Encoded e)
                          )
        => [AnyProtocol e (ResponseM e m)]
        -> m ()

runProto hh = do
  me       <- ownPeer @e @m
  pipe     <- getFabriq

  -- defer <- newPipeline @(ResponseM e m ()) @m defProtoPipelineSize

  let resp = [ (pid, a) | a@AnyProtocol { myProtoId = pid } <- hh ]

  let disp = Map.fromList resp

  forever $ do

    messages <- receive pipe (To me)

    for_ messages $ \(From pip, bs) -> do

      case deserialiseOrFail @(AnyMessage e) bs of

        Left _-> pure ()

        Right (AnyMessage n msg) -> do

          case Map.lookup n disp of
            Nothing -> pure ()

            Just (AnyProtocol { protoDecode = decoder
                              , handle = h
                              }) -> maybe (pure ()) (runResponseM pip . h) (decoder msg)

instance ( HasProtocol e p
         , Serialise (Encoded e)
         , MonadTrans (ResponseM e)
         , HasStorage (PeerM e IO)
         ) => Response e p (ResponseM e (PeerM e IO)) where

  thatPeer _ = asks (view answTo)

  deferred _ action = do
    who <- asks (view answTo)
    fab <- lift $ getFabriq @e
    pip <- lift $ asks (view envDeferred)
    ss  <- lift getStorage
    liftIO $ addJob pip $ runPeerM ss fab who (runResponseM who action)

  response msg = do
    let proto = protoId @e @p (Proxy @p)
    who <- asks (view answTo)
    self <- lift $ ownPeer @e
    fab  <- lift $ getFabriq @e
    let bs = serialise (AnyMessage @e proto (encode msg))
    sendTo fab (To who) (From self) bs



instance ( MonadIO m
         , HasProtocol e p
         , Sessions e p m
         , Eq (SessionKey e p)
         , Typeable (SessionKey e p)
         , Typeable (SessionData e p)
         , Hashable (SessionKey e p)
         ) => Sessions e p (ResponseM e m) where

  find k f = lift (find k f)

  fetch i d k f = lift (fetch i d k f)

  update d k f = lift (update d k f)

  expire k = lift (expire  k)


runTestPeer :: Peer Fake
            -> (SimpleStorage HbSync -> IO ())
            -> IO ()

runTestPeer p zu = do

  dir <- liftIO $ canonicalizePath ( ".peers" </> show p)
  let chDir = dir </> "tmp-chunks"
  liftIO $ createDirectoryIfMissing True dir

  let opts = [ StoragePrefix dir
             ]

  stor <- simpleStorageInit @_ @_ @HbSync opts
  cww  <- newChunkWriterIO stor (Just chDir)

  sw <- liftIO $ async $ simpleStorageWorker stor
  cw <- liftIO $ async $ runChunkWriter cww

  zu stor

  simpleStorageStop stor
  stopChunkWriter cww

  mapM_ cancel [sw,cw]


handleBlockInfo :: forall e m . ( MonadIO m
                                , Sessions e (BlockSize e) m
                                , Default (SessionData e (BlockSize e))
                                , Ord (Peer e)
                                , Pretty (Peer e)
                                )

                => (Peer e, Hash HbSync, Maybe Integer)
                -> m ()

handleBlockInfo (p, h, sz') = do
   maybe1 sz' (pure ()) $ \sz -> do
    let bsz = fromIntegral sz
    update @e def (BlockSizeKey h) (over bsBlockSizes (Map.insert p bsz))
    liftIO $ debug $ "got block:" <+> pretty (p, h, sz)
    -- FIXME: turn back on event notification
    -- lift $ runEngineM env $ emitBlockSizeEvent ev h (p, h, Just sz) -- TODO: fix this crazy shit


blockDownloadLoop :: forall e . ( HasProtocol e (BlockSize e)
                                , Request e (BlockSize e) (PeerM e IO)
                                , Num (Peer e)
                                ) =>  PeerM e IO ()
blockDownloadLoop = do

  -- w <- subscribe ???

  request 1 (GetBlockSize @e "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt")
  request 1 (GetBlockSize @e "81JeD7LNR6Q7RYfyWBxfjJn1RsWzvegkUXae6FUNgrMZ")

  fix \next -> do
    liftIO $ print "piu!"

    pause ( 0.85 :: Timeout 'Seconds )
    next

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering

  void $ race (pause (10 :: Timeout 'Seconds)) $ do

    fake <- newFakeP2P True <&> Fabriq

    let (p0:ps) = [0..1] :: [Peer Fake]

    -- others
    others <- forM ps $ \p -> async $ runTestPeer p $ \s  -> do
                let findBlk = hasBlock s

                let size = 1024*1024

                let blk = B8.concat [ fromString (take 1 $ show x)
                                    | x <- replicate size (fromIntegral p :: Int)
                                    ]

                root <- putAsMerkle s blk

                debug $ "I'm" <+> pretty p <+> pretty root

                runPeerM (AnyStorage s) fake p $ do
                  runProto @Fake
                    [ makeResponse (blockSizeProto findBlk dontHandle)
                    -- , makeResponse (blockChunksProto undefined)
                    ]

    our <- async $ runTestPeer p0 $ \s  -> do
                let blk = hasBlock s
                runPeerM (AnyStorage s) fake p0 $ do
                  env <- ask

                  as <- liftIO $ async $ withPeerM env blockDownloadLoop

                  runProto @Fake
                    [ makeResponse (blockSizeProto blk handleBlockInfo)
                    -- , makeResponse (blockChunksProto undefined)
                    ]

                  liftIO $ cancel as

    pause ( 5 :: Timeout 'Seconds)

    mapM_ cancel (our:others)

    (_, e) <- waitAnyCatchCancel (our:others)

    debug (pretty $ show e)
    debug "we're done"
    assertBool "success" True
    exitSuccess

  assertBool "failed" False



