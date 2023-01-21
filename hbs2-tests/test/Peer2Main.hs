{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language RankNTypes #-}
{-# Language AllowAmbiguousTypes #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Storage
import HBS2.Net.Proto
import HBS2.Net.Proto.BlockChunks
import HBS2.Net.Proto.BlockInfo
import HBS2.Net.Messaging
import HBS2.Net.Messaging.Fake

import Control.Monad.Reader
import Data.Foldable
import Control.Monad
import Codec.Serialise hiding (encode,decode)
import Data.ByteString.Lazy (ByteString)
import Data.Default
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word
import GHC.TypeLits
import Lens.Micro.Platform
import Prettyprinter

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


type instance SessionData Fake (BlockSize Fake) = BlockSizeSession Fake
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

data Fabriq e = forall bus . Messaging bus e ByteString => Fabriq bus

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

class IsResponse e m where
  responseTo :: m (Peer e)


type ResponseM e = ReaderT (Peer e)

runResponseM :: forall e m . Monad m => Peer e -> ResponseM e m () -> m ()
runResponseM peer f = runReaderT f peer


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


data PeerEnv e =
  PeerEnv
  { _envSelf :: Peer e
  , _envFab  :: Fabriq e
  }

makeLenses 'PeerEnv

newtype PeerM e m a = PeerM { fromPeerM :: ReaderT (PeerEnv e) m a }
                      deriving newtype ( Functor
                                       , Applicative
                                       , Monad
                                       , MonadReader (PeerEnv e)
                                       , MonadIO
                                       )


instance Monad m => HasOwnPeer e (PeerM e m) where
  ownPeer = asks (view envSelf)

instance Monad m => HasFabriq e (PeerM e m) where
  getFabriq = asks (view envFab)

runPeerM p bus f = do
  let env = PeerEnv p bus
  runReaderT (fromPeerM f) env


instance (MonadIO m, HasProtocol e p) => Response e p (ResponseM e (PeerM e m))

main :: IO ()
main = do
  print "preved"

  fake <- newFakeP2P True

  runPeerM (FakePeer 0) (Fabriq fake) $ do
    runProto @Fake
      [ makeResponse (blockSizeProto undefined undefined)
      , makeResponse (blockChunksProto undefined)
      ]

  pure ()


