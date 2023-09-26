{-# Language TypeFamilyDependencies #-}
{-# Language FunctionalDependencies #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language TemplateHaskell #-}
{-# Language MultiWayIf #-}
module HBS2.Net.Proto.Types
  ( module HBS2.Net.Proto.Types
  ) where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Net.IP.Addr

import Control.Applicative
import Data.Digest.Murmur32
import Data.Hashable
import Data.Kind
import Data.Text qualified as Text
import GHC.TypeLits
import Lens.Micro.Platform
import Network.Socket
import System.Random qualified as Random
import Codec.Serialise
import Data.Maybe
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)

-- e -> Transport (like, UDP or TChan)
-- p -> L4 Protocol (like Ping/Pong)

data CryptoAction = Sign | Encrypt

data GroupKeyScheme = Symm | Asymm
  deriving stock (Eq,Ord,Show,Data,Generic)

type family PubKey  (a :: CryptoAction) e  :: Type
type family PrivKey (a :: CryptoAction) e  :: Type

type family Encryption e :: Type

data family GroupKey (scheme :: GroupKeyScheme) s

-- TODO: move-to-an-appropriate-place
newtype AsGroupKeyFile a = AsGroupKeyFile a

data family ToEncrypt (scheme :: GroupKeyScheme) s a -- = ToEncrypt a

data family ToDecrypt (scheme :: GroupKeyScheme) s a

-- FIXME: move-to-a-crypto-definition-modules
data HBS2Basic

data L4Proto = UDP | TCP
               deriving stock (Eq,Ord,Generic)
               deriving stock (Enum,Bounded)

instance Hashable L4Proto where
  hashWithSalt s l = hashWithSalt s ("l4proto", fromEnum l)

instance Show L4Proto where
  show UDP = "udp"
  show TCP = "tcp"

instance Pretty L4Proto where
  pretty UDP = "udp"
  pretty TCP = "tcp"

-- type family Encryption e :: Type

class Monad m => GenCookie e m where
  genCookie :: Hashable salt => salt -> m (Cookie e)


class Monad m => HasNonces p m where
  type family Nonce p :: Type
  newNonce :: m (Nonce p)


class HasCookie e p | p -> e where
  type family Cookie e :: Type
  getCookie :: p -> Maybe (Cookie e)
  getCookie = const Nothing

type PeerNonce = Nonce ()

class HasPeerNonce e m where
  peerNonce :: m PeerNonce

-- instance {-# OVERLAPPABLE #-} HasPeerNonce e IO where
--   peerNonce = newNonce @()


data WithCookie e p = WithCookie (Cookie e) p

class (Hashable (Peer e), Eq (Peer e)) => HasPeer e where
  data family (Peer e) :: Type

class ( Eq (PeerAddr e)
      , Monad m
      , Hashable (PeerAddr e)
      ) => IsPeerAddr e m where
  data family PeerAddr e :: Type

  toPeerAddr   :: Peer e -> m (PeerAddr e)
  fromPeerAddr :: PeerAddr e -> m (Peer e)

class (Monad m, HasProtocol e p) => HasThatPeer e p (m :: Type -> Type) where
  thatPeer :: Proxy p -> m (Peer e)

class (MonadIO m, HasProtocol e p) => HasDeferred e p m | p -> e where
  deferred :: Proxy p -> m () -> m ()

-- TODO: actually-no-idea-if-it-works
instance (HasDeferred e p m, Monad m) => HasDeferred e p (MaybeT m) where
  deferred p a = lift $ deferred p (void $ runMaybeT a)

class ( MonadIO m
      , HasProtocol e p
      , HasThatPeer e p m
      ) => Response e p m | p -> e where

  response :: p -> m ()

class HasProtocol e p => Request e p (m :: Type -> Type) | p -> e where
  request :: Peer e -> p -> m ()

data ReqLimPeriod = NoLimit
                  | ReqLimPerProto   (Timeout 'Seconds)
                  | ReqLimPerMessage (Timeout 'Seconds)

class (KnownNat (ProtocolId p), HasPeer e ) => HasProtocol e p | p -> e  where
  type family ProtocolId p = (id :: Nat) | id -> p
  type family Encoded e :: Type

  protoId :: forall . KnownNat (ProtocolId p) => Proxy p -> Integer
  protoId _ = natVal (Proxy @(ProtocolId p))

  decode :: Encoded e -> Maybe p
  encode :: p -> Encoded e

  requestPeriodLim :: ReqLimPeriod
  requestPeriodLim = NoLimit

-- FIXME: slow and dumb
instance {-# OVERLAPPABLE #-} (MonadIO m, Num (Cookie e)) => GenCookie e m where
  genCookie salt = do
    r <- liftIO $ Random.randomIO @Int
    pure $ fromInteger $ fromIntegral $ asWord32 $ hash32 (hash salt + r)

class FromSockAddr ( t :: L4Proto)  a where
  fromSockAddr :: SockAddr -> a

instance HasPeer L4Proto where
  data instance Peer L4Proto =
    PeerL4
    { _sockType :: L4Proto
    , _sockAddr :: SockAddr
    }
    deriving stock (Eq,Ord,Show,Generic)


instance AddrPriority (Peer L4Proto) where
  addrPriority (PeerL4 _ sa) = addrPriority sa

instance Hashable (Peer L4Proto) where
  hashWithSalt salt p = case _sockAddr p of
    SockAddrInet  pn h     -> hashWithSalt salt (4, fromEnum (_sockType p), fromIntegral pn, h)
    SockAddrInet6 pn _ h _ -> hashWithSalt salt (6, fromEnum (_sockType p), fromIntegral pn, h)
    SockAddrUnix s         -> hashWithSalt salt ("unix", s)

-- FIXME: support-udp-prefix
instance Pretty (Peer L4Proto) where
  pretty (PeerL4 UDP p) = pretty p
  pretty (PeerL4 TCP p) = "tcp://" <> pretty p

instance FromSockAddr 'UDP (Peer L4Proto) where
  fromSockAddr = PeerL4 UDP

instance FromSockAddr 'TCP (Peer L4Proto) where
  fromSockAddr = PeerL4 TCP

makeLenses 'PeerL4

newtype FromIP a = FromIP { fromIP :: a }


-- FIXME: tcp-and-udp-support
instance (MonadIO m) => IsPeerAddr L4Proto m where
-- instance MonadIO m => IsPeerAddr L4Proto m where
  data instance PeerAddr L4Proto =
    L4Address L4Proto (IPAddrPort L4Proto)
    deriving stock (Eq,Ord,Show,Generic)

  -- FIXME: backlog-fix-addr-conversion
  toPeerAddr (PeerL4 t p) = pure $ L4Address t (fromString $ show $ pretty p)
  --

  -- FIXME: ASAP-tcp-support
  fromPeerAddr (L4Address UDP iap) = do
    ai <- liftIO $ parseAddrUDP $ fromString (show (pretty iap))
    pure $ PeerL4 UDP $ addrAddress (head ai)

  fromPeerAddr (L4Address TCP iap) = do
    ai <- liftIO $ parseAddrTCP $ fromString (show (pretty iap))
    pure $ PeerL4 TCP $ addrAddress (head ai)

instance Hashable (PeerAddr L4Proto)

instance Pretty (PeerAddr L4Proto) where
  pretty (L4Address UDP a) = pretty a
  pretty (L4Address TCP a) = "tcp://" <> pretty a

instance IsString (PeerAddr L4Proto) where
  fromString s = fromMaybe (error "invalid address") (fromStringMay s)

instance FromStringMaybe (PeerAddr L4Proto) where
  fromStringMay s | Text.isPrefixOf "tcp://" txt = L4Address TCP <$> fromStringMay addr
                  | otherwise                    = L4Address UDP <$> fromStringMay addr
    where
      txt = fromString s :: Text
      addr = Text.unpack $ fromMaybe txt (Text.stripPrefix "tcp://" txt <|> Text.stripPrefix "udp://" txt)

instance Serialise L4Proto
instance Serialise (PeerAddr L4Proto)

