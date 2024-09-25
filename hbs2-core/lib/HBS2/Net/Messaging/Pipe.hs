{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NumericUnderscores #-}
module HBS2.Net.Messaging.Pipe where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Types
import HBS2.Actors.Peer.Types
import HBS2.Net.Messaging

import Control.Concurrent.STM qualified as STM
import Control.Monad.Fix
import Control.Monad.Reader
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Hashable
import Network.ByteOrder hiding (ByteString)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.IO
import UnliftIO

-- define new transport protocol type
data PIPE = PIPE
            deriving (Eq,Ord,Show,Generic)


-- address for the new protocol
newtype PipeAddr = PipeAddr Handle
                   deriving newtype (Eq,Show)

-- the protocol work data
data MessagingPipe =
  MessagingPipe
  { pipeIn  :: Handle
  , pipeOut :: Handle
  , inQ     :: TQueue ByteString
  }

remotePeer :: MessagingPipe -> Peer PIPE
remotePeer = PeerPIPE . PipeAddr . pipeOut

localPeer :: MessagingPipe -> Peer PIPE
localPeer = PeerPIPE . PipeAddr . pipeIn

newMessagingPipe :: MonadIO m => (Handle, Handle) -> m MessagingPipe
newMessagingPipe (pIn,pOut) = do
  MessagingPipe pIn pOut
    <$> newTQueueIO

instance Hashable PipeAddr where
  hashWithSalt salt (PipeAddr pip) = hashWithSalt salt ("pipe-addr" :: String, fd)
    where
      fd = unsafePerformIO (handleToFd pip <&> fromIntegral @_ @Word)

instance HasPeer PIPE where
  newtype instance Peer PIPE = PeerPIPE { _fromPeerPipe :: PipeAddr }
    deriving stock (Eq,Show,Generic)
    deriving newtype (Hashable)

instance Pretty (Peer PIPE) where
  pretty (PeerPIPE p) = parens ("pipe" <+> viaShow p)

-- Messaging definition for protocol
instance Messaging MessagingPipe PIPE ByteString where

  sendTo bus _ _ msg = liftIO do
    LBS.hPutStr (pipeOut bus) (B.toLazyByteString frame <> msg)
    hFlush (pipeOut bus)

    where
      frame = B.word32BE (fromIntegral $ LBS.length msg)

  receive bus _ = do
    msg <- liftIO $ atomically $ peekTQueue q >> STM.flushTQueue q
    for msg $ \m -> pure (From (PeerPIPE (PipeAddr who)), m)

    where
      q = inQ bus
      who = pipeIn bus

runMessagingPipe :: MonadIO m => MessagingPipe -> m ()
runMessagingPipe bus = liftIO do
  fix \next -> do
    frame <- LBS.hGet who 4 <&> word32 . LBS.toStrict
    piece <- LBS.hGet who (fromIntegral frame)
    atomically (writeTQueue (inQ bus) piece)
    next

  where
    who = pipeIn bus

instance (MonadIO m, Messaging MessagingPipe PIPE (Encoded PIPE))
  => HasFabriq PIPE (ReaderT MessagingPipe m) where
  getFabriq = asks Fabriq

instance MonadIO m => HasOwnPeer PIPE (ReaderT MessagingPipe m) where
  ownPeer = asks localPeer


