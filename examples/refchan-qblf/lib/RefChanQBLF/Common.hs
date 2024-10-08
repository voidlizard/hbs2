module RefChanQBLF.Common where

import HBS2.Data.Types
import HBS2.Peer.RPC.Client.Unix ()

import Control.Monad.Cont
import Control.Monad.Except
import Data.Bool
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter
import UnliftIO

data MyError
    = DeserializationError
    | SignatureError
    | SignerDoesNotMatchRefchan Text Text
    | TxUnsupported
    | SomeOtherError
    deriving stock (Generic, Show)
instance Serialise MyError
instance Exception MyError

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb mu = bool (pure ()) mu =<< mb

contAsync :: (MonadUnliftIO m) => m a -> ContT r m ()
contAsync = (link =<<) . ContT . withAsync

orE :: (MonadError e m) => e -> Maybe b -> m b
orE msg = maybe (throwError msg) pure

orEM :: (MonadError e m) => e -> m (Maybe b) -> m b
orEM msg mb = orE msg =<< mb

leftE :: (MonadError e m) => (a -> e) -> Either a b -> m b
leftE toe = either (throwError . toe) pure

leftEM :: (MonadError e m) => (a -> e) -> m (Either a b) -> m b
leftEM toe meab = leftE toe =<< meab

peelMWith
    :: (Monad m)
    => (e -> m a)
    -> (b -> Either e a)
    -> m b
    -> m a
peelMWith ema bea mb = either ema pure . bea =<< mb

newtype PrettyEither e a = PrettyEither (Either e a)

instance
    (Pretty e, Pretty a)
    => Pretty (PrettyEither e a)
    where
    pretty (PrettyEither ea) = case ea of
        Left e -> "Left" <+> pretty e
        Right a -> "Right" <+> pretty a
