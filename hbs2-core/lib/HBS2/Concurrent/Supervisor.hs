module HBS2.Concurrent.Supervisor
( module HBS2.Concurrent.Supervisor
, module X
) where

import Control.Arrow hiding ((<+>))
import Control.Concurrent.Async qualified as Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Proxy
import Data.Text (Text)
import Prettyprinter
import System.IO (Handle)
import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Async
import UnliftIO.Async as X hiding (async)
import UnliftIO.Concurrent
import UnliftIO.Exception

import HBS2.System.Logger.Simple


data Sup = Sup
  { supAsync :: Async ()
  }

data SupFinished = SupFinished Text
  deriving (Show)
instance Exception SupFinished

withAsyncSupervisor :: (MonadUnliftIO io) => Text -> (Sup -> io a) -> io a
withAsyncSupervisor name k =
    bracket
        (Sup <$> async (forever (threadDelay (10^9))))
        (flip throwTo (SupFinished name) . asyncThreadId . supAsync)
        (\sup -> (k sup)
              `withException` \(e :: SomeException) -> do
                  debug $ "Finished sup " <> pretty name <> " " <> viaShow e
        )

asyncStick :: MonadUnliftIO m => Sup -> m a -> m (Async a)
asyncStick sup ioa = do
    a <- async ioa
    liftIO $ Async.link2Only (const True) (supAsync sup) a
    pure a

asyncStick' :: MonadUnliftIO m => Sup -> Text -> m a -> m (Async a)
asyncStick' sup name ioa = do
    a <- async $
        ioa
            `withException` \(e :: SomeException) ->
                debug $ "finished async" <+> pretty name <+> ":" <+> viaShow e
    liftIO $ Async.link2Only (const True) (supAsync sup) a
    pure a


selectException_ :: forall e m . (Exception e, Monad m)
  => Proxy e -> SomeException -> MaybeT m ()
selectException_ _ = fromException >>> \case
    Nothing -> MaybeT (pure Nothing)
    Just (e :: e) -> pure ()

selectException :: forall e m . (Exception e, Monad m)
  => SomeException -> (e -> m ()) -> MaybeT m ()
selectException e f = case (fromException e) of
    Nothing -> MaybeT (pure Nothing)
    Just e' -> lift (f e')

withExceptionIO :: Exception e => IO a -> (e -> IO b) -> IO a
withExceptionIO io what = io `catch` \e -> do
      _ <- what e
      throwIO e

withSomeExceptionIO :: IO a -> (SomeException -> IO b) -> IO a
withSomeExceptionIO = withExceptionIO

