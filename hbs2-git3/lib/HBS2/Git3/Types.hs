module HBS2.Git3.Types
  ( module HBS2.Git3.Types
  , module Exported
  ) where

import HBS2.Prelude.Plated
import HBS2.Net.Auth.Credentials
import HBS2.Git.Local as Exported
import UnliftIO
import Control.Monad.Trans.Cont
import Control.Concurrent.STM qualified as STM


type GitRemoteKey = PubKey 'Sign 'HBS2Basic

type GitRepoKey = PubKey 'Sign HBS2Basic

newtype Short x = Short x

instance Pretty (Short GitObjectType) where
  pretty = \case
    (Short Tree)   -> "T"
    (Short Blob)   -> "B"
    (Short Commit) -> "C"


instance FromStringMaybe (Short GitObjectType) where
  fromStringMay = \case
    "T" -> Just (Short Tree)
    "B" -> Just (Short Blob)
    "C" -> Just (Short Commit)
    _   -> Just (Short Blob)


instance FromStringMaybe (Short SegmentObjectType) where
  fromStringMay = \case
    "T" -> Just (Short (GitObject Tree))
    "B" -> Just (Short (GitObject Blob))
    "C" -> Just (Short (GitObject Commit))
    "R" -> Just (Short RefObject)
    _   -> Just (Short (GitObject Blob))

data SegmentObjectType =
     GitObject GitObjectType
   | RefObject


contWorkerPool :: (MonadUnliftIO m)
  => Int
  -> ContT () m (a -> m b)
  -> ContT () m (a -> m b)
contWorkerPool n w = fmap join <$> contWorkerPool' n w

-- | здесь: a -> m (m b)
-- первое m - чтобы задать вопрос
-- второе m - чтобы получить ответ
contWorkerPool' :: (MonadUnliftIO m)
  => Int
  -> ContT () m (a -> m b)
  -> ContT () m (a -> m (m b))
contWorkerPool' n contWorker = do
    inQ <- newTQueueIO
    -- запускаем воркеров
    replicateM_ n do
      (link <=< ContT . withAsync) do
        runContT contWorker \w -> do
          (fix . (>>)) do
            (a, reply) <- atomically $ readTQueue inQ
            reply =<< tryAny (w a)
    -- возвращаем функцию, с помощью которой отправлять воркерам запрос
    -- и получать ответ
    pure \a -> do
      tmv <- newEmptyTMVarIO
      atomically $ writeTQueue inQ (a, atomically . STM.putTMVar tmv)
      pure do
        either throwIO pure =<< atomically (readTMVar tmv)


