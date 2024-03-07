module HBS2.Git.Client.RefLog where

import HBS2.Git.Client.Prelude
import HBS2.Git.Client.App.Types
import HBS2.Git.Data.RefLog

data RefLogRequestTimeout = RefLogRequestTimeout
                            deriving (Show,Typeable)

data RefLogRequestError = RefLogRequestError
                            deriving (Show,Typeable)

instance Exception RefLogRequestTimeout

instance Exception RefLogRequestError

subscribeRefLog :: (GitPerks m, MonadReader GitEnv m) => RefLogId -> m ()
subscribeRefLog puk = do
  api <- asks _peerAPI
  void $ callService @RpcPollAdd api (puk, "reflog", 13)


getRefLogMerkle :: (GitPerks m, MonadReader GitEnv m) => RefLogId -> m (Maybe HashRef)
getRefLogMerkle puk = do

  api <- asks _refLogAPI

  void $ race (pause @'Seconds 1) (callService @RpcRefLogFetch api puk)
          >>= orThrow RefLogRequestTimeout
          >>= orThrow RefLogRequestError

  race (pause @'Seconds 1) (callService @RpcRefLogGet api puk)
    >>= orThrow RefLogRequestTimeout
    >>= orThrow RefLogRequestError



