module HBS2.Git.Client.RefLog
  ( module HBS2.Git.Client.RefLog
  , module HBS2.Peer.Proto.RefLog
  ) where

import HBS2.Git.Client.Prelude
import HBS2.Git.Client.App.Types
import HBS2.Git.Data.RefLog
import HBS2.Git.Data.LWWBlock
import HBS2.Peer.Proto.RefLog

data RefLogRequestTimeout = RefLogRequestTimeout
                            deriving (Show,Typeable)

data RefLogRequestError = RefLogRequestError
                            deriving (Show,Typeable)

instance Exception RefLogRequestTimeout

instance Exception RefLogRequestError

doSomeRandomShit :: HasAPI PeerAPI UNIX m => m ()
doSomeRandomShit = error "FUCK"

subscribeRefLog :: forall m .(GitPerks m, HasAPI PeerAPI UNIX m) => RefLogId -> m ()
subscribeRefLog puk = do
  api <- getAPI @PeerAPI @UNIX
  void $ callService @RpcPollAdd api (puk, "reflog", 13)

subscribeLWWRef :: forall m . (GitPerks m, HasAPI PeerAPI UNIX m) => LWWRefKey HBS2Basic -> m ()
subscribeLWWRef puk = do
  api <- getAPI @PeerAPI @UNIX
  void $ callService @RpcPollAdd api (fromLwwRefKey puk, "lwwref", 17)

fetchLWWRef :: forall m . (GitPerks m, HasAPI LWWRefAPI UNIX m) => LWWRefKey HBS2Basic -> m ()
fetchLWWRef key = do
  api <- getAPI @LWWRefAPI @UNIX
  void $ race (pause @'Seconds 1) (callService @RpcLWWRefFetch api key)

getRefLogMerkle :: forall m . (GitPerks m, HasAPI RefLogAPI UNIX m) => RefLogId -> m (Maybe HashRef)
getRefLogMerkle puk = do

  api <- getAPI @RefLogAPI @UNIX

  void $ race (pause @'Seconds 1) (callService @RpcRefLogFetch api puk)
          >>= orThrow RefLogRequestTimeout
          >>= orThrow RefLogRequestError

  race (pause @'Seconds 1) (callService @RpcRefLogGet api puk)
    >>= orThrow RefLogRequestTimeout
    >>= orThrow RefLogRequestError



