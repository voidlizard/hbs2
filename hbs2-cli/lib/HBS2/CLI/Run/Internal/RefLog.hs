{-# Language AllowAmbiguousTypes #-}
module HBS2.CLI.Run.Internal.RefLog (copyTransactions, RefLogCLIException(..)) where

import HBS2.CLI.Prelude hiding (mapMaybe)
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.KeyMan

import HBS2.Peer.Proto.RefLog
import HBS2.Base58
import HBS2.Storage
import HBS2.Data.Detect
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema()
import HBS2.Peer.Proto
import HBS2.Data.Types.SignedBox
import HBS2.Data.Types.Refs
import HBS2.Storage.Operations.Class
import HBS2.KeyMan.Keys.Direct

import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog

import Codec.Serialise
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce
import Data.Maybe
import Lens.Micro.Platform

data RefLogCLIException =
   RefLogRpcTimeout
 | RefLogNoCredentials String
 deriving (Typeable, Show)

instance Exception RefLogCLIException

type ForCloneRefLog e s m = ( s ~ Encryption e
                            , MonadUnliftIO m
                            , HasClientAPI RefLogAPI UNIX m
                            , HasClientAPI StorageAPI UNIX m
                            , HasClientAPI PeerAPI UNIX m
                            , HasStorage m
                            , Signatures s
                            , IsRefPubKey s
                            , Serialise (Nonce (RefLogUpdate e))
                            )


-- useful for forking git repositories
-- it accepts credential lookup method
-- since reflog B may be inferred from some other secret
-- normally, you dont need this method
copyTransactions :: forall e s m . (ForCloneRefLog e s m, s ~ Encryption e, e ~ L4Proto)
                 => m (PeerCredentials s) -- ^ obtain credentials for reflog B
                 -> PubKey Sign s         -- ^ original reflog
                 -> PubKey Sign s         -- ^ destination reflog
                 -> m ()

copyTransactions cre a b  = do

  api <- getClientAPI @RefLogAPI @UNIX
  sto <- getStorage

  creds <- cre

  let pk = view peerSignPk creds
  let sk = view peerSignSk creds

  void $ runMaybeT do

    rvA <- lift (callRpcWaitMay @RpcRefLogGet (TimeoutSec 1) api a)
             >>= orThrow RefLogRpcTimeout
             >>= toMPlus

    logA <- readLogThrow (getBlock sto) rvA

    new <- for logA $ \h -> runMaybeT do
      RefLogUpdate{..} <- getBlock sto (coerce h)
                            >>= toMPlus
                            <&> deserialiseOrFail @(RefLogUpdate e)
                            >>= toMPlus

      lift (makeRefLogUpdate @e pk sk _refLogUpdData)

    lift $ for_ (catMaybes new) $ \n -> do
      void $ callService @RpcRefLogPost api n


