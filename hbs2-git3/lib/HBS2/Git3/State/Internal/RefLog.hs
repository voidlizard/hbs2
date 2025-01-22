module HBS2.Git3.State.Internal.RefLog where

import HBS2.Git3.Prelude

import HBS2.Git3.State.Internal.Types

import Control.Applicative
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
import Streaming.Prelude qualified as S

data GitTx =
    TxSegment    { gitTxTree :: HashRef }
  | TxCheckpoint { gitTxRank :: Natural, gitTxTree ::  HashRef }

getGitTxRank  :: GitTx -> Natural
getGitTxRank = \case
  TxSegment _ -> 0
  TxCheckpoint n _ -> n

data RefLogException =
    RefLogRPCException
  deriving stock (Show, Typeable)

instance Exception RefLogException

readTxMay :: forall m . ( MonadIO m
                        )
          => AnyStorage -> HashRef -> m (Maybe GitTx)

readTxMay sto href = runMaybeT do

    tx <- getBlock sto (coerce href)
             >>= toMPlus

    RefLogUpdate{..} <- deserialiseOrFail @(RefLogUpdate L4Proto) tx
      & toMPlus

    toMPlus $
     ( deserialiseOrFail (LBS.fromStrict _refLogUpdData) & either (const Nothing) fromAnn )
      <|>
     ( deserialiseOrFail (LBS.fromStrict _refLogUpdData) & either (const Nothing) fromSeq )

  where
    fromAnn = \case
      AnnotatedHashRef _ h -> Just (TxSegment h)

    fromSeq = \case
      (SequentialRef n (AnnotatedHashRef _ h)) -> Just $ TxCheckpoint (fromIntegral n) h

refLogRef :: forall m . ( HBS2GitPerks m
                        , HasStorage m
                        , HasClientAPI RefLogAPI UNIX m
                        , HasGitRemoteKey m
                        )
            => m (Maybe HashRef)

refLogRef = do
  refLogAPI <- getClientAPI @RefLogAPI  @UNIX
  reflog <- getGitRemoteKey >>= orThrow RefLogNotSet

  callRpcWaitMay @RpcRefLogGet (TimeoutSec 1) refLogAPI reflog
   >>= orThrow RefLogNotSet

txList :: forall m . ( HBS2GitPerks m
                     , HasStorage m
                     , HasClientAPI RefLogAPI UNIX m
                     , HasGitRemoteKey m
                     )
       => ( HashRef -> m Bool )
       -> Maybe HashRef
       -> m [(HashRef, GitTx)]

txList filt mhref = do

  sto <- getStorage

  fromMaybe mempty <$> runMaybeT do

    rv <- case mhref of
           Just x -> pure x
           Nothing -> lift refLogRef >>= toMPlus

    hxs <- S.toList_ $ walkMerkle @[HashRef] (coerce rv) (getBlock sto) $ \case
      Left{} -> throwIO (MissedBlockError2 "txList")
      Right hs -> filterM (lift . lift . filt) hs >>= S.each

    S.toList_ $ for_ hxs $ \h -> do
      tx <- liftIO (readTxMay sto h)
      maybe none (S.yield . (h,)) tx

txListAll :: forall m . ( HBS2GitPerks m
                        , HasStorage m
                        , HasClientAPI RefLogAPI UNIX m
                        , HasGitRemoteKey m
                        )
       => Maybe HashRef
       -> m [(HashRef, GitTx)]

txListAll = txList (const $ pure True)

