{-# Language RecordWildCards #-}
module HBS2.Storage.NCQ2.Internal.Probes where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.System.Logger.Simple.ANSI
import HBS2.Misc.PrettyStuff

import HBS2.Data.Log.Structured.NCQ

import HBS2.Storage.NCQ2.Internal.Types
import HBS2.Storage.NCQ.Types

import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Maybe
import Data.Vector ((!))
import Data.Vector qualified as V
import Lens.Micro.Platform
import System.Random.MWC qualified as MWC
import UnliftIO


randomTrackedFile :: MonadUnliftIO m => NCQStorage2 -> m (Maybe FileKey)
randomTrackedFile ncq@NCQStorage2{..} = runMaybeT do
  files0 <- lift (ncqListTrackedFiles ncq)
  let files = V.toList $ V.filter (isNotPending . view _2) files0
  guard (not (null files))
  i <- liftIO $ MWC.uniformRM (0, length files - 1) ncqRndGen
  pure (view _1 (files !! i))

randomTrackedFilePair :: MonadUnliftIO m => NCQStorage2 -> m (Maybe (FileKey, FileKey))
randomTrackedFilePair ncq@NCQStorage2{..} = runMaybeT do
  files0 <- lift (ncqListTrackedFiles ncq)
  let files = V.toList $ V.filter (isNotPending . view _2) files0
  guard (length files >= 2)

  (a, b) <- liftIO $ fix \loop -> do
    i <- MWC.uniformRM (0, length files - 1) ncqRndGen
    j <- MWC.uniformRM (0, length files - 1) ncqRndGen
    if i == j then loop else pure (min i j, max i j)

  let fa = view _1 (files !! a)
  let fb = view _1 (files !! b)
  pure (fa, fb)


ncqTombCountProbeFor :: MonadUnliftIO m => NCQStorage2 -> FileKey -> m (Maybe Int)
ncqTombCountProbeFor ncq@NCQStorage2{..} fkey = runMaybeT do
  let fIndex = ncqGetFileName ncq $ toFileName (IndexFile fkey)

  (bs, nh) <- liftIO (nwayHashMMapReadOnly fIndex) >>= toMPlus

  liftIO do
    ref <- newTVarIO 0
    nwayHashScanAll nh bs $ \_ k v -> do
      let NCQIdxEntry _ s = decodeEntry v
      when (k /= ncqEmptyKey && s < 64) $
        atomically $ modifyTVar' ref (+1)
    readTVarIO ref

ncqKeyNumIntersectionProbeFor :: MonadUnliftIO m => NCQStorage2 -> (FileKey, FileKey) -> m (Maybe Int)
ncqKeyNumIntersectionProbeFor ncq@NCQStorage2{..} (fka, fkb) = runMaybeT do
  let key = FactKey $ coerce $ hashObject @HbSync $ serialise $ List.sort [fka, fkb]

  known <- lift (readTVarIO ncqFacts <&> HM.member key)
  guard (not known)

  let fIndexA = ncqGetFileName ncq (toFileName (IndexFile fka))
  let fIndexB = ncqGetFileName ncq (toFileName (IndexFile fkb))

  idxPair' <- liftIO $ try @_ @IOException do
    (,) <$> nwayHashMMapReadOnly fIndexA
        <*> nwayHashMMapReadOnly fIndexB

  ((bs1,n1),(bs2,n2)) <- case idxPair' of
    Right (Just x, Just y) -> pure (x,y)
    _ -> warn ("can't load index pair" <+> pretty (fka, fkb)) >> mzero

  liftIO do
    ref <- newTVarIO 0
    nwayHashScanAll n1 bs1 $ \_ k _ -> when (k /= ncqEmptyKey) do
      here <- ncqLookupIndex (coerce k) (bs2,n2)
      when (isJust here) $ atomically $ modifyTVar' ref (+1)
    readTVarIO ref


ncqTombCountProbe :: MonadUnliftIO m => NCQStorage2 -> m ()
ncqTombCountProbe ncq = useVersion ncq $ const $ void $ runMaybeT do
  fk <- MaybeT (randomTrackedFile ncq)
  count <- MaybeT (ncqTombCountProbeFor ncq fk)
  debug $ yellow "ncqTombCountProbe" <+> pretty fk <+> pretty count

ncqKeyNumIntersectionProbe :: MonadUnliftIO m => NCQStorage2 -> m ()
ncqKeyNumIntersectionProbe ncq = useVersion ncq $ const $ void $ runMaybeT do
  (fa, fb) <- MaybeT (randomTrackedFilePair ncq)
  n <- MaybeT (ncqKeyNumIntersectionProbeFor ncq (fa, fb))
  debug $ yellow "ncqKeyNumIntersectionProbe" <+> pretty fa <+> pretty fb <+> pretty n

