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

ncqKeyNumIntersectionProbe :: MonadUnliftIO m => NCQStorage2 -> m ()
ncqKeyNumIntersectionProbe me@NCQStorage2{..} = useVersion me $ const $ void $ runMaybeT do

  -- Фильтруем pending
  files0 <- lift (ncqListTrackedFiles me)
  let files = V.toList $ V.filter (isNotPending . view _2) files0

  when (length files < 2) mzero

  (a,b) <- liftIO $ fix \next -> do
    i <- MWC.uniformRM (0, length files - 1) ncqRndGen
    j <- MWC.uniformRM (0, length files - 1) ncqRndGen
    if i == j then next else pure (files !! min i j, files !! max i j)

  let fka = view _1 a
  let fkb = view _1 b
  let key = FactKey $ coerce $ hashObject @HbSync $ serialise $ List.sort [fka, fkb]

  known <- lift (readTVarIO ncqFacts <&> HM.member key)
  when known mzero

  let fIndexA = ncqGetFileName me (toFileName (IndexFile fka))
  let fIndexB = ncqGetFileName me (toFileName (IndexFile fkb))

  idxPair' <- liftIO $ try @_ @IOException do
    (,) <$> nwayHashMMapReadOnly fIndexA
        <*> nwayHashMMapReadOnly fIndexB

  ((bs1,n1),(bs2,n2)) <- case idxPair' of
    Right (Just x, Just y) -> pure (x,y)
    _ -> warn ("can't load index pair" <+> pretty (fka, fkb)) >> mzero

  n <- liftIO $ do
    ref <- newTVarIO 0
    nwayHashScanAll n1 bs1 $ \_ k _ -> when (k /= ncqEmptyKey ) do
        here <- ncqLookupIndex (coerce k) (bs2,n2)
        when (isJust here) $ atomically $ modifyTVar' ref (+1)

    readTVarIO ref

  debug $ yellow "ncqKeyNumIntersectionProbe"
       <+> pretty fka <+> pretty fkb <+> pretty n


