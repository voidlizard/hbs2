{-# Language MultiWayIf #-}
module Fixme.GK where

import Fixme.Prelude
import Fixme.Config
import Fixme.Types

import HBS2.OrDie
-- import HBS2.System.Dir
import HBS2.Storage.Operations.ByteString
import HBS2.Storage.Operations.Class

import HBS2.Net.Auth.GroupKeySymm
import HBS2.Peer.Proto.RefChan as RefChan
import HBS2.System.Dir
-- import HBS2.Net.Auth.Credentials

import Control.Monad.Trans.Maybe
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Lens.Micro.Platform

data GroupKeyOpError =
    NoRefChanHead
  | NoReadersSet
  deriving (Eq,Ord,Show,Typeable)

instance Exception GroupKeyOpError


groupKeyFile :: forall m . FixmePerks m => m FilePath
groupKeyFile = do
  dir <- localConfigDir
  pure $ dir </> "gk0"

-- TODO: rotate-group-key

loadGroupKey :: forall s m . (s ~ 'HBS2Basic, FixmePerks m) => FixmeM m (Maybe (HashRef, GroupKey 'Symm s))
loadGroupKey = do

  sto <- getStorage
  gkF <- groupKeyFile

  runMaybeT do

    rchan <- lift (asks fixmeEnvRefChan >>= readTVarIO) >>= toMPlus

    rch <- getRefChanHead @L4Proto sto (RefChanHeadKey rchan)
             >>= orThrow NoRefChanHead

    guard ( not $ HS.null (view refChanHeadReaders  rch) )

    flip fix 0 $ \next -> \case

        attempt | attempt > 2 -> mzero

        attempt -> do

          let readers = view refChanHeadReaders rch

          gkHash <- liftIO (try @_ @IOError $ readFile gkF)
                      <&> either (const Nothing) ( (=<<) (fromStringMay @HashRef) . headMay . lines )

          debug $ "GK0" <+> pretty gkHash

          case gkHash of
            Nothing -> do
              debug "generate new group key"
              gknew <- generateGroupKey @'HBS2Basic Nothing (HS.toList readers)
              ha <- writeAsMerkle sto (serialise gknew)
              liftIO $ writeFile gkF (show $ pretty ha)
              next (succ attempt)

            Just h -> do
              now <- liftIO $ getPOSIXTime <&> round
              gk' <- loadGroupKeyMaybe @s sto h

              (_,gk) <- maybe1 gk' (rm gkF >> next (succ attempt)) (pure . (h,))

              let ts = getGroupKeyTimestamp gk & fromMaybe 0

              -- FIXME: timeout-hardcode
              --   $class: hardcode
              if | now - ts > 2592000 -> do
                   rm gkF
                   next (succ attempt)

                 | HM.keysSet (recipients gk) /= readers -> do
                   rm gkF
                   next (succ attempt)

                 | otherwise -> do
                    pure (h,gk)

