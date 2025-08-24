{-# Language ViewPatterns #-}
module HBS2.Storage.NCQ3.Internal.State where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.Flags
import HBS2.Storage.NCQ3.Internal.MMapCache

import Data.Config.Suckless.Script

import Data.Generics.Labels()
import Data.List qualified as List
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as Set
import UnliftIO.IO.File
import System.IO qualified as IO
import Lens.Micro.Platform

import Control.Concurrent.STM qualified as STM

newtype StateOP a =
  StateOP { fromStateOp :: ReaderT NCQStorage STM a }
  deriving newtype (Functor,Applicative,Monad,MonadReader NCQStorage)

{- HLINT ignore "Eta reduce"-}

ncqStateDump :: MonadIO m
             => NCQStorage
             -> m FileKey
ncqStateDump ncq@NCQStorage{..} = do
    state <- readTVarIO ncqState
    key <-  ncqGetNewFileKey ncq StateFile
    let snkFile = ncqGetFileName ncq (StateFile key)

    liftIO $ withBinaryFileDurableAtomic snkFile WriteMode $ \fh -> do
      IO.hPrint fh (pretty state)

    atomically do
      writeTVar ncqStateKey key
      ncqClearFlagSTM  ncqStateDumpReq

    debug $ yellow "ncqStateDump" <+> pretty key <+> pretty (toFileName (StateFile key))
    pure key

ncqStateUpdateLoop :: MonadIO m
                   => NCQStorage
                   -> m ()

ncqStateUpdateLoop ncq@NCQStorage{..} = do

  debug $ red "ncqStateUpdateLoop"


  sInit <- readTVarIO ncqState

  flip fix sInit $ \next s0 -> do
    state <- atomically do
      s1   <- readTVar ncqState
      stop <- readTVar ncqStopReq
      dump <- readTVar ncqStateDumpReq
      if s1 == s0 && not stop && not dump then STM.retry else pure s1

    key <- ncqStateDump ncq

    done <- atomically do
      modifyTVar ncqWrites succ
      readTVar ncqStopReq

    unless done do
      next =<< readTVarIO ncqState

ncqStateUpdate :: MonadIO m
               => NCQStorage
               -> StateOP a
               -> m ()

ncqStateUpdate ncq@NCQStorage{..} action = do
  atomically do
    s0 <- readTVar ncqState
    void $ runReaderT (fromStateOp action) ncq
    s1 <- readTVar ncqState
    when (s0 /= s1) do
      modifyTVar ncqState (over #ncqStateVersion succ)
      ncqSetFlagSTM ncqStateDumpReq

ncqStateAddDataFile :: FileKey -> StateOP ()
ncqStateAddDataFile fk = do
  NCQStorage{..} <- ask
  StateOP $ lift do
    modifyTVar ncqState (over #ncqStateFiles (HS.insert fk))

ncqStateDelDataFile :: FileKey -> StateOP ()
ncqStateDelDataFile fk = do
  sto@NCQStorage{..} <- ask
  StateOP $ lift do
    modifyTVar ncqState (over #ncqStateFiles (HS.delete fk))
    ncqDelCachedDataSTM sto fk

ncqStateAddFact :: Fact -> StateOP ()
ncqStateAddFact fact = do
  NCQStorage{..} <- ask
  StateOP $ lift do
    modifyTVar ncqState (over #ncqStateFacts (Set.insert fact))

ncqStateDelFact :: Fact -> StateOP ()
ncqStateDelFact fact = do
  NCQStorage{..} <- ask
  StateOP $ lift do
    modifyTVar ncqState (over #ncqStateFacts (Set.delete fact))

ncqStateAddIndexFile :: POSIXTime
                     -> FileKey
                     -> StateOP ()

ncqStateAddIndexFile ts fk  = do
  NCQStorage{..} <- ask
  StateOP $ lift $ modifyTVar' ncqState (sortIndexes . over #ncqStateIndex ((Down ts, fk) :))

ncqStateDelIndexFile :: FileKey  -> StateOP ()
ncqStateDelIndexFile fk  = do
  sto@NCQStorage{..} <- ask
  StateOP $ lift do
    modifyTVar' ncqState (over #ncqStateIndex $ filter f)
    ncqDelCachedIndexSTM sto fk

  where f (_,b) = b /= fk

sortIndexes :: NCQState -> NCQState
sortIndexes = over #ncqStateIndex sortIndexes0


ncqStateCapture :: forall m . MonadUnliftIO m
            => NCQStorage
            -> m FileKey

ncqStateCapture me@NCQStorage{..} = do
  atomically do
    key      <- readTVar ncqStateKey
    stateUse <- readTVar ncqStateUse
    case HM.lookup key stateUse of
      Just (_, tv) -> modifyTVar tv succ
      Nothing      -> do
        state    <- readTVar ncqState
        new   <- (state,) <$> newTVar 1
        modifyTVar ncqStateUse (HM.insert key new)
    pure key

ncqStateDismiss :: forall m . MonadUnliftIO m
                => NCQStorage
                -> FileKey
                -> m ()
ncqStateDismiss me@NCQStorage{..} key = atomically do
  useMap <- readTVar ncqStateUse
  case HM.lookup key useMap of
    Nothing -> pure ()
    Just (_, tv) -> do
      modifyTVar tv (max 0 . pred)
      cnt <- readTVar tv
      when (cnt <= 0) do
        modifyTVar ncqStateUse (HM.delete key)

ncqWithState :: forall a m . MonadUnliftIO m
             => NCQStorage
             -> ( FileKey -> m a  )
             -> m a
ncqWithState sto = bracket (ncqStateCapture sto) (ncqStateDismiss sto)

readStateMay :: forall m . MonadUnliftIO m
             => NCQStorage
             -> FileKey
             -> m (Maybe NCQState)
readStateMay sto key = fmap sortIndexes <$> do
  s <- liftIO (readFile (ncqGetFileName sto (StateFile key)))
  runMaybeT do
    sexps <- parseTop s & toMPlus

    flip fix (ncqState0, sexps) $ \next -> \case
      (acc, []) -> pure acc
      (acc, e : ss)  -> next (acc <> entryOf e, ss)

  where

    entryOf = \case
       ListVal [SymbolVal "i",  LitIntVal n, LitIntVal ts] ->
         ncqState0 { ncqStateIndex = [ (fromIntegral ts, fromIntegral n) ] }

       ListVal [SymbolVal "f",  LitIntVal n] ->
         ncqState0 { ncqStateFiles = HS.singleton (fromIntegral n) }

       ListVal [SymbolVal "fp", LitIntVal a, LitIntVal s] ->
         ncqState0 { ncqStateFacts = Set.singleton (P (PData (DataFile $ fromIntegral a) (fromIntegral s))) }

       ListVal [SymbolVal "n", LitIntVal a] ->
         ncqState0 { ncqStateFileSeq = fromIntegral a }

       _ -> ncqState0

-- aux. functions for tests and something

ncqGetIndex :: MonadIO m => NCQStorage -> m [(Down POSIXTime, FileKey)]
ncqGetIndex NCQStorage{..} =
  readTVarIO ncqState <&> view #ncqStateIndex

ncqGetIndexSTM :: NCQStorage -> STM [(Down POSIXTime, FileKey)]
ncqGetIndexSTM NCQStorage{..} =
  view #ncqStateIndex <$> readTVar ncqState

