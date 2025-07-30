{-# Language ViewPatterns #-}
module HBS2.Storage.NCQ3.Internal.State where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.MMapCache

import Data.Config.Suckless.Script

import Data.Generics.Product
import Data.Generics.Labels
import Data.List qualified as List
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as Set
import Data.ByteString qualified as BS
import UnliftIO.IO.File
import Network.ByteOrder qualified as N
import UnliftIO.IO
import System.IO qualified as IO
import Lens.Micro.Platform
import Streaming.Prelude qualified as S

newtype StateOP a =
  StateOP { fromStateOp :: ReaderT NCQStorage3 STM a }
  deriving newtype (Functor,Applicative,Monad,MonadReader NCQStorage3)

{- HLINT ignore "Eta reduce"-}

ncqStateUpdate :: MonadIO m
               => NCQStorage3
               -> StateOP a
               -> m ()
ncqStateUpdate ncq@NCQStorage3{..} action = do
  s0 <- readTVarIO ncqState

  s1 <- atomically do
          void $ runReaderT (fromStateOp action) ncq
          modifyTVar ncqWrites succ
          readTVar ncqState

  unless (s1 == s0) do
    key <-  ncqGetNewFileKey ncq StateFile
    let snkFile = ncqGetFileName ncq (StateFile key)
    liftIO $ withBinaryFileDurableAtomic snkFile WriteMode $ \fh -> do
      IO.hPrint fh (pretty s1)
    atomically $ writeTVar ncqStateKey key

ncqStateAddDataFile :: FileKey -> StateOP ()
ncqStateAddDataFile fk = do
  NCQStorage3{..} <- ask
  StateOP $ lift do
    modifyTVar ncqState (over #ncqStateFiles (HS.insert fk))

ncqStateDelDataFile :: FileKey -> StateOP ()
ncqStateDelDataFile fk = do
  sto@NCQStorage3{..} <- ask
  StateOP $ lift do
    modifyTVar ncqState (over #ncqStateFiles (HS.delete fk))
    ncqDelCachedDataSTM sto fk

ncqStateAddFact :: Fact -> StateOP ()
ncqStateAddFact fact = do
  NCQStorage3{..} <- ask
  StateOP $ lift do
    modifyTVar ncqState (over #ncqStateFacts (Set.insert fact))

ncqStateDelFact :: Fact -> StateOP ()
ncqStateDelFact fact = do
  NCQStorage3{..} <- ask
  StateOP $ lift do
    modifyTVar ncqState (over #ncqStateFacts (Set.delete fact))

ncqStateAddIndexFile :: POSIXTime
                     -> FileKey
                     -> StateOP ()

ncqStateAddIndexFile ts fk  = do
  NCQStorage3{..} <- ask
  StateOP $ lift $ modifyTVar' ncqState (sortIndexes . over #ncqStateIndex ((Down ts, fk) :))

ncqStateDelIndexFile :: FileKey  -> StateOP ()
ncqStateDelIndexFile fk  = do
  sto@NCQStorage3{..} <- ask
  StateOP $ lift do
    modifyTVar' ncqState (over #ncqStateIndex $ filter f)
    ncqDelCachedIndexSTM sto fk

  where f (_,b) = b /= fk

sortIndexes :: NCQState -> NCQState
sortIndexes = over #ncqStateIndex (List.sortOn fst)

ncqFileFastCheck :: MonadUnliftIO m => FilePath -> m ()
ncqFileFastCheck fp = do

  -- debug $ "ncqFileFastCheck" <+> pretty fp

  mmaped <- liftIO $ mmapFileByteString fp Nothing
  let size = BS.length mmaped
  let s = BS.drop (size - 8) mmaped & N.word64

  unless ( BS.length mmaped == fromIntegral s ) do
    throwIO $ NCQFsckIssueExt (FsckInvalidFileSize (fromIntegral s))

ncqStateCapture :: forall m . MonadUnliftIO m
            => NCQStorage3
            -> m FileKey

ncqStateCapture me@NCQStorage3{..} = do
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
                => NCQStorage3
                -> FileKey
                -> m ()
ncqStateDismiss me@NCQStorage3{..} key = atomically do
  useMap <- readTVar ncqStateUse
  case HM.lookup key useMap of
    Nothing -> pure ()
    Just (_, tv) -> do
      modifyTVar tv (max 0 . pred)
      cnt <- readTVar tv
      when (cnt <= 0) do
        modifyTVar ncqStateUse (HM.delete key)

ncqWithState :: forall a m . MonadUnliftIO m
             => NCQStorage3
             -> ( FileKey -> m a  )
             -> m a
ncqWithState sto = bracket (ncqStateCapture sto) (ncqStateDismiss sto)

readStateMay :: forall m . MonadUnliftIO m
             => NCQStorage3
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



