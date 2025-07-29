{-# Language ViewPatterns #-}
module HBS2.Storage.NCQ3.Internal.State where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Files

import Data.Config.Suckless.Script

import Data.Generics.Product
import Data.List qualified as List
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Data.HashSet qualified as HS
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
          readTVar ncqState

  unless (s1 == s0) do
    snkFile <- ncqGetNewFileKey ncq StateFile <&> ncqGetFileName ncq . StateFile
    liftIO $ withBinaryFileDurableAtomic snkFile WriteMode $ \fh -> do
      IO.hPrint fh (pretty s1)

ncqStateAddDataFile :: FileKey -> StateOP ()
ncqStateAddDataFile fk = do
  NCQStorage3{..} <- ask
  StateOP $ lift do
    modifyTVar ncqState (over (field @"ncqStateFiles") (HS.insert fk))

ncqStateAddFact :: Fact -> StateOP ()
ncqStateAddFact fact = do
  NCQStorage3{..} <- ask
  StateOP $ lift do
    modifyTVar ncqState (over (field @"ncqStateFacts") (Set.insert fact))

ncqStateDelFact :: Fact -> StateOP ()
ncqStateDelFact fact = do
  NCQStorage3{..} <- ask
  StateOP $ lift do
    modifyTVar ncqState (over (field @"ncqStateFacts") (Set.delete fact))

ncqStateAddIndexFile :: POSIXTime
                     -> FileKey
                     -> StateOP ()

ncqStateAddIndexFile ts fk  = do
  NCQStorage3{..} <- ask
  StateOP $ lift $ modifyTVar' ncqState sortIndexes

sortIndexes :: NCQState -> NCQState
sortIndexes = over (field @"ncqStateIndex") (List.sortOn fst)

ncqFileFastCheck :: MonadUnliftIO m => FilePath -> m ()
ncqFileFastCheck fp = do

  -- debug $ "ncqFileFastCheck" <+> pretty fp

  mmaped <- liftIO $ mmapFileByteString fp Nothing
  let size = BS.length mmaped
  let s = BS.drop (size - 8) mmaped & N.word64

  unless ( BS.length mmaped == fromIntegral s ) do
    throwIO $ NCQFsckIssueExt (FsckInvalidFileSize (fromIntegral s))


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
      (acc, e : ss)  -> liftIO (print (pretty e)) >> next (acc <> entryOf e, ss)

  where

    entryOf = \case
       ListVal [SymbolVal "i",  LitIntVal n, LitIntVal ts] ->
         ncqState0 { ncqStateIndex = [ (fromIntegral ts, fromIntegral n) ] }

       ListVal [SymbolVal "f",  LitIntVal n] ->
         ncqState0 { ncqStateFiles = HS.singleton (fromIntegral n) }

       ListVal [SymbolVal "fi", LitIntVal a, LitIntVal b] ->
         ncqState0 { ncqStateFacts = Set.singleton (FI (DataFile (fromIntegral a)) (IndexFile (fromIntegral b))) }

       ListVal [SymbolVal "fp", LitIntVal a, LitIntVal s] ->
         ncqState0 { ncqStateFacts = Set.singleton (P (PData (DataFile $ fromIntegral a) (fromIntegral s))) }

       ListVal [SymbolVal "n", LitIntVal a] ->
         ncqState0 { ncqStateFileSeq = fromIntegral a }

       _ -> ncqState0



