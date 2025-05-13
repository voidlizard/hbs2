{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language MultiWayIf #-}
{-# Language RecordWildCards #-}
{-# Language ViewPatterns #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Clock
import HBS2.Merkle

import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Storage.Operations.ByteString

import HBS2.System.Logger.Simple.ANSI

import HBS2.Storage.NCQ
import HBS2.Data.Log.Structured.NCQ


import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Script
import Data.Config.Suckless.System

import DBPipe.SQLite hiding (field)

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as TE
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Builder
import Data.Maybe
import Data.Word
import Data.List qualified as List
import Data.Vector qualified as V
import Data.Vector ((!))
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Network.ByteOrder qualified as N
import Data.Coerce
import Data.HashPSQ qualified as HPSQ
import Data.HashSet qualified as HS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.Fixed
import System.Environment
import System.Directory
import System.Posix.Fcntl
import System.Posix.IO
import System.IO.MMap
import System.IO qualified as IO
import System.Exit (exitSuccess, exitFailure)
import System.Random
import Safe
import Lens.Micro.Platform
import Control.Concurrent.STM qualified as STM

import UnliftIO

import Text.InterpolatedString.Perl6 (qc)

import Streaming.Prelude qualified as S
import System.TimeIt

import System.IO.Unsafe (unsafePerformIO)

{- HLINT ignore "Functor law" -}

setupLogger :: MonadIO m => m ()
setupLogger = do
  -- setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStdout . logPrefix ""

flushLoggers :: MonadIO m => m ()
flushLoggers = do
  silence

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE
  setLoggingOff @TRACE


data TCQError =
     TCQAlreadyOpen FilePath
   | TCQGone FilePath
  deriving stock (Show,Typeable)

instance Exception TCQError

newtype TCQ =
  TCQ FilePath
  deriving newtype (Eq,Ord,Show,Typeable)

main :: IO ()
main = do

  instances <- newTVarIO (mempty :: HashMap FilePath (NCQStorage, Async ()))

  tvd <- newTVarIO mempty

  let finalizeStorages = do
        debug "finalize ncq"
        r <- readTVarIO instances <&> HM.toList
        mapM_ ncqStorageStop (fmap (fst.snd) r)
        mapM_ wait (fmap (snd.snd) r)

  let getNCQ (TCQ p) = do
        readTVarIO instances
          <&> HM.lookup p
          <&> fmap fst
          >>= orThrow  (TCQGone p)

  let dict = makeDict @C do

        entry $ bindMatch "--help" $ nil_ \case
          HelpEntryBound what -> helpEntry what
          [StringLike s]      -> helpList False (Just s)
          _                   -> helpList False Nothing

        internalEntries

        entry $ bindMatch "--run" $ \case
          [ StringLike what ] -> liftIO do
            liftIO (readFile what)
              <&> parseTop
              >>= either (error.show) pure
              >>= runEval tvd

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "debug" $ nil_ \case

          [ LitBoolVal False ] -> do
             setLoggingOff @DEBUG

          [ StringLike "off" ] -> do
             setLoggingOff @DEBUG

          _ ->
             setLogging @DEBUG  $ toStderr . logPrefix "[debug] "

        entry $ bindMatch "ncq:open" $ \case
          [ StringLike path ] -> do
             debug $ "ncq:open" <+> pretty path
             ncq <- ncqStorageOpen path
             r   <- async (ncqStorageRun ncq)

             e <- atomically do
                    already <- readTVar instances <&> HM.member path
                    if already then
                      pure $ Left $ TCQAlreadyOpen path
                    else do
                      modifyTVar instances (HM.insert path (ncq,r))
                      pure $ Right ()

             either throwIO pure e

             mkOpaque (TCQ path)

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:poke" $ \case
          [ isOpaqueOf @TCQ -> Just tcq ] -> lift do
            ncq <- getNCQ tcq
            pure $ mkSym "okay"

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:get" $ \case
          [ isOpaqueOf @TCQ -> Just tcq, HashLike hash ] -> lift do
            ncq <- getNCQ tcq
            ncqStorageGet ncq hash >>= maybe (pure nil) mkOpaque

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:put" $ \syn -> do
          (tcq,bs) <- case syn of
            [ isOpaqueOf @TCQ -> Just tcq, isOpaqueOf @ByteString -> Just bs ] -> lift do
              pure (tcq, LBS.fromStrict bs)

            [ isOpaqueOf @TCQ -> Just tcq, TextLike s ] -> lift do
              pure (tcq, LBS.fromStrict (TE.encodeUtf8 s))

            e -> throwIO $ BadFormException @C (mkList e)

          lift do
            ncq <- getNCQ tcq
            r <- ncqStoragePut ncq bs
            pure $ maybe nil (mkSym . show . pretty) r


  setupLogger

  argz <- liftIO getArgs

  forms <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  atomically $ writeTVar tvd dict

  (runEval tvd forms >>= eatNil display)
    `finally` (finalizeStorages >> flushLoggers)



