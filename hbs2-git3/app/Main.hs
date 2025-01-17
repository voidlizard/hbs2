{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language MultiWayIf #-}
{-# Language FunctionalDependencies #-}
{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
{-# Language RecordWildCards #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language OverloadedLabels #-}
module Main where

import HBS2.Git3.Prelude
import HBS2.Git3.State.Index
import HBS2.Git3.Git.Pack
import HBS2.Git3.Run

import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Storage.Operations.Missed

-- move to Data.Config.Suckless.Script.Filea sepatate library
import HBS2.Data.Log.Structured

import HBS2.CLI.Run.Internal.Merkle (getTreeContents)
import HBS2.CLI.Run.RefLog (getCredentialsForReflog,mkRefLogUpdateFrom)

import HBS2.System.Dir

import HBS2.Git3.Types
import HBS2.Git3.Config.Local
import HBS2.Git3.Git
import HBS2.Git3.Export
import HBS2.Git3.Import
import HBS2.Git3.State.RefLog

import Data.Config.Suckless.Script
import Data.Config.Suckless.Script.File

import Codec.Compression.Zstd.Streaming qualified as ZstdS
import Codec.Compression.Zstd.Streaming (Result(..))
import Codec.Compression.Zstd.Lazy qualified as ZstdL

import Codec.Compression.Zlib qualified as Zlib

import Data.HashPSQ qualified as HPSQ
import Data.HashPSQ (HashPSQ)

import Data.Maybe
import Data.List qualified as L
import Data.List (sortBy)
import Data.List.Split (chunksOf)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy ( ByteString )
import Data.ByteString.Builder as Builder
import Network.ByteOrder qualified as N
import Text.InterpolatedString.Perl6 (qc)
import Data.Set qualified as Set
import Data.HashSet qualified as HS
import Data.HashSet (HashSet(..))
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict (HashMap(..))
import Data.Word
import Data.Fixed
import Data.Either
import Data.Ord (comparing)
import Data.Generics.Labels
import Data.Generics.Product
import Lens.Micro.Platform

import Streaming.Prelude qualified as S

import System.Exit qualified as Q
import System.Environment qualified as E
import System.Process.Typed
import Control.Monad.State qualified as State
import Control.Monad.Trans.Writer.CPS qualified as Writer
import Control.Concurrent.STM qualified as STM
import System.Directory (setCurrentDirectory)
import System.Random hiding (next)
import System.IO.MMap (mmapFileByteString)
import System.IO qualified as IO
import System.IO (hPrint,hPutStrLn,hPutStr)
import System.IO.Temp as Temp
import System.TimeIt

import Data.Vector qualified as Vector
import Data.Vector.Algorithms.Search qualified as MV

import UnliftIO.Concurrent
import UnliftIO.IO.File qualified as UIO

import Control.Monad.ST
import Data.BloomFilter qualified as Bloom
import Data.BloomFilter.Mutable qualified as MBloom

import Crypto.Hash qualified as C

{- HLINT ignore "Functor law" -}
{- HLINT ignore "Eta reduce" -}



readIndexFromFile :: forall m . MonadIO m
                  => FilePath
                  -> m (HashSet GitHash)
readIndexFromFile fname = do

    bs <- liftIO $ LBS.readFile fname

    r <- S.toList_ $ runConsumeLBS bs $ flip fix 0 \go n -> do
      done <- noBytesLeft
      if done then pure ()
        else do
          _  <- readBytesMaybe 4
                  >>= orThrow SomeReadLogError
                  <&> fromIntegral . N.word32 . LBS.toStrict

          hash   <- readBytesMaybe 20
                       >>= orThrow SomeReadLogError
                       <&> GitHash . LBS.toStrict

          lift (S.yield hash)
          go (succ n)

    pure $ HS.fromList r


-- debugPrefix :: LoggerEntry -> LoggerEntry
debugPrefix = toStderr . logPrefix "[debug] "

setupLogger :: MonadIO m => m ()
setupLogger = do
  -- setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStdout . logPrefix ""
  pure ()

flushLoggers :: MonadIO m => m ()
flushLoggers = do
  silence

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE

main :: IO ()
main = flip runContT pure do

  setupLogger

  ContT $ bracket none $ const do
    silence

  argz <- liftIO $ E.getArgs
  cli <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  env <- nullGit3Env

  void $ lift $ withGit3Env env do
    conf <- readLocalConf
    let dict = theDict
    recover $ setupLogger >> run dict (conf <> cli)
      `finally` silence

