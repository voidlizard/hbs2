{-# Language TypeOperators #-}
module Data.Config.Suckless.Almost.RPC where

import Data.Config.Suckless

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 as LBS8
import Data.Function
import Data.Text.Encoding.Error qualified as TE
import Data.Text.Encoding qualified as TE
import Data.Text qualified as T
import Data.Text (Text)
import Data.Typeable
import Prettyprinter
import System.Process.Typed

import System.IO

data CallProcException =
  CallProcException ExitCode
  deriving (Show,Typeable)

instance Exception CallProcException

-- FIXME: to-suckless-script
callProc :: forall m . MonadIO m
         => FilePath
         -> [String]
         -> [Syntax C]
         -> m [Syntax C]

callProc name params syn = do
  let input = fmap (LBS.fromStrict . TE.encodeUtf8 . T.pack  . show . pretty) syn
                & LBS8.unlines
                & byteStringInput

  -- let what = proc name params & setStderr closed & setStdin input
  let what = proc name params & setStdin input
  (code, i, o) <- readProcess what

  unless (code == ExitSuccess) do
    liftIO $ hPrint stderr ( pretty $ LBS8.unpack o )
    liftIO $ throwIO (CallProcException code)

  let s = TE.decodeUtf8With TE.lenientDecode (LBS.toStrict i)

  parseTop s & either (liftIO . throwIO) pure


-- FIXME: to-suckless-script
callProcRaw :: forall m . MonadIO m
         => FilePath
         -> [String]
         -> m Text

callProcRaw name params  = do
  -- let input = fmap (LBS.fromStrict . TE.encodeUtf8 . T.pack  . show . pretty) syn
  --               & LBS8.unlines
  --               & byteStringInput

  -- let what = proc name params & setStderr closed & setStdin input
  let what = proc name params & setStdin closed
  (code, i, o) <- readProcess what

  unless (code == ExitSuccess) do
    liftIO $ hPrint stderr ( pretty $ LBS8.unpack o )
    liftIO $ throwIO (CallProcException code)

  let s = TE.decodeUtf8With TE.lenientDecode (LBS.toStrict i)

  pure s


runProcAttached :: forall m . MonadIO m
                => FilePath
                -> [String]
                -> m ExitCode
runProcAttached cmd args = do

  let processConfig = setStdout inherit
                    $ setStderr inherit
                    $ proc cmd args

  runProcess processConfig

pipeProcText :: forall m . MonadIO m
         => FilePath
         -> [String]
         -> Text
         -> m Text

pipeProcText name params input' = do

  let input = LBS.fromStrict (TE.encodeUtf8 input')
                & byteStringInput


  let what = proc name params & setStderr closed & setStdin input
  (code, i, o) <- readProcess what

  unless (code == ExitSuccess) do
    liftIO $ hPrint stderr ( pretty $ LBS8.unpack o )
    liftIO $ throwIO (CallProcException code)

  pure $ TE.decodeUtf8With TE.lenientDecode (LBS.toStrict i)


