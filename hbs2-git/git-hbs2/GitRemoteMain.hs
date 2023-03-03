module Main where

import HBS2.Prelude
import HBS2.Data.Types.Refs
import HBS2.Base58

import HBS2.System.Logger.Simple

import HBS2Git.Types
import HBS2Git.App

import Data.Attoparsec.ByteString.Char8 qualified as Atto8
import Data.Attoparsec.Text
import Data.Attoparsec.Text qualified as Atto
import System.IO
import System.Exit
import System.Environment
import Data.Text (Text)
import Data.Text qualified as Text
import Data.ByteString.Char8 qualified as BS
import System.Posix.Signals

parseRepoURL :: String -> Maybe HashRef
parseRepoURL url' = either (const Nothing) Just (parseOnly p url)
  where
    url = Text.pack url'
    p = do
      _ <- string "hbs2://"
      topic'  <- Atto.manyTill' anyChar endOfInput
      let topic = BS.unpack <$> fromBase58 (BS.pack topic')
      maybe (fail "invalid url") (pure . fromString) topic


loop :: MonadIO m => App m ()
loop = do
  debug "PREVED"
  pure ()

main = do

  args <- getArgs
  remote <- case args of
              [_, x] -> case parseRepoURL x of
                          Just y -> pure y
                          _      -> exitFailure
              _      -> exitFailure

  hSetBuffering stderr LineBuffering
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering

  -- liftIO $ installHandler sigPIPE (Catch handlePipe) Nothing
  liftIO $ installHandler sigPIPE Ignore Nothing

  runApp WithLog loop


