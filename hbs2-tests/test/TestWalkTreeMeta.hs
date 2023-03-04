module Main where

import HBS2.Prelude.Plated
import HBS2.Merkle
import HBS2.System.Logger.Simple
import HBS2.OrDie
import HBS2.Data.Types.Refs
import HBS2.Hash

import Text.InterpolatedString.Perl6 (qc)
import Data.Functor
import Data.Function
import Data.Foldable
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.ByteString.Lazy.Char8 (ByteString)
import System.Process.Typed
import Data.Functor
import System.Environment
import System.Exit
import System.IO
import Codec.Serialise


readBlock :: MonadIO m => HashRef -> m (Maybe ByteString)
readBlock hr = do
  (co, out, _) <- readProcess (shell [qc|hbs2 cat --raw {pretty hr}|])
  case co of
    ExitFailure{} -> pure Nothing
    ExitSuccess   -> pure $ Just out

main :: IO ()
main = do

  h <- fromString <$> ( getArgs <&> headMay ) `orDie` "tree hash not set"

  print $ pretty h

  blk <- readBlock h `orDie` "can't read block"

  let ann = deserialiseOrFail @(MTreeAnn [HashRef]) blk & either (error "oopsie") id

  walkMerkleTree (_mtaTree ann) (readBlock . HashRef) $ \(hr :: Either (Hash HbSync) [HashRef]) -> do
    case hr of
      Left hx -> void $ hPrint stderr $ "missed block:" <+> pretty hx
      Right (hrr :: [HashRef]) -> do
        for_ hrr $ \(HashRef hx) -> do
            block <- readBlock (HashRef hx) `orDie` show ("missed block: " <+> pretty hx)
            LBS.putStr block

  exitSuccess
  pure ()

