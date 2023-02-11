module Main where

import HBS2.Prelude
import HBS2.OrDie
import HBS2.Storage (calcChunks)

import Data.Traversable(forM)
import Data.Foldable(for_)
import System.TimeIt
import Crypto.Saltine (sodiumInit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Crypto.Saltine.Core.SecretBox qualified as Symm
import Crypto.Saltine.Core.Box qualified as Asymm
import Crypto.Saltine.Core.Box (publicKey)
import Crypto.Saltine.Core.SecretBox (secretbox)
import System.Environment
import Safe
import Prettyprinter
import Data.Fixed
import System.Directory

defBlockSize = 256*1024


main :: IO ()
main = do
  sodiumInit

  f <- (headMay <$> getArgs) `orDie` "pass file to encrypt"

  key <- Symm.newKey
  nonce <- Symm.newNonce

  kpair <- Asymm.newKeypair
  nonce2 <- Asymm.newNonce

  size <- getFileSize f
  co   <- LBS.readFile f

  let chunks = calcChunks size defBlockSize

  timeItNamed "read and encrypt" do
    sizes <- forM chunks $ \(off,sz) -> do
      let chu = LBS.take sz $ LBS.drop off co
      let box = secretbox key nonce (LBS.toStrict chu)
      pure (BS.length box)

    let s =  (realToFrac (sum sizes) - realToFrac size) / realToFrac size * 100 :: Fixed E6
    print $ pretty (sum sizes) <> comma <+> pretty (show s)
    putStrLn ""

  putStrLn ""


  timeItNamed "read and encrypt asymm" do
    sizes <- forM chunks $ \(off,sz) -> do
      let chu = LBS.take sz $ LBS.drop off co
      box <- Asymm.boxSeal (publicKey kpair) (LBS.toStrict chu)
      pure (BS.length box)

    let s =  (realToFrac (sum sizes) - realToFrac size) / realToFrac size * 100 :: Fixed E6
    print $ pretty (sum sizes) <> comma <+> pretty (show s)
    putStrLn ""

  putStrLn ""


  timeItNamed "just read" do
    sizes <- forM chunks $ \(off,sz) -> do
      let chu = LBS.take sz $ LBS.drop off co
      pure (LBS.length chu)

    let s =  (realToFrac (sum sizes) - realToFrac size) / realToFrac size * 100 :: Fixed E6
    print $ pretty (sum sizes) <> comma <+> pretty (show s)
    putStrLn ""


