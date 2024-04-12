module Main where

import Codec.Serialise (serialise)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS
import HBS2.Base58 (fromBase58)
import HBS2.Net.Auth.Credentials
import HBS2.Peer.Proto hiding (request)
import HBS2.OrDie
import HBS2.Prelude
import Lens.Micro.Platform
import Network.HTTP.Simple
import Network.HTTP.Types
import Options.Applicative

data Options = Options
  { credentialsFile :: FilePath,
    tx :: String
  }

parseOptions :: Parser Options
parseOptions =
  Options
    <$> strOption
      ( long "keyring"
          <> short 'k'
          <> help "reflog keyring"
          <> metavar "FILE"
      )
    <*> strOption
      ( long "transaction"
          <> help "transaction in Base58 format"
          <> short 't'
          <> metavar "TRANSACTION"
      )

main :: IO ()
main = do
  options <-
    execParser $
      info
        (parseOptions <**> helper)
        ( fullDesc
            <> progDesc
              "Program that takes keyfile and some base58 encoded message, \
              \creates signed transaction and posts it to the reflog."
            <> header "Raw tx test"
        )
  krData <- BS.readFile $ credentialsFile options
  creds <- pure (parseCredentials @'HBS2Basic (AsCredFile krData)) `orDie` "bad keyring file"
  let pubk = view peerSignPk creds
  let privk = view peerSignSk creds
  bs <- pure (fromBase58 $ BS8.pack $ tx options) `orDie` "transaction is not in Base58 format"
  msg <- makeRefLogUpdate @L4Proto pubk privk bs <&> serialise

  req <- parseRequest "http://localhost:5001/reflog"

  let request =
        setRequestMethod "POST" $
          setRequestHeader "Content-Type" ["application/octet-stream"] $
            setRequestBodyLBS msg req

  resp <- httpLBS request

  case statusCode (getResponseStatus resp) of
    200 -> do
      let r = LBS.unpack $ getResponseBody resp
      print r
    s -> print $ "error: status " <> show s
