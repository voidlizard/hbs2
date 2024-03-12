module CLI.LWWRef where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Service
import HBS2.Net.Auth.Schema
import HBS2.Peer.Proto.LWWRef

import HBS2.Peer.RPC.API.LWWRef

import CLI.Common
import RPC2()
import PeerLogger hiding (info)

import System.Exit

import Options.Applicative

pLwwRef :: Parser (IO ())
pLwwRef = hsubparser (   command "fetch" (info pLwwRefFetch (progDesc "fetch lwwref"))
                      <> command "get"   (info pLwwRefGet (progDesc "get lwwref"))
                     )
pLwwRefFetch :: Parser (IO ())
pLwwRefFetch = do
  rpc <- pRpcCommon
  ref <- strArgument (metavar "LWWREF")
  pure $ withMyRPC @LWWRefAPI rpc $ \caller -> do
    callService @RpcLWWRefFetch caller ref >>= \case
      Left e  -> err (viaShow e) >> exitFailure
      Right{} -> pure ()

lwwRef :: ReadM (LWWRefKey HBS2Basic)
lwwRef = maybeReader (fromStringMay @(LWWRefKey HBS2Basic))

pLwwRefGet :: Parser (IO ())
pLwwRefGet = do
  rpc <- pRpcCommon
  ref <- strArgument (metavar "LWWREF")
  pure $ withMyRPC @LWWRefAPI rpc $ \caller -> do
    callService @RpcLWWRefGet caller ref >>= \case
      Left e  -> err (viaShow e) >> exitFailure
      Right r -> print $ pretty r


