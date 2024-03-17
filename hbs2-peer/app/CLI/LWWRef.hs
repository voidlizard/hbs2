module CLI.LWWRef where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Net.Proto.Service
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.SignedBox
import HBS2.Net.Auth.Schema
import HBS2.Peer.Proto.LWWRef

import HBS2.Peer.RPC.API.LWWRef
import HBS2.KeyMan.Keys.Direct

import CLI.Common
import RPC2()
import PeerLogger hiding (info)

import System.Exit

import Options.Applicative
import Data.Word
import Lens.Micro.Platform

pLwwRef :: Parser (IO ())
pLwwRef = hsubparser (   command "fetch"  (info pLwwRefFetch  (progDesc "fetch lwwref"))
                      <> command "get"    (info pLwwRefGet    (progDesc "get lwwref"))
                      <> command "update" (info pLwwRefUpdate (progDesc "update lwwref"))
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

pLwwRefUpdate :: Parser (IO ())
pLwwRefUpdate = do
  rpc <- pRpcCommon
  puk <- argument pPubKey (metavar "LWWREF")
  seq' <- optional $ option @Word64 auto (short 's' <> long "seq" <> help "seqno" <>metavar "SEQ")
  val <- option (maybeReader fromStringMay) (short 'v' <> long "value" <> help "value" <> metavar "VALUE")
  pure $ withMyRPC @LWWRefAPI rpc $ \caller -> do


    (sk,pk) <- liftIO $ runKeymanClient do
                 creds  <- loadCredentials puk >>= orThrowUser "can't load credentials"
                 pure ( view peerSignSk  creds, view peerSignPk creds )

    seq <- case seq' of
             Just v -> pure v
             Nothing -> do
               let ref = LWWRefKey puk
               callService @RpcLWWRefGet caller ref >>= \case
                 Left e  -> err (viaShow e) >> exitFailure
                 Right Nothing -> err ("not found value for" <+> pretty ref) >> exitFailure
                 Right (Just r) -> pure $ succ (lwwSeq r)

    let box =  makeSignedBox @L4Proto pk sk (LWWRef @L4Proto seq val Nothing)
    callService @RpcLWWRefUpdate caller box >>= \case
      Left e  -> err (viaShow e) >> exitFailure
      Right r -> print $ pretty r

