module CLI.RefChan where

import HBS2.Prelude.Plated

import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Definition()
import HBS2.Net.Proto.RefChan
import HBS2.Net.Proto.Types
import HBS2.Data.Types.SignedBox
import HBS2.Net.Proto.Service

import HBS2.OrDie

import HBS2.Peer.RPC.API.RefChan

import CLI.Common
import RPC2()

import Options.Applicative
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Lens.Micro.Platform
import Codec.Serialise
import Data.Maybe
import System.Exit

pRefChan :: Parser (IO ())
pRefChan = hsubparser (   command "head" (info pRefChanHead (progDesc "head commands" ))
                       <> command "propose" (info pRefChanPropose (progDesc "post propose transaction"))
                       <> command "notify"  (info pRefChanNotify (progDesc "post notify message"))
                       <> command "fetch"   (info pRefChanFetch (progDesc "fetch and sync refchan value"))
                       <> command "get"     (info pRefChanGet (progDesc "get refchan value"))
                      )


pRefChanHead :: Parser (IO ())
pRefChanHead = hsubparser (   command "gen"  (info pRefChanHeadGen (progDesc "generate head blob"))
                           <> command "dump" (info pRefChanHeadDump (progDesc "dump head blob"))
                           <> command "post" (info pRefChanHeadPost (progDesc "post head transaction"))
                           <> command "fetch" (info pRefChanHeadFetch (progDesc "fetch head from neighbours"))
                           <> command "get"  (info pRefChanHeadGet (progDesc "get head value"))
                          )

pRefChanHeadGen :: Parser (IO ())
pRefChanHeadGen = do
  kr <- strOption (long "keyring" <> short 'k' <> help "owner credentials")
  fn <- optional $ strArgument (metavar "head dsl file")
  pure $ do
    sc <- BS.readFile kr
    creds <- pure (parseCredentials @(Encryption L4Proto) (AsCredFile sc)) `orDie` "bad keyring file"
    s <- maybe1 fn getContents readFile
    hd <- pure (fromStringMay @(RefChanHeadBlock L4Proto) s) `orDie` "can't generate head block"
    let qq = makeSignedBox @L4Proto @(RefChanHeadBlock L4Proto) (view peerSignPk creds) (view peerSignSk creds) hd
    LBS.putStr (serialise qq)

pRefChanHeadDump :: Parser (IO ())
pRefChanHeadDump= do
  fn <- optional $ strArgument (metavar "refchan head blob")
  pure $ do
    lbs <- maybe1 fn LBS.getContents LBS.readFile
    (_, hdblk) <- pure (unboxSignedBox @(RefChanHeadBlock L4Proto) @L4Proto  lbs) `orDie` "can't unbox signed box"
    print $ pretty hdblk


-- FIXME: options-duped-with-peer-main
confOpt :: Parser FilePath
confOpt = strOption ( long "config"  <> short 'c' <> help "config" )

rpcOpt :: Parser String
rpcOpt = strOption ( short 'r' <> long "rpc"
                               <> help "addr:port" )

pRpcCommon :: Parser RPCOpt
pRpcCommon = do
  RPCOpt <$> optional confOpt
         <*> optional rpcOpt

pRefChanHeadPost :: Parser (IO ())
pRefChanHeadPost = do
  opts <- pRpcCommon
  ref <- strArgument (metavar "HEAD-BLOCK-TREE-HASH")
  pure $ withMyRPC @RefChanAPI opts $ \caller -> do
    href <- pure (fromStringMay ref) `orDie` "HEAD-BLOCK-TREE-HASH"
    -- FIXME: proper-error-handling
    void $ callService @RpcRefChanHeadPost caller href

pRefChanHeadFetch :: Parser (IO ())
pRefChanHeadFetch = do
  opts <- pRpcCommon
  ref <- strArgument (metavar "REFCHAH-HEAD-KEY")
  pure $ withMyRPC @RefChanAPI opts $ \caller -> do
    href <- pure (fromStringMay ref) `orDie` "invalid REFCHAN-HEAD-REF"
    void $ callService @RpcRefChanHeadFetch caller href

pRefChanHeadGet :: Parser (IO ())
pRefChanHeadGet = do
  rpc <- pRpcCommon
  ref <- strArgument (metavar "REFCHAH-HEAD-KEY")
  pure $ withMyRPC @RefChanAPI rpc $ \caller -> do
    href <- pure (fromStringMay ref) `orDie` "invalid REFCHAN-HEAD-REF"
    callService @RpcRefChanHeadGet caller href >>= \case
      Left{} -> exitFailure
      Right Nothing -> exitFailure
      Right (Just h) -> print (pretty h) >> exitSuccess

pRefChanPropose :: Parser (IO ())
pRefChanPropose = do
  opts <- pRpcCommon
  kra <- strOption (long "author" <> short 'a' <> help "author credentials")
  fn  <- optional $ strOption (long "file" <> short 'f' <> help "file")
  dry <- optional (flag' True (long "dry" <> short 'n' <> help "only dump transaction")) <&> fromMaybe False
  sref <- strArgument (metavar "REFCHAH-KEY")
  pure $ withMyRPC @RefChanAPI opts $ \caller -> do
    sc <- BS.readFile kra
    puk <- pure (fromStringMay @(RefChanId L4Proto) sref) `orDie` "can't parse refchan/public key"
    creds <- pure (parseCredentials @(Encryption L4Proto) (AsCredFile sc)) `orDie` "bad keyring file"

    lbs <- maybe1 fn LBS.getContents LBS.readFile

    let box = makeSignedBox @L4Proto @BS.ByteString (view peerSignPk creds) (view peerSignSk creds) (LBS.toStrict lbs)

    if dry then do
      LBS.putStr (serialise box)
    else do
      -- FIXME: proper-error-handling
      void $ callService @RpcRefChanPropose caller (puk, box)

pRefChanNotify :: Parser (IO ())
pRefChanNotify = do
  opts <- pRpcCommon
  kra <- strOption (long "author" <> short 'a' <> help "author credentials")
  fn  <- optional $ strOption (long "file" <> short 'f' <> help "file")
  sref <- strArgument (metavar "REFCHAH-REF")
  pure $ withMyRPC @RefChanAPI opts $ \caller -> do
    sc <- BS.readFile kra
    puk <- pure (fromStringMay @(RefChanId L4Proto) sref) `orDie` "can't parse refchan/public key"
    creds <- pure (parseCredentials @(Encryption L4Proto) (AsCredFile sc)) `orDie` "bad keyring file"
    lbs <- maybe1 fn LBS.getContents LBS.readFile
    let box = makeSignedBox @L4Proto @BS.ByteString (view peerSignPk creds) (view peerSignSk creds) (LBS.toStrict lbs)
    void $ callService @RpcRefChanNotify caller (puk, box)

pRefChanGet :: Parser (IO ())
pRefChanGet = do
  opts <- pRpcCommon
  sref <- strArgument (metavar "REFCHAH-KEY")
  pure $ withMyRPC @RefChanAPI opts $ \caller -> do
    puk <- pure (fromStringMay @(RefChanId L4Proto) sref) `orDie` "can't parse refchan/public key"
    callService @RpcRefChanGet caller puk >>= \case
      Left{} -> exitFailure
      Right Nothing -> exitFailure
      Right (Just h) -> print (pretty h) >> exitSuccess

pRefChanFetch :: Parser (IO ())
pRefChanFetch = do
  opts <- pRpcCommon
  ref <- strArgument (metavar "REFCHAH-KEY")
  pure $ withMyRPC @RefChanAPI opts $ \caller -> do
    href <- pure (fromStringMay ref) `orDie` "invalid REFCHAN-HEAD-REF"
    void $ callService @RpcRefChanFetch caller href


