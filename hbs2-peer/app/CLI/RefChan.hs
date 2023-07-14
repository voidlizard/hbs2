module CLI.RefChan where

import HBS2.Prelude.Plated

import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Definition()
import HBS2.Net.Proto.RefChan
import HBS2.Net.Proto.Types

import HBS2.OrDie

import RPC

import Options.Applicative
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Lens.Micro.Platform
import Codec.Serialise

pRefChan :: Parser (IO ())
pRefChan = hsubparser (  command "head" (info pRefChanHead (progDesc "head commands" ))
                     )


pRefChanHead :: Parser (IO ())
pRefChanHead = hsubparser (   command "gen"  (info pRefChanHeadGen (progDesc "generate head blob"))
                           <> command "dump" (info pRefChanHeadDump (progDesc "dump head blob"))
                           <> command "post" (info pRefChanHeadPost (progDesc "post head transaction"))
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
  pure $ do
    href <- pure (fromStringMay ref) `orDie` "HEAD-BLOCK-TREE-HASH"
    runRpcCommand opts (REFCHANHEADSEND href)


pRefChanHeadGet :: Parser (IO ())
pRefChanHeadGet = do
  opts <- pRpcCommon
  ref <- strArgument (metavar "REFCHAH-HEAD-REF")
  pure $ do
    href <- pure (fromStringMay ref) `orDie` "invalid REFCHAN-HEAD-REF"
    runRpcCommand opts (REFCHANHEADGET href)

