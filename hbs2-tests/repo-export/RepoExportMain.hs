module Main where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.Merkle
import HBS2.Clock
import HBS2.OrDie
import HBS2Git.Config
import HBS2.Git.Local.CLI
import HBS2Git.App (detectRPC)
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.System.Logger.Simple
import HBS2.Storage.Operations.ByteString
import HBS2.Storage
import HBS2.Git.Types

import UnliftIO
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy (ByteString)
import Codec.Serialise
import Data.Maybe
import Data.HashSet qualified as HS

data RPCEndpoints =
  RPCEndpoints
  { rpcPeer    :: ServiceCaller PeerAPI UNIX
  , rpcStorage :: ServiceCaller StorageAPI UNIX
  , rpcRefLog  :: ServiceCaller RefLogAPI UNIX
  }

runWithRPC :: forall m . (MonadUnliftIO m, MonadThrow m) => (RPCEndpoints -> m ()) -> m ()
runWithRPC action = do

  let soname' = Just "/tmp/hbs2-rpc.socket"

  soname <- race ( pause @'Seconds 1) (maybe (detectRPC True) pure soname') `orDie` "hbs2-peer rpc timeout!"

  client <- race ( pause @'Seconds 1) (newMessagingUnix False 1.0 soname) `orDie` "hbs2-peer rpc timeout!"

  rpc <- RPCEndpoints <$> makeServiceCaller (fromString soname)
                      <*> makeServiceCaller (fromString soname)
                      <*> makeServiceCaller (fromString soname)

  messaging <- async $ runMessagingUnix client
  link messaging

  let endpoints = [ Endpoint @UNIX  (rpcPeer rpc)
                  , Endpoint @UNIX  (rpcStorage rpc)
                  , Endpoint @UNIX  (rpcRefLog rpc)
                  ]

  c1 <- async $ liftIO $ runReaderT (runServiceClientMulti endpoints) client
  link c1

  test <- race ( pause @'Seconds 1) (callService @RpcPoke (rpcPeer rpc) ()) `orDie` "hbs2-peer rpc timeout!"

  void $ pure test `orDie` "hbs2-peer rpc error!"

  debug $ "hbs2-peer RPC ok" <+> pretty soname

  action rpc

  cancel messaging

  void $ waitAnyCatchCancel [messaging, c1]


main :: IO ()
main = do
  dir <- findGitDir "." >>= orThrowUser "not a git dir"

  flip runContT pure do

    o <- gitListAllObjects

    ep <- ContT runWithRPC

    let sto = StorageClient (rpcStorage ep)

    cat <- startGitCatFile

    -- h <- gitGetHash "HEAD" >>= orThrowUser "wtf1"
    -- rvl <- gitRevList Nothing h

    liftIO do
      allShit' <- for  o $ \r@(o,h) -> runMaybeT do
        GitObject t lbs <- toMPlus =<< gitReadFromCatFileBatch cat h
        liftIO $ print $ pretty (t, h)
        ght <- writeAsMerkle sto lbs

        tt <- getBlock sto ght
                   >>= toMPlus
                   >>= orThrowUser "FUCK" . (deserialiseOrFail @(MTree [HashRef]))

        let txt = fromString (show $ pretty t)
        let ann = MTreeAnn (ShortMetadata txt) NullEncryption tt
        putBlock sto (serialise ann) >>= toMPlus

      let pt = HS.fromList (HashRef <$> catMaybes allShit')
                & HS.toList
                & toPTree (MaxSize 256) (MaxNum 256)

      ht <- makeMerkle 0 pt $ \(_,_,bss) -> do
              void $ putBlock sto bss

      print $ pretty (HashRef ht)


