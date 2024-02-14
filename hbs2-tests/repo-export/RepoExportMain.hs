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
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy (ByteString)
import Codec.Serialise
import Data.Maybe
import Data.HashSet qualified as HS
import Data.Vector qualified as V
import Data.Vector ((!))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet

import Text.InterpolatedString.Perl6 (qc)

import Options.Generic

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


data CLI = CLI { nThreads :: Maybe Int }
  deriving (Generic)

instance ParseRecord CLI


main :: IO ()
main = do
  bs <- gitRunCommand [qc|git cat-file --batch-check='%(objectname)'  --batch-all-object --unordered --buffer|]
          >>= orThrowUser "oopsie"
  let ls = LBS8.lines bs -- & HashSet.fromList
  print $ length ls

main1 :: IO ()
main1 = do
  dir <- findGitDir "." >>= orThrowUser "not a git dir"


  let hrs = [ "A9Y5k28STYMg2XGUA5xwpAU3CcQg3Fh5j56E4v1QYV7A"
            , "BZuMNqPy1vpxev4H5yKJ4TjTHrZ5HAqPzxDM1ZN74XY2"
            , "Bs2jEFJTSQnY7z5nActjBBanCEWSYnbzUzC41xhhHwtX"
            , "2rZxtNqi8haDEhQkVd2v1ddSfDub9bMH4BB9tgwqvxCF"
            , "Fe1cLjj9BPqHcLowTNwZvHkwT7tAL7dywBocdN3VYeMn"
            ] :: [HashRef]

  let pt = toPTree (MaxSize 256) (MaxNum 256) hrs

  root <- makeMerkle 0 pt $ \(hx,_,bss) -> do
            liftIO $ print $ "block:" <+> pretty hx

  print $ pretty root
  error "stop"

  (CLI mn) <- getRecord  "export git repo"

  let n = fromMaybe 1 mn

  flip runContT pure do

    o <- gitListAllObjects

    ep <- ContT runWithRPC

    let sto = StorageClient (rpcStorage ep)

    ou <- newTQueueIO

    qqs <- V.fromList <$> replicateM n newTQueueIO

    w <- liftIO $ async do
           for_ (zip [0..] o) $ \(i,x) -> do
            let j = i `mod` V.length qqs
            atomically $ writeTQueue (qqs ! j) (Just x)
           for_ qqs $ \q -> do
             atomically $ writeTQueue q Nothing

    ws <- liftIO $ for (V.toList qqs) $ \q -> async do
            cat <- startGitCatFile
            fix \next -> do
              e <- atomically $ readTQueue q
              case e of
                Nothing -> pure ()
                Just (_,h)  -> do
                  void $ runMaybeT do
                    GitObject t lbs <- toMPlus =<< gitReadFromCatFileBatch cat h
                    atomically $ writeTQueue ou (t, h, LBS.length lbs)
                  next

    wou <- liftIO $ async do
             fix \next -> do
               r <- atomically $ readTQueue ou
               print $ pretty r
               next

    mapM_ wait (w:ws)

    cancel wou

    -- for_ [q1, q2] -> do

    -- cat2 <- startGitCatFile

    -- h <- gitGetHash "HEAD" >>= orThrowUser "wtf1"
    -- rvl <- gitRevList Nothing h

    -- liftIO do
    --   for_  o $ \r@(o,h) -> runMaybeT do
    --     pure ()
        -- GitObject t lbs <- toMPlus =<< gitReadFromCatFileBatch cat h
        -- liftIO $ print $ pretty (t, h, LBS.length lbs)
        -- ght <- writeAsMerkle sto lbs

        -- tt <- getBlock sto ght
        --            >>= toMPlus
        --            >>= orThrowUser "FUCK" . (deserialiseOrFail @(MTree [HashRef]))

        -- let txt = fromString (show $ pretty t)
        -- let ann = MTreeAnn (ShortMetadata txt) NullEncryption tt
        -- putBlock sto (serialise ann) >>= toMPlus

      -- let pt = HS.fromList (HashRef <$> catMaybes allShit')
        --         & HS.toList
        --         & toPTree (MaxSize 256) (MaxNum 256)

      -- ht <- makeMerkle 0 pt $ \(_,_,bss) -> do
        --       void $ putBlock sto bss

      -- print $ pretty (HashRef ht)


