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

import Data.ByteArray.Hash (SipHash(..), SipKey(..))
import Data.ByteArray.Hash qualified as BA

import System.TimeIt

-- import Data.BloomFilter.Easy qualified as B
import Data.BloomFilter qualified as B
import Data.BloomFilter.Easy qualified as B
import Data.BloomFilter.Hash qualified as B
import Control.Concurrent.STM (flushTQueue)
import Control.DeepSeq (deepseq)
import Data.Bits
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Word
import Data.Bits

import Data.Vector.Mutable qualified as V
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)

import Data.Hashable
import Data.Digest.XXHash.FFI

import Streaming.Prelude qualified as S

-- import Control.Concurrent.BloomFilter qualified as U

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



doAlter :: (Bits a1, Integral a2, Num a1) => a2 -> Maybe a1 -> Maybe a1
doAlter j = \case
  Nothing -> Just (setBit 0 (fromIntegral j))
  Just x  -> Just (setBit x (fromIntegral j))
{-# INLINE doAlter #-}

main :: IO ()
main = do
  ls <- LBS8.readFile "input.txt" <&> LBS8.lines

  z <- newIORef 0

  let (sz, hn) = B.suggestSizing 5000000 0.01

  let bloom = B.fromList (\s -> [ xxh32 (LBS.toStrict s) x | x <- [1 .. fromIntegral hn] ]) sz ls
  -- let bloom = B.fromList (\s -> [ xxh32 (LBS.toStrict s) x | x <- [1 .. fromIntegral 2] ]) sz ls
  -- let bloom = B.fromList (\s -> [ fromIntegral (hashWithSalt x ls) | x <- [1 .. hn] ]) sz ls

  print $ B.length bloom

  -- v <- V.new @_ @Word8 (sz `div` 8)

  -- thm <- newIORef (HashMap.empty @Word64 @Word64)
  -- thi <- newIORef (IntMap.empty @Word64)
  -- tvm <- newTVarIO (HashMap.empty @Word64 @Word64)

  -- tq <- newTQueueIO

  -- haha <- for ls $ \s -> do
  --   let hashes = [ xxh32 s x `mod` fromIntegral sz | x <- [1 .. 7] ]
  --   vals <- for hashes $ \h -> do
               -- let (w,b) = h `divMod` 64
               -- pure (w, setBit 0 (fromIntegral b) :: Word64)
  --   pure $ HashMap.fromListWith (.|.) vals

  -- let result = HashMap.unions haha

  -- print $ length result

    -- for_ hashes $ \i -> do
    --   let (w,b) = i `divMod` 64
    --   pure
    --   pure ()

    -- atomically $ mapM_ (writeTQueue tq) hashes

  -- print "FUCK!"

  -- pure ()
  --     modifyIORef thm (HashMap.alter (doAlter j) (fromIntegral i))

  -- w <- readIORef z
  -- print w

  -- let bloom = B.easyList 0.01 ls

  -- print $ B.length bloom

  --dir <- findGitDir "." >>= orThrowUser "not a git dir"

  --flip runContT pure do

  --  o <- gitListAllObjects

  --  ep <- ContT runWithRPC

  --  let sto = StorageClient (rpcStorage ep)

  --  cat <- startGitCatFile

  --  -- h <- gitGetHash "HEAD" >>= orThrowUser "wtf1"
  --  -- rvl <- gitRevList Nothing h
  --  --

  --  items <- for o $ \(a,GitHash b) -> do
  --             pure b

  --  liftIO $ print $ "bloom params" <+> pretty (B.suggestSizing (length items) 0.01)

  --  timeItNamed (show $ "build bloom filter" <+> pretty (length items)) do
  --    let bloom = B.easyList 0.01 items

  --    liftIO $ print $ "bloom filter size" <+> pretty (B.length bloom) <> line
  --                       <> "data size" <+> pretty (LBS.length (serialise items))

  --  timeItNamed  "calc siphashes" do

  --    let w = 67108864
  --    tvm  <- newTVarIO (HashMap.empty @Word64 @Bool)
  --    -- q <- newTQueueIO

  --    for_ items $ \it -> do
  --      for_ (B.cheapHashes 7 it) $ \hx -> do
  --        let k = fromIntegral (hx `mod` w)
  --        atomically $ modifyTVar tvm (HashMap.insert k True)


  --    wtf <- liftIO $ readTVarIO tvm
  --    liftIO $ print $ length wtf

      -- liftIO $ print $ LBS.length $ serialise bloom

    -- liftIO do
    --   allShit' <- for  o $ \r@(o,h) -> runMaybeT do
    --     GitObject t lbs <- toMPlus =<< gitReadFromCatFileBatch cat h
    --     liftIO $ print $ pretty (t, h)
    --     ght <- writeAsMerkle sto lbs

    --     tt <- getBlock sto ght
    --                >>= toMPlus
    --                >>= orThrowUser "FUCK" . (deserialiseOrFail @(MTree [HashRef]))

    --     let txt = fromString (show $ pretty t)
    --     let ann = MTreeAnn (ShortMetadata txt) NullEncryption tt
    --     putBlock sto (serialise ann) >>= toMPlus

    --   let pt = HS.fromList (HashRef <$> catMaybes allShit')
    --             & HS.toList
    --             & toPTree (MaxSize 256) (MaxNum 256)

    --   ht <- makeMerkle 0 pt $ \(_,_,bss) -> do
    --           void $ putBlock sto bss

    --   print $ pretty (HashRef ht)


