{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.OrDie
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Net.Auth.Credentials
import HBS2.Polling
import HBS2.Misc.PrettyStuff
import HBS2.System.Dir
import HBS2.System.Logger.Simple.ANSI hiding (info)
import HBS2.Net.Messaging.Unix

import HBS2.Net.Proto.Notify
import HBS2.Net.Proto.Service
import HBS2.Peer.Proto.RefChan
import HBS2.Peer.Notify
import HBS2.Peer.RPC.API.Peer
-- import HBS2.Peer.RPC.API.RefLog
-- import HBS2.Peer.RPC.API.LWWRef
-- import HBS2.Peer.RPC.API.Storage
-- import HBS2.Peer.RPC.Client.StorageClient

import HBS2.Peer.CLI.Detect
import HBS2.Peer.Proto.RefLog

import Data.Config.Suckless
import Data.Config.Suckless.Script

import HBS2.System.Logger.Simple.ANSI

import Data.Maybe
import System.Environment
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Lens.Micro.Platform


data MyEnv =
  MyEnv
  { _myRefChan :: Maybe (RefChanId L4Proto)
  }

makeLenses ''MyEnv


main :: IO ()
main = do
  work
    `finally` do
      setLoggingOff @DEBUG
      setLoggingOff @TRACE
      setLoggingOff @NOTICE
      setLoggingOff @ERROR
      setLoggingOff @WARN


respawned :: MonadUnliftIO m => m a1 -> m a2
respawned action = do
  fix \next -> do
    try @_  @SomeException action
    warn $ red "respawning..."
    pause @'Seconds 2
    next

work :: IO ()
work = do

  setLogging @WARN   (toStderr . logPrefix "[warn] ")
  setLogging @ERROR  (toStderr . logPrefix "[error] ")
  setLogging @DEBUG  (toStderr . logPrefix "[debug] ")
  setLogging @TRACE  (toStderr . logPrefix "[trace] ")
  setLogging @NOTICE toStdout

  tv <- newTVarIO (MyEnv mzero)

  let dict = makeDict @C do

          entry $ bindMatch "--refchan" $ nil_ \case
            [SignPubKeyLike rchan] -> do
              atomically $ modifyTVar tv (set myRefChan (Just rchan))

            _ -> throwIO $ BadFormException @C nil


  argz <- getArgs
  forms <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure


  void $ run dict forms

  rchan <- readTVarIO tv <&> view myRefChan
              >>= orThrowUser "refchan not set"

  notice $ yellow "refchan set" <+> pretty (AsBase58 rchan)

  respawned $ flip runContT pure do

    -- NOTE: dont-retry
    --   MUDontRetry -- ВАЖНО
    --   что бы UnixClient не пытался перезапустить транспорт
    --   т.к кажется в этом случае некорректно будут работать
    --   нотификации ( не будет создан новый сокет, а будут
    --   идти в старый. возможно это надо пофиксить, но пока
    --   непонятно, как )
    --
    --   Короче, запретить ему повторный коннект, ловить
    --   исключения и выход из клиентов и всё по новой.
    --
    --   так лучше
    --
    let o = [MUWatchdog 10,MUDontRetry]

    soname <- detectRPC
                >>= orThrowUser "hbs2-peer not found"

    client <- liftIO $ race (pause @'Seconds 1) (newMessagingUnixOpts o False 1.0 soname)
                >>= orThrowUser ("can't connect to" <+> pretty soname)

    notif <- ContT $ withAsync (runMessagingUnix client)

    sink <- newNotifySink

    p1 <- ContT $ withAsync $ flip runReaderT client $ do
        runProto @UNIX
          [ makeResponse (makeNotifyClient @(RefChanEvents L4Proto) sink)
          ]

    psink <- ContT $ withAsync $ flip runReaderT client $ do
              debug $ red "notify restarted!"
              runNotifyWorkerClient sink

    -- NOTE: wrap-to-thread-to-kill
    --   важно обернуть это в поток, что бы
    --   можно было пристрелить, если кто-то еще
    --   отгниёт из других потоков
    --   иначе будет висеть вечно
    psink2 <- ContT $ withAsync do
      runNotifySink sink (RefChanNotifyKey rchan) $ \case
        RefChanUpdated r v -> do
          notice $ red "refchan updated" <+> pretty (AsBase58 r) <+> pretty v

        _ -> do
          notice $ "some other refchan event happened"

    void $ waitAnyCatchCancel [notif,p1,psink,psink2]


