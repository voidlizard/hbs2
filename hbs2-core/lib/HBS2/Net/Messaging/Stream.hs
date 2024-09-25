module HBS2.Net.Messaging.Stream where

import HBS2.Prelude.Plated

import Control.Exception (Exception,throwIO)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Network.Socket hiding (listen,connect)
import Streaming.Prelude qualified as S
import Data.ByteString qualified as BS
import Network.Simple.TCP

data SocketClosedException =
    SocketClosedException
    deriving stock (Show, Typeable)

instance Exception SocketClosedException


-- FIXME: why-streaming-then?
--  Ну и зачем тут вообще стриминг,
--  если чтение всё равно руками написал?
--  Если fromChunks - O(n), и reverse O(n)
--  то мы все равно пройдем все чанки, на
--  кой чёрт тогда вообще стриминг? бред
--  какой-то.
readFromSocket :: forall m . MonadIO m
               => Socket
               -> Int
               -> m ByteString

readFromSocket sock size = LBS.fromChunks <$> (go size & S.toList_)
  where
    go 0 = pure ()
    go n = do
      r <- liftIO $ recv sock n
      maybe1 r eos $ \bs -> do
        let nread = BS.length bs
        S.yield bs
        go (max 0 (n - nread))

    eos = do
      liftIO $ throwIO SocketClosedException

readFromSocket1 :: forall m . MonadIO m
               => Socket
               -> Int
               -> m ByteString

readFromSocket1 sock size = LBS.fromChunks <$> (go size & S.toList_)
  where
    go 0 = pure ()
    go n = do
      r <- liftIO $ recv sock (min 65536 n)
      maybe1 r eos $ \bs -> do
        let nread = BS.length bs
        S.yield bs
        go (max 0 (n - nread))

    eos = do
      liftIO $ throwIO SocketClosedException
