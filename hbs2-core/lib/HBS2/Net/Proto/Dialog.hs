{-# Language UndecidableInstances #-}
{-# LANGUAGE StrictData #-}

module HBS2.Net.Proto.Dialog
( module HBS2.Net.Proto.Dialog
, module HBS2.Net.Dialog.Core
, module HBS2.Net.Dialog.Client
) where

import Codec.Serialise (deserialiseOrFail)
import Control.Arrow
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Generics.Product.Fields ()
import Data.Kind
import Lens.Micro.Platform

import HBS2.Data.Types
import HBS2.Net.Dialog.Client
import HBS2.Net.Dialog.Core
import HBS2.Net.Proto
import HBS2.Prelude.Plated hiding (at)

---

newtype DialReq e = DialReq { unDialReq :: Frames }
  deriving stock (Generic)

dialReqDecode :: MonadFail m => ByteString -> m (DialReq e)
dialReqDecode = fmap DialReq . decodeFramesFail

dialReqEncode :: DialReq e -> ByteString
dialReqEncode = \case
    DialReq xs -> encodeFrames xs


newtype DialResp e = DialResp { unDialResp :: Frames }
  deriving stock (Generic)

dialRespDecode :: MonadFail m => ByteString -> m (DialResp e)
dialRespDecode = fmap DialResp . decodeFramesFail

dialRespEncode :: DialResp e -> ByteString
dialRespEncode = \case
    DialResp xs -> encodeFrames xs

---

newtype DialogProtoEnv m e = DialogProtoEnv
  { dialogProtoEnvCallerEnv :: CallerEnv m
  }

newDialogProtoEnv ::
  ( MonadIO m
  , Ord (Peer e)
  ) => m (DialogProtoEnv m' e)
newDialogProtoEnv = do
    dialogProtoEnvCallerEnv <- newCallerEnv
    pure DialogProtoEnv {..}

-- Adapters should share the same env

data DialReqProtoAdapter e (m :: Type -> Type) s = DialReqProtoAdapter
  { dialReqProtoAdapterDApp :: DApp IO
  , dialReqProtoAdapterNT :: Peer e -> forall a . m a -> IO a
  }

newtype DialRespProtoAdapter e (m :: Type -> Type) s = DialRespProtoAdapter
  { dialRespProtoAdapterEnv :: DialogProtoEnv m e
  }

---

-- | Обрабатывается на стороне сервера
dialReqProto :: forall e s m .
      ( MonadIO m
      , Response e (DialReq e) m
      , Request e (DialResp e) m
      -- , Sessions e (KnownPeer e) m
      , e ~ L4Proto
      )
    => DialReqProtoAdapter e m s
    -> DialReq e
    -> m ()
dialReqProto adapter = unDialReq >>> \frames -> do
    peer <- thatPeer @(DialReq e)

    -- let dialReqEnv :: DialogRequestEnv m (Peer e) (Maybe (PeerData e))
    --     dialReqEnv = DialogRequestEnv
    --       { dreqEnvPeer = peer
    --       , dreqEnvGetPeerData = pure Nothing -- undefined -- find (KnownPeerKey peer) id
    --       }

    let replyToPeer :: Frames -> m ()
        replyToPeer = request peer . DialResp @e

    let replyToPeerIO :: Frames -> IO ()
        replyToPeerIO = dialReqProtoAdapterNT adapter peer <$> replyToPeer

    liftIO $ (dialReqProtoAdapterDApp adapter) frames replyToPeerIO


---

-- | Обрабатывает ответы сервера на стороне клиента
dialRespProto :: forall e s m .
      ( MonadIO m
      , Response e (DialResp e) m
      , e ~ L4Proto
      )
    => DialRespProtoAdapter e m s
    -> DialResp e
    -> m ()
dialRespProto DialRespProtoAdapter{..} = unDialResp >>> unFrames >>> \case
  "": _xs -> do
      -- Ответили прямо нам сюда. Нужно как-то отреагировать на xs
      pure ()

  callerIDRaw: xs -> do

      -- Найти в окружении пира, соответствующего callerID, и продолжение для ответа ему
      -- Если нашли, передать xs в это продолжение (переслать ответ обратно спрашивавшему)
      case deserialiseOrFail (BSL.fromStrict callerIDRaw) of
          Left _ ->
              -- Если не нашли, ничего не предпринимать
              -- Клиент отключился
              pure ()

          Right callerID -> do
              let env = dialogProtoEnvCallerEnv dialRespProtoAdapterEnv
              mh <- findCallerHandler env callerID
              forM_ mh \(CallerHandler h) -> h (Frames xs)

      pure ()

  _ -> do
      -- Прислали пустой ответ неизвестно кому? -- Никак не реагировать.
      pure ()

 where
   dialRespProtoProxy = Proxy @(DialResp e)

