{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Proto.RefLinear where

-- import HBS2.Actors.Peer
import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto
import HBS2.Prelude.Plated

import Codec.Serialise()
import Data.ByteString.Lazy (ByteString)
import Data.Hashable
import Data.Word
import Lens.Micro.Platform
import Type.Reflection (someTypeRep)


newtype AnnLRefNonce = AnnLRefNonce Word64
                         deriving newtype (Num,Enum,Real,Integral)
                         deriving stock (Ord,Eq,Generic,Show)

instance Serialise AnnLRefNonce


data LRef e
  = AnnLRef (Hash HbSync) (Signed SignaturePresent (MutableRef e 'LinearRef))
    deriving stock (Generic)

instance Serialise (Signature e) => Serialise (LRef e)

data AnnLRefI e m =
  AnnLRefI
  { blkSize         :: GetBlockSize HbSync m
  }

refLinearProto :: forall e m  . ( MonadIO m
                                  , Response e (LRef e) m
--                                     , EventEmitter e (LRef e) m
--                                     , Response e (LRef e) m
                                  -- , HasDeferred e (LRef e) m
                                  -- , HasOwnPeer e m
                                  -- , Pretty (Peer e)
                                  )
                 -- => RefLinearI e m
                 => LRef e
                 -> m ()
refLinearProto = \case

-- Анонс ссылки (уведомление о новом состоянии без запроса)
    AnnLRef h (LinearMutableRefSigned{}) -> do

        -- g :: RefGenesis e <- (((either (const Nothing) Just . deserialiseOrFail) =<<)
        --     <$> getBlock ss chh)
        -- Проверить подпись ссылки
        -- Достать наше текущее значение ссылки, сравнить счётчик
        -- Если новое значение больше, обновить его
        -- И разослать анонс на другие ноды
        undefined
        --
--     AnnLRef n info -> do
--       that <- thatPeer (Proxy @(AnnLRef e))
--       emit @e AnnLRefInfoKey (AnnLRefEvent that info n)


-- data instance EventKey e (LRef e) =
--   AnnLRefInfoKey
--   deriving stock (Typeable, Eq,Generic)

-- data instance Event e (LRef e) =
--   AnnLRefEvent (Peer e) (AnnLRefInfo e) PeerNonce
--   deriving stock (Typeable)

-- instance Typeable (AnnLRefInfo e) => Hashable (EventKey e (LRef e)) where
--   hashWithSalt salt _ = hashWithSalt salt (someTypeRep p)
--     where
--       p = Proxy @(AnnLRefInfo e)

-- instance EventType ( Event e ( LRef e) ) where
--   isPersistent = True
