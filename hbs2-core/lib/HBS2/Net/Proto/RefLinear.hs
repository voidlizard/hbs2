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


data AnnLRef e = AnnLRef (Hash HbSync) (Signed SignaturePresent (MutableRef e 'LinearRef))
    deriving stock (Generic)

instance Serialise (Signature e) => Serialise (AnnLRef e)


-- annLRefProto :: forall e m  . ( MonadIO m
--                                     , EventEmitter e (AnnLRef e) m
--                                     , Response e (AnnLRef e) m
--                                     ) => AnnLRef e -> m ()

refLinearProto :: forall e m  . ( MonadIO m
                                  , Response e (AnnLRef e) m
                                  -- , HasDeferred e (AnnLRef e) m
                                  -- , HasOwnPeer e m
                                  -- , Pretty (Peer e)
                                  )
                 -- => RefLinearI e m
                 -- -> AnnLRef e
                 => AnnLRef e
                 -> m ()
-- refLinearProto adapter (AnnLRef c p) =
refLinearProto = \case

-- * Анонс ссылки (уведомление о новом состоянии без запроса)
    AnnLRef h (LinearMutableRefSigned{}) -> do
        undefined
--     AnnLRef n info -> do
--       that <- thatPeer (Proxy @(AnnLRef e))
--       emit @e AnnLRefInfoKey (AnnLRefEvent that info n)


-- data instance EventKey e (AnnLRef e) =
--   AnnLRefInfoKey
--   deriving stock (Typeable, Eq,Generic)

-- data instance Event e (AnnLRef e) =
--   AnnLRefEvent (Peer e) (AnnLRefInfo e) PeerNonce
--   deriving stock (Typeable)

-- instance Typeable (AnnLRefInfo e) => Hashable (EventKey e (AnnLRef e)) where
--   hashWithSalt salt _ = hashWithSalt salt (someTypeRep p)
--     where
--       p = Proxy @(AnnLRefInfo e)

-- instance EventType ( Event e ( AnnLRef e) ) where
--   isPersistent = True
