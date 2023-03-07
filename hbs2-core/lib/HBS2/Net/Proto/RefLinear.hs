{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Proto.RefLinear where

import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto
import HBS2.Prelude.Plated
import HBS2.Refs.Linear

import Codec.Serialise (serialise, deserialiseOrFail)
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
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

data LRefI e m =
  LRefI
  { getBlockI         :: GetBlockI HbSync m
  , tryUpdateLinearRefI :: TryUpdateLinearRefI e HbSync m
  }

type GetBlockI h m = Hash h -> m (Maybe ByteString)

type TryUpdateLinearRefI e h m = Hash h -> Signed SignatureVerified (MutableRef e 'LinearRef) -> m Bool

refLinearProto :: forall e m  .
    ( MonadIO m
    , Response e (LRef e) m
    , HasCredentials e m
    , Serialise (PubKey 'Sign e)
    , Signatures e
    )
    => LRefI e m
    -> LRef e
    -> m ()
refLinearProto LRefI{..} = \case

    -- Анонс ссылки (уведомление о новом состоянии без запроса)
    AnnLRef h (lref@LinearMutableRefSigned{}) -> do
        creds <- getCredentials @e

        void $ runMaybeT do
            g :: RefGenesis e <- MaybeT $
                (((either (const Nothing) Just . deserialiseOrFail) =<<) <$> getBlockI h)

            lift $ forM_ (verifyLinearMutableRefSigned (refOwner g) lref) \vlref -> do
                r <- tryUpdateLinearRefI h vlref
                when r do
                    -- FIXME: В случае успеха разослать анонс на другие ноды
                    pure ()

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
