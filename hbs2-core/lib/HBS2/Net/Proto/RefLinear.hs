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


data LRefProto e
  = AnnLRef (Hash HbSync) (Signed SignaturePresent (MutableRef e 'LinearRef))
  | LRefGetVal (Hash HbSync)
    deriving stock (Generic)

instance Serialise (Signature e) => Serialise (LRefProto e)

data LRefI e m =
  LRefI
  { getBlockI           :: GetBlockI HbSync m
  , tryUpdateLinearRefI :: TryUpdateLinearRefI e HbSync m
  , getLRefValI         :: GetLRefValI e HbSync m
  , announceLRefValI    :: AnnounceLRefValI e HbSync m
  }

type GetBlockI h m = Hash h -> m (Maybe ByteString)

type TryUpdateLinearRefI e h m = Hash h -> Signed SignatureVerified (MutableRef e 'LinearRef) -> m Bool

type GetLRefValI e h m = Hash h -> m (Maybe (Signed SignaturePresent (MutableRef e 'LinearRef)))

type AnnounceLRefValI e h m = Hash h -> m ()

refLinearProto :: forall e m  .
    ( MonadIO m
    , Response e (LRefProto e) m
    , HasCredentials e m
    , Serialise (PubKey 'Sign e)
    , Signatures e
    )
    => LRefI e m
    -> LRefProto e
    -> m ()
refLinearProto LRefI{..} = \case

    AnnLRef h (lref@LinearMutableRefSigned{}) -> do
        creds <- getCredentials @e

        void $ runMaybeT do
            g :: RefGenesis e <- MaybeT $
                (((either (const Nothing) Just . deserialiseOrFail) =<<) <$> getBlockI h)

            lift $ forM_ (verifyLinearMutableRefSigned (refOwner g) lref) \vlref -> do
                r <- tryUpdateLinearRefI h vlref
                when r (announceLRefValI h)

    LRefGetVal h -> void $ runMaybeT do
        slref <- MaybeT (getLRefValI h)
        lift $ response (AnnLRef @e h slref)
