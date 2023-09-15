{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module HBS2.Data.Types.SignedBox where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Types
import HBS2.Net.Auth.Credentials

import Codec.Serialise
import Data.Hashable
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Control.Monad.Trans.Maybe
import Data.Function
import Control.Monad.Identity

data SignedBox p e =
  SignedBox (PubKey 'Sign (Encryption e)) ByteString (Signature (Encryption e))
  deriving stock (Generic)

deriving stock instance
  ( Eq (PubKey 'Sign (Encryption e))
  , Eq (Signature (Encryption e))
  ) => Eq (SignedBox p e)

instance ( Eq (PubKey 'Sign (Encryption e))
         , Eq (Signature (Encryption e))
         , Serialise (SignedBox p e)
         ) =>  Hashable (SignedBox p e) where
  hashWithSalt salt box = hashWithSalt salt (serialise box)


type ForSignedBox e = ( Serialise ( PubKey 'Sign (Encryption e))
                      , FromStringMaybe (PubKey 'Sign (Encryption e))
                      , Serialise (Signature (Encryption e))
                      , Hashable (PubKey 'Sign (Encryption e))
                      )

instance ForSignedBox e => Serialise (SignedBox p e)

makeSignedBox :: forall e p . (Serialise p, ForSignedBox e, Signatures (Encryption e))
              => PubKey 'Sign (Encryption e)
              -> PrivKey 'Sign (Encryption e)
              -> p
              -> SignedBox p e

makeSignedBox pk sk msg = SignedBox @p @e pk bs sign
  where
    bs = LBS.toStrict (serialise msg)
    sign = makeSign @(Encryption e) sk bs


unboxSignedBox0 :: forall p e . (Serialise p, ForSignedBox e, Signatures (Encryption e))
               => SignedBox p e
               -> Maybe (PubKey 'Sign (Encryption e), p)

unboxSignedBox0 (SignedBox pk bs sign) = runIdentity $ runMaybeT do
  guard $ verifySign @(Encryption e) pk sign bs
  p <- MaybeT $ pure $ deserialiseOrFail @p (LBS.fromStrict bs) & either (const Nothing) Just
  pure (pk, p)

unboxSignedBox :: forall p e . (Serialise p, ForSignedBox e, Signatures (Encryption e))
               => LBS.ByteString
               -> Maybe (PubKey 'Sign (Encryption e), p)

unboxSignedBox bs = runIdentity $ runMaybeT do

  box <- MaybeT $ pure $ deserialiseOrFail @(SignedBox p e) bs
                          & either (pure Nothing) Just

  MaybeT $ pure $ unboxSignedBox0 box
