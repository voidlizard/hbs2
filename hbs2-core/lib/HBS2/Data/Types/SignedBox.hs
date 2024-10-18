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
import Control.Monad.Identity

data SignedBox p s =
  SignedBox (PubKey 'Sign s) ByteString (Signature s)
  deriving stock (Generic)

deriving stock instance
  ( Eq (PubKey 'Sign s)
  , Eq (Signature s)
  ) => Eq (SignedBox p s)

instance ( Eq (PubKey 'Sign s)
         , Eq (Signature s)
         , Serialise (SignedBox p s)
         ) =>  Hashable (SignedBox p s) where
  hashWithSalt salt box = hashWithSalt salt (serialise box)


type ForSignedBox s = ( Serialise ( PubKey 'Sign s)
                      , FromStringMaybe (PubKey 'Sign s)
                      , Serialise (Signature s)
                      , Signatures s
                      , Eq (Signature s)
                      , Hashable (PubKey 'Sign s)
                      )

instance ForSignedBox s => Serialise (SignedBox p s)

makeSignedBox :: forall s p . (Serialise p, ForSignedBox s, Signatures s)
              => PubKey 'Sign s
              -> PrivKey 'Sign s
              -> p
              -> SignedBox p s

makeSignedBox pk sk msg = SignedBox @p @s pk bs sign
  where
    bs = LBS.toStrict (serialise msg)
    sign = makeSign @s sk bs


unboxSignedBox0 :: forall p s . (Serialise p, ForSignedBox s, Signatures s)
               => SignedBox p s
               -> Maybe (PubKey 'Sign s, p)

unboxSignedBox0 (SignedBox pk bs sign) = runIdentity $ runMaybeT do
  guard $ verifySign @s pk sign bs
  p <- MaybeT $ pure $ deserialiseOrFail @p (LBS.fromStrict bs) & either (const Nothing) Just
  pure (pk, p)

unboxSignedBox :: forall p s . (Serialise p, ForSignedBox s, Signatures s)
               => LBS.ByteString
               -> Maybe (PubKey 'Sign s, p)

unboxSignedBox bs = runIdentity $ runMaybeT do

  box <- MaybeT $ pure $ deserialiseOrFail @(SignedBox p s) bs
                          & either (pure Nothing) Just

  MaybeT $ pure $ unboxSignedBox0 box

