{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.Proto.LWWRef where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Base58
import HBS2.Storage
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Types
import HBS2.Data.Types.SignedBox
import HBS2.Data.Types.Refs
import HBS2.Net.Proto.Types
import HBS2.Net.Auth.Schema()

import Data.ByteString (ByteString)
import Data.Hashable hiding (Hashed)
import Data.Maybe
import Data.Word
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Codec.Serialise

data LWWRefProtoReq e =
    LWWProtoGet (LWWRefKey (Encryption e))
  | LWWProtoSet (LWWRefKey (Encryption e)) (SignedBox (LWWRef e) e)
  deriving stock Generic


data LWWRefProto e =
  LWWRefProto1 (LWWRefProtoReq e)
  deriving stock (Generic)

data LWWRef e =
  LWWRef
  { lwwSeq      :: Word64
  , lwwValue    :: HashRef
  , lwwProof    :: Maybe HashRef
  }
  deriving stock (Generic)


type ForLWWRefProto e = (ForSignedBox e, Serialise (LWWRefKey (Encryption e)))

instance ForLWWRefProto e => Serialise (LWWRefProtoReq e)
instance ForLWWRefProto e => Serialise (LWWRefProto e)
instance ForLWWRefProto e => Serialise (LWWRef e)

newtype LWWRefKey s =
  LWWRefKey
  { fromLwwRefKey :: PubKey 'Sign s
  }
  deriving stock (Generic)


instance RefMetaData (LWWRefKey s)

deriving stock instance IsRefPubKey s => Eq (LWWRefKey s)

instance IsRefPubKey e => Serialise (LWWRefKey e)

instance IsRefPubKey s => Hashable (LWWRefKey s) where
  hashWithSalt s k = hashWithSalt s (hashObject @HbSync k)

instance IsRefPubKey s => Hashed HbSync (LWWRefKey s) where
  hashObject (LWWRefKey pk) = hashObject ("lwwrefkey|" <> serialise pk)

instance IsRefPubKey s => FromStringMaybe (LWWRefKey s) where
  fromStringMay s = LWWRefKey <$> fromStringMay s

instance IsRefPubKey s =>  IsString (LWWRefKey s) where
  fromString s = fromMaybe (error "bad public key base58") (fromStringMay s)

instance Pretty (AsBase58 (PubKey 'Sign s )) => Pretty (AsBase58 (LWWRefKey s)) where
  pretty (AsBase58 (LWWRefKey k)) = pretty (AsBase58 k)

instance Pretty (AsBase58 (PubKey 'Sign s )) => Pretty (LWWRefKey s) where
  pretty (LWWRefKey k) = pretty (AsBase58 k)


instance Pretty (LWWRef e) where
  pretty (LWWRef{..}) = parens ( "lwwref" <> line
                          <> indent 2 ( seqno <> line <> val <> line <> proof)
                        )
    where
      seqno = parens ( "seq" <+> pretty lwwSeq )
      val   = parens ( "value" <+> dquotes (pretty lwwValue) )
      proof | isNothing lwwProof = mempty
            | otherwise = parens ( "proof" <+> pretty lwwProof)


data ReadLWWRefError =
    ReadLWWStorageError
  | ReadLWWFormatError
  | ReadLWWSignatureError
  deriving stock (Show,Typeable)

readLWWRef :: forall e s m . ( MonadIO m
                             , MonadError ReadLWWRefError m
                             , Encryption e ~ s
                             , ForLWWRefProto e
                             , Signatures s
                             , IsRefPubKey s
                             )
           => AnyStorage
           -> LWWRefKey s
           -> m (Maybe (LWWRef e))

readLWWRef sto key = runMaybeT do
  getRef sto key
    >>= toMPlus
    >>= getBlock sto
    >>= toMPlus
    <&> deserialiseOrFail @(SignedBox (LWWRef e) e)
    >>= orThrowError ReadLWWFormatError
    <&> unboxSignedBox0
    >>= orThrowError ReadLWWSignatureError
    <&> snd

updateLWWRef :: forall s e m . ( Encryption e ~ s
                               , ForLWWRefProto e
                               , MonadIO m
                               , Signatures s
                               , IsRefPubKey s
                               )
             => AnyStorage
             -> LWWRefKey s
             -> PrivKey 'Sign s
             -> LWWRef e
             -> m (Maybe HashRef)

updateLWWRef sto k sk v = do
  let box = makeSignedBox @e (fromLwwRefKey k) sk v
  runMaybeT do
    hx <- putBlock sto (serialise box) >>= toMPlus
    updateRef sto k hx
    pure (HashRef hx)

