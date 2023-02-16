{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language ConstraintKinds #-}
module HBS2.Net.Auth.Credentials where

import HBS2.Prelude.Plated
import HBS2.Base58

import Codec.Serialise
import Crypto.Saltine.Core.Sign (Keypair(..))
import Crypto.Saltine.Core.Sign qualified as Sign
import Crypto.Saltine.Core.Box qualified as Encrypt
import Crypto.Saltine.Class qualified as Crypto
import Crypto.Saltine.Class (IsEncoding)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Char8 (ByteString)
import Data.Function
import Data.List.Split (chunksOf)
import Data.Text (Text)
import Data.List qualified as List
import Lens.Micro.Platform
import Data.Kind
import Prettyprinter


type family EncryptPubKey e :: Type

data CryptoAction = Sign | Encrypt

type family PubKey  ( a :: CryptoAction) e  :: Type
type family PrivKey ( a :: CryptoAction) e  :: Type

class Signatures e where
  type family Signature e :: Type
  makeSign   :: PrivKey 'Sign e -> ByteString  -> Signature e
  verifySign :: PubKey 'Sign e  -> Signature e -> ByteString -> Bool


class HasCredentials e m where
  getCredentials :: m (PeerCredentials e)

data KeyringEntry e =
  KeyringEntry
  { _krPk   :: PubKey  'Encrypt e
  , _krSk   :: PrivKey 'Encrypt e
  , _krDesc :: Maybe Text
  }
  deriving stock (Generic)

deriving stock instance (Eq (PubKey 'Encrypt e), Eq (PrivKey 'Encrypt e))
  => Eq (KeyringEntry e)

data PeerCredentials e =
  PeerCredentials
  { _peerSignSk   :: PrivKey 'Sign e
  , _peerSignPk   :: PubKey 'Sign e
  , _peerKeyring  :: [KeyringEntry e]
  }
  deriving Generic

makeLenses 'KeyringEntry
makeLenses 'PeerCredentials

type SerialisedCredentials e = ( Serialise (PrivKey 'Sign e)
                               , Serialise (PubKey 'Sign e)
                               , Serialise (PubKey 'Encrypt e)
                               , Serialise (PrivKey 'Encrypt e)
                               )

instance SerialisedCredentials e => Serialise (KeyringEntry e)

instance SerialisedCredentials e => Serialise (PeerCredentials e)

newtype AsCredFile a = AsCredFile a

-- FIXME: integration-regression-test-for-keyring
--   Добавить тест: сгенерировали keypair/распарсили keypair

newCredentials :: forall e m . ( MonadIO m
                               , Signatures e
                               , PrivKey 'Sign e ~ Sign.SecretKey
                               , PubKey 'Sign e ~ Sign.PublicKey
                               ) => m (PeerCredentials e)
newCredentials = do
  pair <- liftIO Sign.newKeypair
  pure $ PeerCredentials @e (secretKey pair) (publicKey pair) mempty


newKeypair :: forall e m . ( MonadIO m
                           , PrivKey 'Encrypt e ~ Encrypt.SecretKey
                           , PubKey 'Encrypt e ~ Encrypt.PublicKey
                           )
          => Maybe Text -> m (KeyringEntry e)
newKeypair txt = do
  pair <- liftIO Encrypt.newKeypair
  pure $ KeyringEntry @e (Encrypt.publicKey pair) (Encrypt.secretKey pair) txt

addKeyPair :: forall e m . ( MonadIO m
                           , PrivKey 'Encrypt e ~ Encrypt.SecretKey
                           , PubKey 'Encrypt e ~ Encrypt.PublicKey
                           )
           => Maybe Text -> PeerCredentials e -> m (PeerCredentials e)

addKeyPair txt cred = do
  kp <- newKeypair @e txt
  pure $ cred & over peerKeyring (List.nub . (<> [kp]))

delKeyPair :: forall e m . ( MonadIO m
                           , PrivKey 'Encrypt e ~ Encrypt.SecretKey
                           , PubKey 'Encrypt e ~ Encrypt.PublicKey
                           )
           => AsBase58 String -> PeerCredentials e -> m (PeerCredentials e)
delKeyPair (AsBase58 pks) cred = do
  let kring = view peerKeyring cred
  let asStr e = show (pretty (AsBase58 ( Crypto.encode (e ^. krPk) ) ) )
  let rest = [ e | e <- kring, asStr e /= pks ]
  pure $ cred & set peerKeyring rest

parseCredentials :: forall e . ( Signatures e
                               , PrivKey 'Sign e ~ Sign.SecretKey
                               , PubKey 'Sign e ~ Sign.PublicKey
                               , SerialisedCredentials e
                               )
                 =>  AsCredFile ByteString -> Maybe (PeerCredentials e)
parseCredentials (AsCredFile bs) = parseSerialisableFromBase58 bs

parseSerialisableFromBase58 :: Serialise a => ByteString -> Maybe a
parseSerialisableFromBase58 bs = maybe1 b58_1 Nothing fromCbor
  where
    fromCbor s = deserialiseOrFail s
                   & either (const Nothing) Just

    b58_1 = B8.lines bs & dropWhile hdr
                        & filter ( not . B8.null )
                        & B8.concat
                        & fromBase58
                        & fmap LBS.fromStrict

    hdr s = B8.isPrefixOf "#" s || B8.null s

instance ( Serialise  (PeerCredentials e)
         )

  =>  Pretty (AsBase58 (PeerCredentials e)) where
  pretty (AsBase58 c@(PeerCredentials s p _)) = pretty $ B8.unpack (toBase58 bs)
    where
     bs = LBS.toStrict $ serialise c


instance Pretty (AsBase58 Sign.PublicKey) where
  pretty (AsBase58 pk) = pretty $ B8.unpack $ toBase58 (Crypto.encode pk)

-- FIXME: test-from-string-maybe-sign-pub-key
--
instance FromStringMaybe Sign.PublicKey where
  fromStringMay s = de
    where
      de = bs >>= Crypto.decode
      bs = fromBase58 (fromString s)

instance Pretty (AsBase58 a) => Pretty (AsCredFile (AsBase58 a)) where
  pretty (AsCredFile pc) =  "# hbs2 credentials file" <> line
                         <> "# keep it private" <> line <> line
                         <> co
    where
      co = vcat $ fmap pretty
                $ chunksOf 60
                $ show
                $ pretty pc


newtype ListKeyringKeys e = ListKeyringKeys (PeerCredentials e)

instance ( IsEncoding (PubKey 'Sign e), Pretty (KeyringEntry e) )
  => Pretty (ListKeyringKeys e) where
  pretty (ListKeyringKeys p) =
    fill 10 "sign-key:" <+> pretty (AsBase58 (Crypto.encode (view peerSignPk p)))
    <> line <> vcat (fmap pretty (view peerKeyring p))

instance IsEncoding (PubKey 'Encrypt e)
  => Pretty (KeyringEntry e) where
  pretty ke = fill 10 "pub-key:" <+> pretty (AsBase58 (Crypto.encode (view krPk ke)))


