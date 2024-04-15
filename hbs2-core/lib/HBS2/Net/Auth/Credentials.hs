{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language ConstraintKinds #-}
{-# Language PatternSynonyms #-}
module HBS2.Net.Auth.Credentials
  ( module HBS2.Net.Auth.Credentials
  , module HBS2.Net.Auth.Schema
  ) where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Types
import HBS2.Net.Auth.Schema
import HBS2.Base58
import HBS2.Hash

import Control.Applicative
import Codec.Serialise
import Crypto.Saltine.Core.Sign (Keypair(..))
import Crypto.Saltine.Core.Sign qualified as Sign
import Crypto.Saltine.Core.Box qualified as Encrypt
import Crypto.Saltine.Class qualified as Crypto
import Crypto.Saltine.Class (IsEncoding)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Char8 (ByteString)
import Data.List.Split (chunksOf)
import Data.List qualified as List
import Lens.Micro.Platform
import Data.Kind


instance Signatures 'HBS2Basic where
  type Signature 'HBS2Basic = Sign.Signature
  makeSign = Sign.signDetached
  verifySign = Sign.signVerifyDetached

type instance KeyActionOf Sign.PublicKey = 'Sign
type instance KeyActionOf Encrypt.PublicKey = 'Encrypt

instance Serialise Sign.Signature
instance Serialise Sign.PublicKey
instance Serialise Sign.SecretKey
instance Serialise Encrypt.PublicKey
instance Serialise Encrypt.SecretKey

type family EncryptPubKey e :: Type

class Signatures e where
  type family Signature e :: Type
  makeSign   :: PrivKey 'Sign e -> ByteString  -> Signature e
  verifySign :: PubKey 'Sign e  -> Signature e -> ByteString -> Bool

class AsymmPubKey e ~ PubKey 'Encrypt e => Asymm e where
  type family AsymmKeypair e :: Type
  type family AsymmPrivKey e :: Type
  type family AsymmPubKey e :: Type
  type family CommonSecret e :: Type
  asymmNewKeypair :: MonadIO m => m (AsymmKeypair e)
  privKeyFromKeypair :: AsymmKeypair e -> AsymmPrivKey e
  pubKeyFromKeypair :: AsymmKeypair e -> AsymmPubKey e
  genCommonSecret :: Asymm e => AsymmPrivKey e -> AsymmPubKey e -> CommonSecret e

class HasCredentials s m where
  getCredentials :: m (PeerCredentials s)

data KeyringEntry s =
  KeyringEntry
  { _krPk   :: PubKey  'Encrypt s
  , _krSk   :: PrivKey 'Encrypt s
  , _krDesc :: Maybe Text
  }
  deriving stock (Generic)

pattern KeyringKeys :: forall {s} . PubKey 'Encrypt s -> PrivKey 'Encrypt s -> KeyringEntry s
pattern KeyringKeys a b <- KeyringEntry a b _
{-# COMPLETE KeyringKeys #-}

deriving stock instance (Eq (PubKey 'Encrypt e), Eq (PrivKey 'Encrypt e))
  => Eq (KeyringEntry e)

data PeerCredentials s =
  PeerCredentials
  { _peerSignSk   :: PrivKey 'Sign s
  , _peerSignPk   :: PubKey 'Sign s
  , _peerKeyring  :: [KeyringEntry s]
  }
  deriving Generic

makeLenses 'KeyringEntry
makeLenses 'PeerCredentials

type ForHBS2Basic s = ( Signatures s
                       , PrivKey 'Sign s ~ Sign.SecretKey
                       , PubKey 'Sign s ~ Sign.PublicKey
                       , Eq (PubKey 'Encrypt 'HBS2Basic)
                       , IsEncoding (PubKey 'Encrypt s)
                       , Eq (PubKey 'Encrypt 'HBS2Basic)
                       , s ~ 'HBS2Basic
                       )

type SerialisedCredentials ( s :: CryptoScheme ) =
    ( Serialise (PrivKey 'Sign s)
    , Serialise (PubKey 'Sign s)
    , Serialise (PubKey 'Encrypt s)
    , Serialise (PrivKey 'Encrypt s)
    )

instance SerialisedCredentials s => Serialise (KeyringEntry s)

instance SerialisedCredentials s => Serialise (PeerCredentials s)

newtype AsCredFile a = AsCredFile a



-- FIXME: integration-regression-test-for-keyring
--   Добавить тест: сгенерировали keypair/распарсили keypair

newCredentials :: forall s m . ( MonadIO m
                               , Signatures s
                               , PrivKey 'Sign s ~ Sign.SecretKey
                               , PubKey 'Sign s ~ Sign.PublicKey
                               ) => m (PeerCredentials s)
newCredentials = do
  pair <- liftIO Sign.newKeypair
  pure $ PeerCredentials @s (secretKey pair) (publicKey pair) mempty


newKeypair :: forall s m . ( MonadIO m
                           , PrivKey 'Encrypt s ~ Encrypt.SecretKey
                           , PubKey 'Encrypt s ~ Encrypt.PublicKey
                           )
          => Maybe Text -> m (KeyringEntry s)
newKeypair txt = do
  pair <- liftIO Encrypt.newKeypair
  pure $ KeyringEntry @s (Encrypt.publicKey pair) (Encrypt.secretKey pair) txt

addKeyPair :: forall e m . ( MonadIO m
                           , PrivKey 'Encrypt e ~ Encrypt.SecretKey
                           , PubKey 'Encrypt e ~ Encrypt.PublicKey
                           )
           => Maybe Text -> PeerCredentials e -> m (PeerCredentials e)

addKeyPair txt cred = do
  kp <- newKeypair @e txt
  pure $ cred & over peerKeyring (List.nub . (<> [kp]))

delKeyPair :: forall e m . ( MonadIO m
                           , ForHBS2Basic e
                           )
           => AsBase58 String -> PeerCredentials e -> m (PeerCredentials e)
delKeyPair (AsBase58 pks) cred = do
  let kring = view peerKeyring cred
  let asStr e = show (pretty (AsBase58 ( Crypto.encode (e ^. krPk) ) ) )
  let rest = [ e | e <- kring, asStr e /= pks ]
  pure $ cred & set peerKeyring rest


parseCredentials :: forall s . ( -- ForHBS2Basic s
                                 SerialisedCredentials s
                               )
                 =>  AsCredFile ByteString -> Maybe (PeerCredentials s)
parseCredentials (AsCredFile bs) =
  parseSerialisableFromBase58 bs <|> parseSerialisableFromCbor (LBS.fromStrict bs)

parseSerialisableFromCbor :: SerialisedCredentials s => LBS.ByteString -> Maybe (PeerCredentials s)
parseSerialisableFromCbor = fromCbor
    where fromCbor s = deserialiseOrFail s
                        & either (const Nothing) Just

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

-- FIXME: move-thouse-instances-to-appropriate-place-ASAP

instance Pretty (AsBase58 Sign.PublicKey) where
  pretty (AsBase58 pk) = pretty $ B8.unpack $ toBase58 (Crypto.encode pk)

instance Pretty (AsBase58 Encrypt.PublicKey) where
  pretty (AsBase58 pk) = pretty $ B8.unpack $ toBase58 (Crypto.encode pk)

-- FIXME: test-from-string-maybe-sign-pub-key
--
instance FromStringMaybe Sign.PublicKey where
  fromStringMay s = de
    where
      de = bs >>= Crypto.decode
      bs = fromBase58 (fromString s)

instance FromStringMaybe Encrypt.PublicKey where
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


instance Asymm 'HBS2Basic where
  type AsymmKeypair 'HBS2Basic = Encrypt.Keypair
  type AsymmPrivKey 'HBS2Basic = Encrypt.SecretKey
  type AsymmPubKey 'HBS2Basic = Encrypt.PublicKey
  type CommonSecret 'HBS2Basic = Encrypt.CombinedKey
  asymmNewKeypair = liftIO Encrypt.newKeypair
  privKeyFromKeypair = Encrypt.secretKey
  pubKeyFromKeypair = Encrypt.publicKey
  genCommonSecret = Encrypt.beforeNM

instance Hashed HbSync Sign.PublicKey where
  hashObject pk = hashObject (Crypto.encode pk)



