{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language ConstraintKinds #-}
module HBS2.Net.Auth.AccessKey where

import HBS2.Base58
import HBS2.Data.Detect
import HBS2.Data.Types
import HBS2.Defaults
import HBS2.Merkle
import HBS2.Net.Auth.Credentials
import HBS2.Net.Messaging.UDP (UDP)
import HBS2.Net.Proto.Definition()
import HBS2.Net.Proto.Types
import HBS2.OrDie
import HBS2.Prelude.Plated

import Codec.Serialise
import Control.Monad ((<=<))
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


newtype EncryptedBox = EncryptedBox { unEncryptedBox :: ByteString }
  deriving stock (Generic)

instance Serialise EncryptedBox

data EncryptionSchema = NaClAsymm

---

data family AccessKey e ( s :: EncryptionSchema )

newtype instance AccessKey e 'NaClAsymm =
  AccessKeyNaClAsymm
  { permitted :: [(PubKey 'Encrypt e, EncryptedBox)]
  }
  deriving stock (Generic)

instance Serialise (AccessKey e 'NaClAsymm)

---

data family GroupKey e ( s :: EncryptionSchema )

data instance GroupKey e 'NaClAsymm =
  GroupKeyNaClAsymm
  { recipientPk :: PubKey 'Encrypt e
  , accessKey :: AccessKey e 'NaClAsymm
  }
  deriving stock (Generic)

instance Serialise (GroupKey e 'NaClAsymm)

---

newtype AsGroupKeyFile a = AsGroupKeyFile a

-- FIXME: integration-regression-test-for-groupkey
--   Добавить тест: сгенерировали groupkey/распарсили groupkey

parseGroupKey :: forall e . ()
                 =>  AsGroupKeyFile ByteString -> Maybe (GroupKey e 'NaClAsymm)
parseGroupKey (AsGroupKeyFile bs) = parseSerialisableFromBase58 bs

instance ( Serialise  (GroupKey e s)
         )

  =>  Pretty (AsBase58 (GroupKey e s)) where
  pretty (AsBase58 c) =
    pretty . B8.unpack . toBase58 . LBS.toStrict . serialise $ c


instance Pretty (AsBase58 a) => Pretty (AsGroupKeyFile (AsBase58 a)) where
  pretty (AsGroupKeyFile pc) =  "# hbs2 groupkey file" <> line <> co
    where
      co = vcat $ fmap pretty
                $ chunksOf 60
                $ show
                $ pretty pc


-- newtype ListGroupKeyKeys e s = ListGroupKeyKeys (GroupKey e s)

-- instance ()
--   => Pretty (ListGroupKeyKeys e 'NaClAsymm) where
--   pretty (ListGroupKeyKeys (GroupKeyNaClAsymm keypair pubkeys)) =
--     fill 10 "recipient public keys:"
--     <+> vcat (pretty . AsBase58 . Crypto.encode <$> pubkeys)
--     <> line
--     <> pretty keypair

---

parsePubKeys :: forall e . ()
                 =>  ByteString -> Maybe [PubKey 'Encrypt e]

parsePubKeys = sequenceA . fmap (Crypto.decode <=< fromBase58) . B8.lines

---

mkEncryptedKey :: KeyringEntry MerkleEncryptionType -> PubKey 'Encrypt MerkleEncryptionType -> IO EncryptedBox
mkEncryptedKey kr pk = EncryptedBox <$> Encrypt.boxSeal pk ((LBS.toStrict . serialise) kr)

openEncryptedKey :: EncryptedBox -> KeyringEntry MerkleEncryptionType -> Maybe (KeyringEntry MerkleEncryptionType)
openEncryptedKey (EncryptedBox bs) kr =
    either (const Nothing) Just . deserialiseOrFail . LBS.fromStrict =<< Encrypt.boxSealOpen (_krPk kr) (_krSk kr) bs
