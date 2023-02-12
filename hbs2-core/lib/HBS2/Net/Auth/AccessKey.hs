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

newtype AccessKeyV1 e = AccessKeyV1
  { permitted :: [(PubKey 'Encrypt e, EncryptedBox)]
  }
  deriving stock (Generic)

instance Serialise (AccessKeyV1 e)
