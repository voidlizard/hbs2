{-# Language UndecidableInstances #-}
module HBS2.Net.Auth.Credentials.Sigil where

import HBS2.Prelude
import HBS2.Base58
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Net.Proto.Types
import Data.List.Split (chunksOf)
import HBS2.Net.Auth.Credentials

import Codec.Serialise
import Crypto.Saltine.Class qualified as Crypto
import Crypto.Saltine.Class (IsEncoding(..))
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Maybe
import Lens.Micro.Platform


-- | The Sigil data, representing a user identifier in the system.
--
-- Contains an encryption public key, optional additional information,
-- and a possible reference to an additional information block.

data SigilData s =
  SigilData
  { sigilDataEncKey :: PubKey 'Encrypt s
  , sigilDataInfo   :: Maybe Text
  , sigilDataExt    :: Maybe HashRef
  }
  deriving stock (Generic)


-- | The 'Sigil' structure, representing an identification artifact in the system.
--
-- Includes a signature public key and signed 'SigilData',
-- ensuring user authentication and verification.

data Sigil s =
  Sigil
  { sigilSignPk     :: PubKey 'Sign s
  , sigilData       :: SignedBox (SigilData s) s
  }
  deriving stock (Generic)


type ForSigil s = ( Serialise (PubKey 'Encrypt s)
                  , Serialise (PubKey 'Sign s)
                  , Serialise (Signature s)
                  , Signatures s
                  , Hashable (PubKey 'Sign s)
                  , IsEncoding (PubKey 'Encrypt s)
                  , Eq (PubKey 'Encrypt s)
                  , FromStringMaybe (PubKey 'Sign s)
                  )

type ForPrettySigil s =
  ( IsEncoding (PubKey 'Encrypt s)
  , Pretty (AsBase58 (PubKey 'Sign s))
  )

instance ForSigil s => Serialise (SigilData s)
instance ForSigil s => Serialise (Sigil s)


instance ForPrettySigil s => Pretty (SigilData s) where
  pretty s = vcat $ [ parens ("encrypt-pubkey" <+> dquotes epk)
                    ] <> catMaybes [pinfo, pext]
    where
      epk = pretty (AsBase58 (Crypto.encode $ sigilDataEncKey s))
      pinfo = sigilDataInfo s >>= \x -> pure $ parens ("info" <+> dquotes (pretty x))
      pext = sigilDataExt s >>= \x -> pure $ parens ("ext" <+> dquotes (pretty x))

instance ForPrettySigil s => Pretty (Sigil s) where
  pretty s = vcat
             [ parens ("sign-pubkey" <+> psk)
             ]
    where
      psk = dquotes (pretty (AsBase58 (sigilSignPk s)))

-- Nothing, если ключ отсутствует в Credentials
makeSigilFromCredentials :: forall s . ForSigil s
                         => PeerCredentials s
                         -> PubKey 'Encrypt s
                         -> Maybe Text
                         -> Maybe HashRef
                         -> Maybe (Sigil s)

makeSigilFromCredentials cred pk i ha = runIdentity $ runMaybeT do

  let ppk = view peerSignPk cred
  let psk = view peerSignSk cred

  ke <- MaybeT $ pure $ headMay [ view krPk x
                                | x <- view peerKeyring cred
                                , view krPk x == pk
                                ]

  let sd = SigilData ke i ha

  let box = makeSignedBox @s ppk psk sd

  let sigil = Sigil
              { sigilSignPk = view peerSignPk cred
              , sigilData   = box
              }

  pure sigil


instance ForSigil s => Pretty (AsBase58 (Sigil s)) where
  pretty (AsBase58 s) = "# sigil file. public data" <> line <> sd
    where
      sd = vcat $ fmap pretty
                $ chunksOf 60
                $ B8.unpack
                $ toBase58 (LBS.toStrict $ serialise s)

