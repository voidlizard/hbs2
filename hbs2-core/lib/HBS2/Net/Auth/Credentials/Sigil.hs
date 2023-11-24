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

data SigilData e =
  SigilData
  { sigilDataEncKey :: PubKey 'Encrypt (Encryption e)
  , sigilDataInfo   :: Maybe Text
  , sigilDataExt    :: Maybe HashRef
  }
  deriving stock (Generic)


-- | The 'Sigil' structure, representing an identification artifact in the system.
--
-- Includes a signature public key and signed 'SigilData',
-- ensuring user authentication and verification.

data Sigil e =
  Sigil
  { sigilSignPk     :: PubKey 'Sign (Encryption e)
  , sigilData       :: SignedBox (SigilData e) e
  }
  deriving stock (Generic)


type ForSigil e = ( Serialise (PubKey 'Encrypt (Encryption e))
                  , Serialise (PubKey 'Sign (Encryption e))
                  , Serialise (Signature (Encryption e))
                  , Signatures (Encryption e)
                  , Hashable (PubKey 'Sign (Encryption e))
                  , IsEncoding (PubKey 'Encrypt (Encryption e))
                  , Eq (PubKey 'Encrypt (Encryption e))
                  , FromStringMaybe (PubKey 'Sign (Encryption e))
                  )

type ForPrettySigil e =
  ( IsEncoding (PubKey 'Encrypt (Encryption e))
  , Pretty (AsBase58 (PubKey 'Sign (Encryption e)))
  )

instance ForSigil e => Serialise (SigilData e)
instance ForSigil e => Serialise (Sigil e)


instance ForPrettySigil e => Pretty (SigilData e) where
  pretty s = vcat $ [ parens ("encrypt-pubkey" <+> dquotes epk)
                    ] <> catMaybes [pinfo, pext]
    where
      epk = pretty (AsBase58 (Crypto.encode $ sigilDataEncKey s))
      pinfo = sigilDataInfo s >>= \x -> pure $ parens ("info" <+> dquotes (pretty x))
      pext = sigilDataExt s >>= \x -> pure $ parens ("ext" <+> dquotes (pretty x))

instance ForPrettySigil e => Pretty (Sigil e) where
  pretty s = vcat
             [ parens ("sign-pubkey" <+> psk)
             ]
    where
      psk = dquotes (pretty (AsBase58 (sigilSignPk s)))

-- Nothing, если ключ отсутствует в Credentials
makeSigilFromCredentials :: forall e . ForSigil e
                         => PeerCredentials (Encryption e)
                         -> PubKey 'Encrypt (Encryption e)
                         -> Maybe Text
                         -> Maybe HashRef
                         -> Maybe (Sigil e)

makeSigilFromCredentials cred pk i ha = runIdentity $ runMaybeT do

  let ppk = view peerSignPk cred
  let psk = view peerSignSk cred

  ke <- MaybeT $ pure $ headMay [ view krPk x
                                | x <- view peerKeyring cred
                                , view krPk x == pk
                                ]

  let sd = SigilData ke i ha

  let box = makeSignedBox @e ppk psk sd

  let sigil = Sigil
              { sigilSignPk = view peerSignPk cred
              , sigilData   = box
              }

  pure sigil


instance ForSigil e => Pretty (AsBase58 (Sigil e)) where
  pretty (AsBase58 s) = "# sigil file. public data" <> line <> sd
    where
      sd = vcat $ fmap pretty
                $ chunksOf 60
                $ B8.unpack
                $ toBase58 (LBS.toStrict $ serialise s)

