module HBS2.CLI.Run.Internal.Merkle where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.GroupKey

import HBS2.Data.Types.Refs
import HBS2.Merkle
import HBS2.Storage
import HBS2.Storage.Operations.ByteString
import HBS2.Peer.RPC.Client.Unix
import HBS2.KeyMan.Keys.Direct

import HBS2.Net.Auth.Schema()

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as Text

-- TODO: client-api-candidate
createTreeWithMetadata :: (MonadUnliftIO m)
                       => AnyStorage
                       -> Maybe (GroupKey 'Symm 'HBS2Basic)
                       -> HashMap Text Text
                       -> LBS.ByteString
                       -> m HashRef
createTreeWithMetadata sto mgk meta lbs = do -- flip runContT pure do

    let mt = vcat [ pretty k <> ":" <+> dquotes (pretty v) | (k,v) <- HM.toList meta ]
               & show & Text.pack

    case mgk of
      Nothing -> createSimpleTree mt
      Just gk -> createEncryptedTree gk mt

  where
    createSimpleTree mt = do
        t0 <- writeAsMerkle sto lbs
                >>= getBlock sto
                >>= orThrowUser "can't read merkle tree just written"
                <&> deserialiseOrFail @(MTree [HashRef])
                >>= orThrowUser "merkle tree corrupted/invalid"

        -- FIXME: support-encryption
        let mann = MTreeAnn (ShortMetadata mt) NullEncryption t0

        putBlock sto (serialise mann)
          >>= orThrowUser "can't write tree"
          <&> HashRef

    createEncryptedTree gk mt = do
      -- 1.
      error "oopsie"


