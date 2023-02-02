{-# Language UndecidableInstances #-}
module HBS2.Net.Auth.Credentials where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Types
import HBS2.Base58
import HBS2.Net.Messaging.UDP (UDP)

import Codec.Serialise
import Crypto.Saltine.Core.Sign (Keypair(..))
import Crypto.Saltine.Core.Sign qualified as Sign
import Crypto.Saltine.Class qualified as Crypto
import Crypto.Saltine.Class (IsEncoding)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Char8 (ByteString)
import Data.Function
import Data.List.Split (chunksOf)
import Prettyprinter

newtype AsBase58 a = AsBase58 a

newtype AsCredFile a = AsCredFile a

newCredentials :: forall e m . ( MonadIO m
                               , Signatures e
                               , PrivKey 'Sign e ~ Sign.SecretKey
                               , PubKey 'Sign e ~ Sign.PublicKey
                               ) => m (PeerCredentials e)
newCredentials = do
  pair <- liftIO Sign.newKeypair
  pure $ PeerCredentials @e (secretKey pair) (publicKey pair)


parseCredentials :: forall e . ( Signatures e
                               , PrivKey 'Sign e ~ Sign.SecretKey
                               , PubKey 'Sign e ~ Sign.PublicKey
                               )
                 =>  AsCredFile ByteString -> Maybe (PeerCredentials e)

parseCredentials (AsCredFile bs) = maybe1 b58_1 Nothing fromCbor

  where
    fromCbor s = deserialiseOrFail @(ByteString, ByteString) s
                   & either (const Nothing) fromPair

    fromPair (s1,s2) = PeerCredentials <$> Crypto.decode s1
                                       <*> Crypto.decode s2

    b58_1 = B8.lines bs & dropWhile hdr
                        & filter ( not . B8.null )
                        & B8.concat
                        & fromBase58
                        & fmap LBS.fromStrict

    hdr s = B8.isPrefixOf "#" s || B8.null s

instance ( IsEncoding (PrivKey 'Sign e)
         , IsEncoding (PubKey 'Sign e)
         )

  =>  Pretty (AsBase58 (PeerCredentials e)) where
  pretty (AsBase58 (PeerCredentials s p)) = pretty $ B8.unpack (toBase58 bs)
    where
     sk = Crypto.encode s
     pk = Crypto.encode p
     bs = serialise (sk,pk) & LBS.toStrict

instance Pretty (AsBase58 Sign.PublicKey) where
  pretty (AsBase58 pk) = pretty $ B8.unpack $ toBase58 (Crypto.encode pk)

instance Pretty (AsBase58 a) => Pretty (AsCredFile (AsBase58 a)) where
  pretty (AsCredFile pc) =  "# hbs2 credentials file" <> line
                         <> "# keep it private" <> line <> line
                         <> co
    where
      co = vcat $ fmap pretty
                $ chunksOf 32
                $ show
                $ pretty pc


