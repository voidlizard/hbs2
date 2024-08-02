{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.CLI.Run.Internal
  ( module HBS2.CLI.Run.Internal
  , module SC
  ) where

import HBS2.CLI.Prelude

import HBS2.OrDie
import HBS2.Base58
import HBS2.Data.Types.Refs
import HBS2.Storage
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient

import Data.Config.Suckless.Script qualified as SC
import Data.Config.Suckless.Script hiding (internalEntries)

import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as Text

pattern HashLike:: forall {c} . HashRef -> Syntax c
pattern HashLike x <- (
  \case
    StringLike s -> fromStringMay @HashRef s
    _            -> Nothing
      -> Just x )

pattern SignPubKeyLike :: forall {c} . (PubKey 'Sign 'HBS2Basic) -> Syntax c
pattern SignPubKeyLike x <- (
  \case
    StringLike s -> fromStringMay s
    _            -> Nothing
      -> Just x )

internalEntries :: forall c m . (IsContext c, Exception (BadFormException c), MonadUnliftIO m) => MakeDictM c m ()
internalEntries = do
    SC.internalEntries

    entry $ bindMatch "blob:base58" $ \case
      [LitStrVal t] -> do
        bs <- pure (Text.unpack t & BS8.pack & fromBase58)
               `orDie` "invalid base58"
              <&> BS8.unpack

        pure (mkForm "blob" [mkStr @c bs])

      _ -> throwIO (BadFormException @c nil)


    let decodeB58 t = do
          pure (Text.unpack t & BS8.pack & fromBase58)
            `orDie` "invalid base58"

    let decodeAndOut t = do
          liftIO $ BS8.putStr =<< decodeB58 t

    entry $ bindMatch "base58:encode" $ \case
      [LitStrVal t] -> do
        let s = Text.unpack t & BS8.pack & toBase58 & BS8.unpack
        pure (mkForm "blob:base58" [mkStr @c s])

      [ListVal [SymbolVal "blob", LitStrVal t]] -> do
        let s = Text.unpack t & BS8.pack & toBase58 & BS8.unpack
        pure (mkForm "blob:base58" [mkStr @c s])

      e -> throwIO (BadFormException @c nil)

    entry $ bindMatch "base58:decode" $ \case

      [ListVal [SymbolVal "blob:base58", LitStrVal t]] -> do
        s <- decodeB58 t <&> BS8.unpack
        pure $ mkForm "blob" [mkStr @c s]

      e -> throwIO (BadFormException @c nil)

    entry $ bindMatch "base58:put" $ nil_ $ \case
      [ListVal [SymbolVal "blob:base58", LitStrVal t]] ->
        decodeAndOut t

      [LitStrVal t] -> decodeAndOut t

      e -> throwIO (BadFormException @c nil)


instance MonadUnliftIO m => HasStorage (RunM c m) where
  getStorage = do
    so <- detectRPC `orDie` "hbs2-peer not found"
    withRPC2 @StorageAPI  @UNIX so $ \caller -> do
      pure $ AnyStorage (StorageClient caller)

withPeerStorage :: (IsContext c, MonadUnliftIO m) => (AnyStorage -> RunM c m a) -> RunM c m a
withPeerStorage m = do
    so <- detectRPC `orDie` "hbs2-peer not found"

    withRPC2 @StorageAPI  @UNIX so $ \caller -> do
      let sto = AnyStorage (StorageClient caller)
      m sto


