module HBS2.CLI.Run.MetaData where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Data.Types.Refs
import HBS2.Merkle
import HBS2.System.Logger.Simple.ANSI as All

import HBS2.Storage
import HBS2.Storage.Operations.ByteString
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient

import HBS2.Net.Auth.Schema()

import Codec.Serialise
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.Either
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Data.Text.Encoding qualified as TE
import Data.Text qualified as Text

{- HLINT ignore "Functor law" -}

metaFromSyntax :: [Syntax c] -> HashMap Text Text
metaFromSyntax syn =
  HM.fromList [ (t k, t v) | (ListVal [ k, v ]) <- syn ]
  where
    t x = Text.pack (show $ pretty x)

createTreeWithMetadata :: (MonadUnliftIO m)
                       => HashMap Text Text
                       -> LBS.ByteString
                       -> m HashRef
createTreeWithMetadata meta lbs = do
    debug   "create fucking metadata"
    -- TODO: set-hbs2-peer
    so <- detectRPC `orDie` "hbs2-peer not found"

    let mt = vcat [ pretty k <> ":" <+> pretty v | (k,v) <- HM.toList meta ]
               & show & Text.pack

    withRPC2 @StorageAPI  @UNIX so $ \caller -> do
      let sto = AnyStorage (StorageClient caller)

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


metaDataEntries :: forall c m . (c ~ C, IsContext c, MonadUnliftIO m) => MakeDictM c m ()
metaDataEntries = do

  entry $ bindMatch "hbs2:tree:metadata:get" $ \case
    [ SymbolVal how, StringLike hash ] -> do

      -- FIXME: put-to-the-state
      so <- detectRPC `orDie` "hbs2-peer not found"

      r <- withRPC2 @StorageAPI  @UNIX so $ \caller -> do
        let sto = AnyStorage (StorageClient caller)

        runMaybeT do

          headBlock <- getBlock sto (fromString hash)
                         >>= toMPlus
                         <&> deserialiseOrFail @(MTreeAnn [HashRef])
                         >>= toMPlus

          case headBlock of
            MTreeAnn { _mtaMeta = ShortMetadata s } -> do
              pure $ mkStr s

            MTreeAnn { _mtaMeta = AnnHashRef h } -> do
              getBlock sto h
                 >>= toMPlus
                 <&> LBS.toStrict
                 <&> TE.decodeUtf8
                 <&> mkStr

            _ -> mzero

      case (how, r) of
        ("parsed", Just (LitStrVal r0)) -> do


          let xs = parseTop r0
                     & fromRight mempty

          pure $ mkForm  "dict" xs

        _ -> pure $ fromMaybe nil r

    _ -> throwIO (BadFormException @c nil)

  entry $ bindMatch "hbs2:tree:metadata:create" $ \syn -> do

    case syn of

      args -> do
        error $ show $ pretty args

      (LitStrVal s : meta) -> do
        let lbs = fromString (Text.unpack s) :: LBS.ByteString
        h <- createTreeWithMetadata (metaFromSyntax meta) lbs
        pure $ mkStr (show $ pretty h)

      (ListVal [SymbolVal "from-file", StringLike fn ] : meta) -> do
        lbs <- liftIO $ LBS.readFile fn
        h <- createTreeWithMetadata (metaFromSyntax meta) lbs
        pure $ mkStr (show $ pretty h)

      (ListVal [SymbolVal "from-stdin"] : meta) -> do
        lbs <- liftIO $ LBS.getContents
        h <- createTreeWithMetadata (metaFromSyntax meta) lbs
        pure $ mkStr (show $ pretty h)

      _ -> throwIO (BadFormException @c nil)

  entry $ bindMatch "cbor:base58" $ \case
    [ LitStrVal x ] -> do
      pure $ mkForm "cbor:base58" [mkStr x]

    _ -> throwIO (BadFormException @c nil)


