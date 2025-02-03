{-# Language MultiWayIf #-}

module HBS2.CLI.Run.MetaData
  ( metaDataEntries
  , createTreeWithMetadata
  , getTreeContents
  , getGroupKeyHash
  ) where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.GroupKey as G
import HBS2.CLI.Run.Internal.Merkle

import HBS2.Data.Types.Refs
import HBS2.Merkle
import HBS2.System.Dir
import HBS2.Storage
import HBS2.Storage.Operations.ByteString

import HBS2.Net.Auth.Schema()

import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix

import Codec.Serialise
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy (ByteString)
import Data.Either
import Data.Set qualified as Set
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Data.Text.Encoding qualified as TE
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO

import Magic.Data
import Magic.Init       (magicLoadDefault,magicOpen)
import Magic.Operations (magicFile)

{- HLINT ignore "Functor law" -}

data CreateMetaDataOpt =
    Auto
  | Stdin
  | Encrypted String
  | MetaDataEntry Id String
  | MetaDataFile FilePath
  deriving stock (Eq,Ord,Show,Data,Generic)

txt :: Pretty  a => a -> Text
txt a = Text.pack (show $ pretty a)

metaFromSyntax :: [Syntax c] -> HashMap Text Text
metaFromSyntax syn =
  HM.fromList [ (t k, t v) | (ListVal [ k, v ]) <- syn ]
  where
    t x = Text.pack (show $ pretty x)


type ForMetadata c m = ( IsContext c
                       , MonadUnliftIO m
                       , Exception (BadFormException c)
                       , HasStorage m
                       , HasClientAPI StorageAPI UNIX m
                       )

metaDataEntries :: forall c m . ( ForMetadata c m
                                ) => MakeDictM c m ()
metaDataEntries = do

  brief "update group key for tree"
    $ args [arg "string" "tree", arg "list" "update-ops"]
    $ desc ( "update-ops is a list of pairs, like" <> line
               <> indent 4 ( parens ("list"
                     <+> indent 2 (vcat [ parens "remove . PUBLIC-KEY-ID"
                                        , parens "add . PUBLIC-KEY-ID"
                        ]))))
    $ returns "string" "new-tree-hash"
    $ examples [qc|

(define gk (hbs2:groupkey:load 6XJGpJszP6f68fmhF17AtJ9PTgE7BKk8RMBHWQ2rXu6N))

(hbs2:groupkey:update gk
      (list (remove . CcRDzezX1XQdPxRMuMKzJkfHFB4yG7vGJeTYvScKkbP8)
            (add . EiwWxY3xwTfnLKJdzzxNMZgA2ZvYAZd9e8B8pFeCtnrn)))
    |]
    $ entry $ bindMatch "hbs2:tree:metadata:update-gk" $ \case
      [StringLike tree, ListVal ins] -> do

        ha <- orThrowUser "invalid hash" (fromStringMay tree)

        -- 1. load-group-key
        (gkh', headBlk) <- getGroupKeyHash ha

        gkh <- orThrowUser "not encrypted" gkh'

        gk <- loadGroupKey gkh
                >>= orThrowUser "can't load gk"

        gk1 <- modifyGroupKey gk ins

        sto <- getStorage
        gk1h <- writeAsMerkle sto (serialise gk1)

        case headBlk of
          w@(MTreeAnn { _mtaCrypt = EncryptGroupNaClSymm _ nonce }) -> do
            let w1 = w { _mtaCrypt = EncryptGroupNaClSymm gk1h nonce }

            h <- putBlock sto (serialise w1)
                   >>= orThrowUser "can't put block"

            pure $ mkStr (show $ pretty h)

          _ -> pure nil

      _ -> throwIO (BadFormException @c nil)

  brief "get group key from encrypted tree"
    $ args [arg "string" "tree-hash"]
    $ returns "group-key-hash" "string"
    $ examples [qc|

(hbs2:tree:metadata:get-gk 7J2BZYskBjmDsWZHvVoGGorZDrFYkbbQweRauaYGSTNd)
5fshZRucawt47YJLuD1rVXRez2dcvCbz17m69YyduTEm

    |]
    $ entry $ bindMatch "hbs2:tree:metadata:get-gk" $ \case
      [ StringLike hash ] -> flip runContT pure do

        (gk,_) <- lift $ getGroupKeyHash (fromString hash)

        case gk of
          Just h -> pure $ mkStr (show $ pretty h)
          _      -> pure nil

      _ -> throwIO (BadFormException @c nil)

  brief "get metadata from tree"
    $ args [arg "symbol?" "method", arg "string" "tree-hash"]
    $ returns "group-key-hash" "string"
    $ desc ( opt "symbol?" ":parsed" <+> "return metadata as dict" <> line
              <> "if other value or absense then return metadata as string"
           )
    $ examples [qc|

(hbs2:tree:metadata:get 7J2BZYskBjmDsWZHvVoGGorZDrFYkbbQweRauaYGSTNd)
((mime-type: "text/plain; charset=us-ascii") (file-name: "qqq.txt"))

(hbs2:tree:metadata:get :raw 7J2BZYskBjmDsWZHvVoGGorZDrFYkbbQweRauaYGSTNd
mime-type: "text/plain; charset=us-ascii"
file-name: "qqq.txt"
    |]
    $ entry $ bindMatch "hbs2:tree:metadata:get"
    $ \case
     [ StringLike hash ] -> do

        r <- flip runContT pure do

          sto <- getStorage

          runMaybeT do

            headBlock <- getBlock sto (fromString hash)
                           >>= toMPlus
                           <&> deserialiseOrFail @(MTreeAnn [HashRef])
                           >>= toMPlus

            case headBlock of
              MTreeAnn { _mtaMeta = ShortMetadata s } -> do
                pure $ mkStr s

              MTreeAnn { _mtaMeta = AnnHashRef h, _mtaCrypt = NullEncryption } -> do
                getBlock sto h
                   >>= toMPlus
                   <&> LBS.toStrict
                   <&> TE.decodeUtf8
                   <&> mkStr

              MTreeAnn { _mtaMeta = AnnHashRef h } -> do
                getBlock sto h
                   >>= toMPlus
                   <&> deserialiseOrFail @(SmallEncryptedBlock AnnMetaData)
                   >>= toMPlus
                   >>= lift . lift . G.decryptBlock sto
                   <&> \case
                    ShortMetadata s -> mkStr s
                    _ -> nil

              _ -> mzero

        maybe1 r (pure nil) $ \case
          TextLike r0 -> do

            let xs = parseTop r0
                       & either mempty (fmap fixContext)

            pure $ mkList xs

          _ -> pure $ fromMaybe nil r

     _ -> throwIO (BadFormException @c nil)


  let metadataCreateMan = brief "creates a tree with metadata"
  let kw = arg "kw" "opts"

  metadataCreateMan $ args [kw, arg "string" "filename"] $
    entry $ bindMatch "hbs2:tree:metadata:file" $ \case
      [ syn@(ListVal{}), StringLike fn ] -> do
          meta0 <- liftIO do
            magic <- magicOpen [MagicMimeType,MagicMime,MagicMimeEncoding]
            magicLoadDefault magic
            mime <- magicFile magic fn
            pure $ HM.fromList [ ("file-name", Text.pack (takeFileName fn))
                               , ("mime-type", Text.pack mime)
                               ]
          doCreateMetadataTree meta0 syn (liftIO $ LBS.readFile fn)

      _ -> throwIO (BadFormException @c nil)

  metadataCreateMan $ args [kw] $
    entry $ bindMatch "hbs2:tree:metadata:stdin" $ \case
        [syn@(ListVal{})] -> do
          doCreateMetadataTree mempty syn (liftIO LBS.getContents)

        _ -> throwIO (BadFormException @c nil)

  metadataCreateMan $ args [kw, arg "string" "input"] $
    entry $ bindMatch "hbs2:tree:metadata:string" $ \case
        [ syn@(ListVal{}), TextLike content ] -> do
          -- liftIO $ TIO.putStr content
          doCreateMetadataTree mempty syn (pure $ LBS.fromStrict $ TE.encodeUtf8 content)

        _ -> throwIO (BadFormException @c nil)

  entry $ bindMatch "cbor:base58" $ \case
    [ LitStrVal x ] -> do
      pure $ mkForm "cbor:base58" [mkStr x]

    _ -> throwIO (BadFormException @c nil)


groupKeyFromSyntax :: Syntax c -> Maybe HashRef
groupKeyFromSyntax = \case
  ListVal es -> headMay [ v | ListVal [ TextLike "gk", HashLike v ] <- es ]
  _ -> Nothing

loadGroupKeyFromSyntax :: ( ForMetadata c m )
                       => Syntax c
                       -> RunM c m (Maybe (GroupKey 'Symm 'HBS2Basic))

loadGroupKeyFromSyntax syn = runMaybeT do
  hash <- groupKeyFromSyntax syn & toMPlus
  toMPlus =<< lift (loadGroupKey hash)

metadataFromSyntax :: Syntax c -> HashMap Text Text
metadataFromSyntax = \case
  ListVal es -> HM.fromList [ (k,v) | ListVal [ TextLike k, TextLike v] <- es, k /= "gk" ]
  _ -> mempty


doCreateMetadataTree :: forall c m . ForMetadata c m
                     => HashMap Text Text
                     -> Syntax c
                     -> m ByteString
                     -> RunM c m (Syntax c)
doCreateMetadataTree meta0 syn getLbs = do
  let meta = metadataFromSyntax syn
  let gkh  = groupKeyFromSyntax syn

  gk <- loadGroupKeyFromSyntax syn

  when (isJust gkh && isNothing gk) do
    throwIO (GroupKeyNotFound 1)

  sto <- getStorage

  lbs <- lift getLbs

  href <- lift (createTreeWithMetadata sto  gk (meta0 <> meta) lbs)
             >>= orThrow StorageError

  pure $ mkStr (show $ pretty href)



