{-# Language MultiWayIf #-}

module HBS2.CLI.Run.MetaData (metaDataEntries) where

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

import Codec.Serialise
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Data.ByteString.Lazy qualified as LBS
import Data.Either
import Data.Set qualified as Set
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Data.Text.Encoding qualified as TE
import Data.Text qualified as Text

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


metaDataEntries :: forall c m . (c ~ C, IsContext c, MonadUnliftIO m) => MakeDictM c m ()
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

        flip runContT pure do
          sto <- ContT withPeerStorage
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

(hbs2:tree:metadata:get :parsed 7J2BZYskBjmDsWZHvVoGGorZDrFYkbbQweRauaYGSTNd)
(dict (mime-type: "text/plain; charset=us-ascii") (file-name: "qqq.txt"))

(hbs2:tree:metadata:get :raw 7J2BZYskBjmDsWZHvVoGGorZDrFYkbbQweRauaYGSTNd
mime-type: "text/plain; charset=us-ascii"
file-name: "qqq.txt"
    |]
    $ entry $ bindMatch "hbs2:tree:metadata:get"
    $ \case
      [ SymbolVal how, StringLike hash ] -> do

        r <- flip runContT pure do

          sto <- ContT withPeerStorage

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

        case (how, r) of
          ("parsed", Just (LitStrVal r0)) -> do


            let xs = parseTop r0
                       & fromRight mempty

            pure $ mkForm  "dict" xs

          _ -> pure $ fromMaybe nil r

      _ -> throwIO (BadFormException @c nil)

  brief "creates a merkle tree with metadata"
    $ returns "string" "hash"
    $ args [ arg "list-of-options" "..." ]
    $ desc ( "options:" <> line
             <> indent 4 (
                  vcat [ opt ":stdin" "read data from stdin"
                       , opt ":auto"  "create metadata from file using libmagic"
                       , opt  "[kw [encrypted group-key-hash]]" "encrypt metadata with given group key"
                       , opt  "dict" "custom metadata dictionary"
                       , opt  "filename : string-like" "file name, ignored if stdin option set"
                       ])
             )
    $ examples [qc|
Create not encrypted merkle tree for string from stdin without metadata

$ echo TEST | hbs2-cli hbs2:tree:metadata:create :stdin
7dGqTtoehsgn7bADcVTyp93tq2FfuQgtBuVvYL46jdyz

;; empty metadata

hbs2-cli hbs2:tree:metadata:get :raw 7dGqTtoehsgn7bADcVTyp93tq2FfuQgtBuVvYL46jdyz

Create merkle tree with custom metadata

$ echo TEST | hbs2-cli hbs2:tree:metadata:create :stdin [kw hello world]
2ASBLBPRUMrHoSkNYsRWwJQiiXuSGDZTaCXAdDTdeJY6

$ hbs2-cli hbs2:tree:metadata:get :raw 2ASBLBPRUMrHoSkNYsRWwJQiiXuSGDZTaCXAdDTdeJY6
hello: "world"

$ hbs2-cli hbs2:tree:metadata:create :auto ./lambda.svg
3fv5ym8NhY8zat37NaTvY9PDcwJqMLUD73ewHxtHysWg

$ hbs2-cli hbs2:tree:metadata:get :raw 3fv5ym8NhY8zat37NaTvY9PDcwJqMLUD73ewHxtHysWg
mime-type: "image/svg+xml; charset=us-ascii"
file-name: "lambda.svg"

Create encrypted tree metadata with a new groupkey

$ hbs2-cli [define pks [list EiwWxY3xwTfnLKJdzzxNMZgA2ZvYAZd9e8B8pFeCtnrn]] \
  and [define gk [hbs2:groupkey:store [hbs2:groupkey:create pks]]] \
  and [hbs2:tree:metadata:create :auto [kw :encrypted gk] ./lambda.svg]

BFLcbpNEqngsJ8gzx3ps4ETXfpUMGgjEETNEVgR18KG4

Check group key

$ hbs2-cli hbs2:tree:metadata:get-gk BFLcbpNEqngsJ8gzx3ps4ETXfpUMGgjEETNEVgR18KG4y

GixS4wssCD4x7LzvHve2JhFCghW1Hwia2tiGTfTTef1u

Check metadata

$ hbs2-cli hbs2:tree:metadata:get :raw BFLcbpNEqngsJ8gzx3ps4ETXfpUMGgjEETNEVgR18KG4y

mime-type: "image/svg+xml; charset=us-ascii"
file-name: "lambda.svg"

List group key

$ hbs2-cli hbs2:groupkey:list-public-keys [hbs2:groupkey:load GixS4wssCD4x7LzvHve2JhFCghW1Hwia2tiGTfTTef1u]
("EiwWxY3xwTfnLKJdzzxNMZgA2ZvYAZd9e8B8pFeCtnrn")
    |]
    $ entry $ bindMatch "hbs2:tree:metadata:create" $ \syn -> do
        case syn of

          args -> do
            opts' <- for args $ \case
              SymbolVal "stdin"     -> pure [Stdin]

              SymbolVal "auto"      -> pure [Auto]

              ListVal (SymbolVal "dict" : [ListVal [SymbolVal "encrypted", StringLike key]])
                -> do
                  pure [Encrypted key]

              ListVal (SymbolVal "dict" : w)  -> do
                pure [MetaDataEntry x y | ListVal [SymbolVal x, StringLike y] <- w ]

              StringLike rest  -> do
                pure [MetaDataFile rest]

              _ -> pure mempty

            let opts = mconcat opts' & Set.fromList
            let inFile = headMay [ x | MetaDataFile x <- universeBi opts ]

            lbs <- case (Set.member Stdin opts, inFile) of
                     (True, _) -> liftIO LBS.getContents
                     (False, Just fn) -> liftIO (LBS.readFile fn)
                     (_, Nothing) -> liftIO LBS.getContents

            meta0 <- if not (Set.member Auto opts) || isNothing inFile then
                       pure (mempty :: HashMap Text Text)
                     else liftIO do
                        let fn = fromJust inFile
                        magic <- magicOpen [MagicMimeType,MagicMime,MagicMimeEncoding]
                        magicLoadDefault magic
                        mime <- magicFile magic fn
                        pure $ HM.fromList [ ("file-name", Text.pack (takeFileName fn))
                                           , ("mime-type", Text.pack mime)
                                           ]

            let meta1  = HM.fromList [  (txt n, txt e) | MetaDataEntry n e <- universeBi opts ]

            let enc = headMay [ e | x@(Encrypted e) <- universeBi opts ]

            gk <- runMaybeT do
                    s <- toMPlus enc
                    g <- lift $ loadGroupKey (fromString s)
                    toMPlus g

            when (isJust enc && isNothing gk) do
              error $ show $  "Can't load group key" <+> pretty enc

            flip runContT pure do

              sto <- ContT withPeerStorage

              href <- lift (createTreeWithMetadata sto  gk (meta0 <> meta1) lbs)
                        `orDie` "encryption error"

              pure $ mkStr (show $ pretty href)

  entry $ bindMatch "cbor:base58" $ \case
    [ LitStrVal x ] -> do
      pure $ mkForm "cbor:base58" [mkStr x]

    _ -> throwIO (BadFormException @c nil)


