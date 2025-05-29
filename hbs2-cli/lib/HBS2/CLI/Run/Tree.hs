module HBS2.CLI.Run.Tree
  ( treeEntries
  ) where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.GroupKey as G
import HBS2.CLI.Run.Internal.Merkle

import HBS2.Defaults

import HBS2.Base58
import HBS2.Data.Types.Refs
import HBS2.Merkle
import HBS2.Data.Detect
import HBS2.System.Dir
import HBS2.Storage
import HBS2.Storage.Operations.Class
import HBS2.Storage.Operations.ByteString
import HBS2.Storage.Operations.Missed
import HBS2.Storage.Operations.Delete

import HBS2.Net.Auth.Schema()

import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix

import Data.ByteString.Lazy qualified as LBS
import Data.Coerce
import Data.Text qualified as Text
import Control.Monad.Except
import Codec.Serialise
import Streaming.Prelude qualified as S

pattern GroveHashes :: forall {c}. [HashRef] -> [Syntax c]
pattern GroveHashes hashes <- ( groveHashes -> hashes )

groveHashes :: [Syntax c] -> [HashRef]
groveHashes = \case
  [ ListVal (HashLikeList hashes) ] -> hashes
  HashLikeList hashes -> hashes
  _ -> mempty

treeEntries :: forall c m . ( IsContext c
                                , MonadUnliftIO m
                                , Exception (BadFormException c)
                                , HasStorage m
                                , HasClientAPI StorageAPI UNIX m
                                ) => MakeDictM c m ()
treeEntries = do

  brief "reads merkle tree data from storage"
    $ args [arg "string" "tree"]
    $ desc "hbs2:tree:read HASH"
    $ returns "bytestring" "data"
    $ entry $ bindMatch "hbs2:tree:read" $ \case
       [ HashLike h ] -> lift do
        sto <- getStorage

        co <- runExceptT (getTreeContents sto h)
                >>= orThrowPassIO

        mkOpaque co

       _ -> throwIO (BadFormException @c nil)


  brief "reads merkle tree data from storage to stdout"
    $ args [arg "string" "tree"]
    $ desc "hbs2:tree:read:stdout HASH"
    $ returns "nil" ""
    $ entry $ bindMatch "hbs2:tree:read:stdout" $ nil_ \case
       [ HashLike h ] -> lift do
        sto <- getStorage

        runExceptT (getTreeContents sto h)
                >>= orThrowPassIO
                >>= liftIO . LBS.putStr

       _ -> throwIO (BadFormException @c nil)


  brief "creates a 'grove' -- an annotated hashref list"
    $ args [arg "list of hashes" "trees"]
    $ desc [qc|hbs2:grove creates a 'grove' - merkle tree of list of hashes of merkle trees
It's just an easy way to create a such thing, you may browse it by hbs2 cat -H
|]
    $ returns "hash" "string"
    $ entry $ bindMatch "hbs2:grove" $ \case
        HashLikeList hashes@(x:_) -> lift do
          sto <- getStorage
          let pt = toPTree (MaxSize defHashListChunk) (MaxNum defTreeChildNum) hashes
          mkSym . show . pretty <$> liftIO (makeMerkle 0 pt $ \(_,_,bss) -> do
                  void $ putBlock sto bss)

        _ -> throwIO (BadFormException @c nil)

  entry $ bindMatch "hbs2:grove:annotated" $ \case
    (ListVal ann : GroveHashes hashes) -> lift do
      sto <- getStorage

      let pt = toPTree (MaxSize defHashListChunk) (MaxNum defTreeChildNum) hashes

      tree <- liftIO (makeMerkle 0 pt $ \(_,_,bss) -> do
                void $ putBlock sto bss)

      block <- getBlock sto tree
                  >>= orThrow MissedBlockError
                  <&> deserialiseOrFail @(MTree [HashRef])
                  >>= orThrow UnsupportedFormat

      let kwa = Text.unlines $ fmap (Text.pack . show . pretty) ann
      let mann = MTreeAnn (ShortMetadata kwa) NullEncryption block

      r <- putBlock sto (serialise mann)
              >>= orThrowUser "can't write tree"
              <&> HashRef

      pure $ mkSym (show $ pretty r)

    _ -> throwIO (BadFormException @c nil)


  entry $ bindMatch "hbs2:tree:missed" $ \case
    [HashLike href] -> do
      sto <- getStorage
      findMissedBlocks sto href
                <&> mkList . fmap (mkStr @c . show . pretty)

    _ -> throwIO (BadFormException @c nil)


  entry $ bindMatch "hbs2:tree:refs" $ \case
    [HashLike href] -> do
      sto <- getStorage

      blk <- getBlock sto (coerce href)
               >>= orThrow MissedBlockError

      let refs = extractBlockRefs (coerce href) blk

      pure $ mkList @c (fmap (mkStr . show . pretty) refs)

    _ -> throwIO (BadFormException @c nil)


  brief "shallow scan of a block/tree" $
    entry $ bindMatch "hbs2:tree:scan" $ \case
      [HashLike href] -> do
        sto <- getStorage

        r <- S.toList_ $
                deepScan ScanShallow (const none) (coerce href) (getBlock sto) $ \ha -> S.yield ha

        -- let refs = extractBlockRefs (coerce href) blk
        pure $ mkList @c (fmap (mkSym . show . pretty . HashRef) r)

      _ -> throwIO (BadFormException @c nil)

  brief "delete tree" $
    entry $ bindMatch "hbs2:tree:delete" $ nil_ \case
      [HashLike href] -> do
        sto <- getStorage
        deleteMerkleTree sto href

      _ -> throwIO (BadFormException @c nil)


  brief "shallow scan of a block/tree" $
    entry $ bindMatch "hbs2:tree:scan:deep" $ \case
      [HashLike href] -> do
        sto <- getStorage

        r <- S.toList_ $
                deepScan ScanDeep (const none) (coerce href) (getBlock sto) $ \ha -> S.yield ha

        -- let refs = extractBlockRefs (coerce href) blk
        pure $ mkList @c (fmap (mkSym . show . pretty . HashRef) r)

      _ -> throwIO (BadFormException @c nil)


  brief "shallow scan of a block/tree" $
    entry $ bindMatch "hbs2:tree:scan:deep:stdout" $ nil_ \case
      [HashLike href] -> do
        sto <- getStorage

        deepScan ScanDeep (const none) (coerce href) (getBlock sto) $ \ha -> do
          liftIO  $ print $ pretty ha

      _ -> throwIO (BadFormException @c nil)


