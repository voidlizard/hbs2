module HBS2.CLI.Run.Tree
  ( treeEntries
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

import Control.Monad.Except

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


