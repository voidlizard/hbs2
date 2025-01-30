module HBS2.Git3.Repo.Fork (forkEntries) where

import HBS2.Git3.Prelude
import HBS2.Git3.State
import HBS2.Git3.Import
import HBS2.Git3.Repo.Init
import HBS2.Git3.Git
import HBS2.Data.Detect

import HBS2.Data.Log.Structured

import HBS2.CLI.Run.Internal.Merkle (createTreeWithMetadata)
-- import HBS2.CLI.Run.RefLog (mkRefLogUpdateFrom)

import HBS2.System.Dir

import HBS2.Git3.Config.Local
import HBS2.Git3.Logger

import Data.Config.Suckless.Script
import Data.Config.Suckless.Almost.RPC

import Data.Maybe

import Codec.Compression.Zstd.Streaming qualified as ZstdS
import Codec.Compression.Zstd.Streaming (Result(..))
import Data.ByteString.Builder as Builder
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.ByteString (ByteString)
import Data.Fixed
import Data.HashPSQ qualified as HPSQ
import Data.HashPSQ (HashPSQ)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.List (sortBy)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import Lens.Micro.Platform
import Streaming.Prelude qualified as S
import System.IO (hPrint)
import System.IO qualified as IO
import System.IO.Temp as Temp
import UnliftIO.Concurrent

import Text.InterpolatedString.Perl6 (qc)

forkEntries :: forall m . (HBS2GitPerks m) => Id -> MakeDictM C (Git3 m) ()
forkEntries prefix = do

  brief "forks hbs2-git repository"
    $ desc ("All new repo creation boilerplate:" <> line
             <> "creates a new sign key,"
             <+> "creates a new lww reference,"
             <+> "adds this key to hbs2-keyman," <> line
             <> "creates default repo manifest")
    $ args [ arg "key" "repo-ref"
           ]
    $ examples [qc|
hbs2-git repo:fork EvP3kskPVuKuKVMUc3LnfdW7GcFYjz6f5fFU1EGzrdgk
|]  $
      entry $ bindMatch (prefix <> "fork") $ nil_ $ \case
        [ SignPubKeyLike forked ] -> lift do

          connectedDo do
            waitRepo Nothing forked
            importGitRefLog

          -- hereGit <- gitDir

          -- when (isJust hereGit) do
          --   die "This is an existing git repo. Start from scratch, please. Fork operation aborted"

          -- debug $ "call fucking initRepo" <+> pretty [newRepoOpt]

          -- env <- ask
          -- withGit3Env env do
          -- initRepo [newRepoOpt]

          -- envNew <- nullGit3Env
          -- none
          -- connectedDo do
          --   notice "SHIT!"
          --   none

            -- newRepo <- getGitRepoKey >>= orThrowUser "can't create new repo"
            -- notice $ yellow "new repo key" <+> pretty (AsBase58 newRepo)

        _ -> throwIO $ BadFormException @C nil


