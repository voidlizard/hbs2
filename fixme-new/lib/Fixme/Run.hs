{-# Language PatternSynonyms #-}
module Fixme.Run where

import Prelude hiding (init)
import Fixme.Prelude
import Fixme.Types
import Fixme.Scan.Git as Git

import HBS2.Git.Local.CLI

import HBS2.System.Dir

import Data.Config.Suckless

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Either
import System.Environment
import Data.Maybe
import Text.InterpolatedString.Perl6 (qc)
import Lens.Micro.Platform

{- HLINT ignore "Functor law" -}

pattern Init :: forall {c}. Syntax c
pattern Init <- ListVal [SymbolVal "init"]

pattern ScanGitLocal :: forall {c}. Syntax c
pattern ScanGitLocal <- ListVal [SymbolVal "scan-git"]

binName :: FixmePerks m => m FilePath
binName = liftIO getProgName

localConfigDir :: FixmePerks m => m FilePath
localConfigDir = do
  p <- pwd
  b <- binName
  pure (p </> ("." <> b))

localConfig:: FixmePerks m => m FilePath
localConfig = localConfigDir <&> (</> "config")

init :: FixmePerks m => FixmeM m ()
init = do
  lo <- localConfigDir

  let lo0 = takeFileName lo

  touch (lo </> "config")
  mkdir lo

  let gitignore = lo </> ".gitignore"
  here <- doesPathExist gitignore

  unless here do
    liftIO $ writeFile gitignore $ show $
      vcat [ "./state.db"
           ]

  notice $ yellow "run" <> line <> vcat [
      "git add" <+> pretty (lo0  </> ".gitignore")
    , "git add" <+> pretty (lo0  </> "config")
    ]

listCommits :: FixmePerks m => m [GitHash]
listCommits = do
  let gd = ""
  -- FIXME: git-dir
  gitRunCommand [qc|git rev-list --all|]
    <&> fromRight mempty
    <&> mapMaybe (headMay . LBS8.words) . LBS8.lines
    <&> mapMaybe (fromStringMay @GitHash . LBS8.unpack)


listBlobs :: FixmePerks m => GitHash -> m [(FilePath, GitHash)]
listBlobs co = do
  -- FIXME: git-dir
  gitRunCommand [qc|git ls-tree -r -l -t {pretty co}|]
    <&> fromRight mempty
    <&> fmap LBS8.words . LBS8.lines
    <&> mapMaybe
          (\case
             [a,_,h,_,fn] -> (LBS8.unpack fn,) <$> fromStringMay @GitHash (LBS8.unpack h)
             _                 -> Nothing)


scanGitLocal :: FixmePerks m => Maybe FilePath -> m ()
scanGitLocal p = do
  debug $ yellow "scan for fixmies, wtf?"
  co <- listCommits
  for_ co $ \c -> do
    blobs <- listBlobs c
    debug $ vcat (fmap pretty blobs)

help :: FixmePerks m => m ()
help = do
  notice "this is help  message"

run :: FixmePerks m => [String] -> FixmeM m ()
run what = do

  let s0 = parseTop (unwords what)
             & fromRight mempty

  debug $ pretty  s0

  case s0 of
    [Init]         -> init

    [ScanGitLocal] -> scanGitLocal Nothing


    _         -> help


