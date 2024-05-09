{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
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
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as Text
import Text.InterpolatedString.Perl6 (qc)


{- HLINT ignore "Functor law" -}

pattern Init :: forall {c}. Syntax c
pattern Init <- ListVal [SymbolVal "init"]

pattern ScanGitLocal :: forall {c}. Syntax c
pattern ScanGitLocal <- ListVal [SymbolVal "scan-git"]

pattern FixmeFiles :: forall {c} . [FilePattern] -> Syntax c
pattern FixmeFiles e  <- ListVal (SymbolVal "fixme-files" : (fileMasks -> e))

pattern FixmeGitScanFilterDays :: forall {c}. Integer -> Syntax c
pattern FixmeGitScanFilterDays d <- ListVal [ SymbolVal "fixme-git-scan-filter-days", LitIntVal d ]

fileMasks :: [Syntax c] -> [FilePattern]
fileMasks what = [ show (pretty s) | s <- what ]

binName :: FixmePerks m => m FilePath
binName = liftIO getProgName

localConfigDir :: FixmePerks m => m FilePath
localConfigDir = do
  p <- pwd
  b <- binName
  pure (p </> ("." <> b))

localConfig:: FixmePerks m => m FilePath
localConfig = localConfigDir <&> (</> "config")


readConfig :: FixmePerks m => FixmeM m [Syntax C]
readConfig = do
  localConfig
     >>= liftIO . readFile
     <&> parseTop
     <&> fromRight mempty

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

listCommits :: FixmePerks m => FixmeM m [GitHash]
listCommits = do
  let gd = ""

  days <- asks fixmeEnvGitScanDays
            >>= readTVarIO
            <&> fmap ( \x -> "--since" <+> squotes (pretty x <+> "days ago"))
            <&> fromMaybe mempty
            <&> show

  let cmd = [qc|git log --all --format="%H" {days}|]

  -- FIXME: git-dir
  gitRunCommand cmd
    <&> fromRight mempty
    <&> mapMaybe (headMay . LBS8.words) . LBS8.lines
    <&> mapMaybe (fromStringMay @GitHash . LBS8.unpack)


listBlobs :: FixmePerks m => GitHash -> m [(FilePath,GitHash)]
listBlobs co = do
  -- FIXME: git-dir
  gitRunCommand [qc|git ls-tree -r -l -t {pretty co}|]
    <&> fromRight mempty
    <&> fmap LBS8.words . LBS8.lines
    <&> mapMaybe
          (\case
             [a,"blob",h,_,fn] -> (LBS8.unpack fn,) <$> fromStringMay @GitHash (LBS8.unpack h)
             _                 -> Nothing)

filterBlobs :: FixmePerks m
            => [(FilePath,GitHash)]
            -> FixmeM m [(FilePath,GitHash)]

filterBlobs xs = do
  pat <- asks fixmeEnvFileMask >>= readTVarIO <&> fmap (True,)
  let src = [ ((f,h),f) | (f,h) <- xs ]
  let r = [(h,f) | (_,(f,h),_) <- matchMany pat src] & HM.fromList & HM.toList
  pure $ [ (b,a) | (a,b) <- r ]

scanGitLocal :: FixmePerks m => Maybe FilePath -> FixmeM m ()
scanGitLocal p =  do
  debug $ yellow "scan for fixmies, wtf?"
  co <- listCommits
  for_ co $ \c -> do
    blobs <- listBlobs c >>= filterBlobs
    debug $ vcat (fmap pretty blobs)

help :: FixmePerks m => m ()
help = do
  notice "this is help  message"

run :: FixmePerks m => [String] -> FixmeM m ()
run what = do

  sc <- readConfig

  let s0 = parseTop (unwords what)
            & fromRight mempty
            & (sc <>)

  for_ s0 $ \s -> do

    case s of

      FixmeFiles xs -> do
        t <- asks fixmeEnvFileMask
        atomically (modifyTVar t (<> xs))

      FixmeGitScanFilterDays d -> do
        t <- asks fixmeEnvGitScanDays
        atomically (writeTVar t (Just d))

      Init         -> init

      ScanGitLocal -> scanGitLocal Nothing

      w         -> err (pretty w)


