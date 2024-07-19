module HBS2.Git.Client.Config (getConfigDir, readConfig, hbs2Name) where

import HBS2.Git.Client.Prelude
import HBS2.Git.Client.App.Types

import HBS2.System.Dir
import HBS2.Git.Local.CLI

import Data.Either
import Text.InterpolatedString.Perl6 (qc)


data ConfigDirNotFound = ConfigDirNotFound
                         deriving stock (Show,Typeable,Generic)

instance HasErrorStatus ConfigDirNotFound where
  getStatus = const Failed

instance Exception ConfigDirNotFound

hbs2Name :: String
hbs2Name = "hbs21"

getConfigDir :: GitPerks m => m FilePath
getConfigDir = do
  git <- gitDir >>= orThrow ConfigDirNotFound

  let p = splitDirectories git & reverse

  if headMay p == Just ".git" then
    pure $ joinPath $ reverse (".hbs2-git" : drop 1 p)
  else do
    pure $ git </> ".hbs2-git"

readConfig :: (GitPerks m) => Bool -> m Config
readConfig canTouch = do
{- HLINT ignore "Functor law" -}
  confPath <- getConfigDir
  let confRoot = confPath </> "config"

  when canTouch do

    here <- doesPathExist confRoot

    unless here do
      mkdir confPath
      liftIO $ writeFile confRoot defConf

  try @_ @SomeException (liftIO (readFile confRoot))
    <&> fromRight mempty
    <&> parseTop
    <&> fromRight mempty


defConf :: String
defConf = [qc|;; hbs2-git config file
; those branches will be replicated by default
export include "refs/heads/master"
export include "refs/heads/main"
export exclude "refs/heads/*"
export tags
|]
