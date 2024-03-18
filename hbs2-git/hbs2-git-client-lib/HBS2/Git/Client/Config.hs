module HBS2.Git.Client.Config (getConfigDir, readConfig, getManifest, hbs2Name) where

import HBS2.Git.Client.Prelude
import HBS2.Git.Client.App.Types

import HBS2.System.Dir
import HBS2.Git.Local.CLI

import Data.List qualified as L
import Data.Text qualified as Text
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

getManifest :: GitPerks m => m (Text, Text, Maybe Text)
getManifest = do
  dir <- getConfigDir
  let mf = dir </> "manifest"

  let defname = takeFileName (takeDirectory dir) & Text.pack
  let defbrief = "n/a"

  content <- liftIO (try @_ @IOException $ readFile mf)
               <&> fromRight ""

  let txt = if L.null content then Nothing else Just (Text.pack content)

  -- FIXME: size-hardcode
  let header  = lines (take 1024 content)
                   & takeWhile ( not . L.null )
                   & unlines
                   & parseTop
                   & fromRight mempty

  let name = lastDef defname [ n | ListVal [ SymbolVal "name:", LitStrVal n ] <- header ]
  let brief = lastDef defbrief [ n | ListVal [ SymbolVal "brief:", LitStrVal n ] <- header ]

  pure (name,brief,txt)

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
