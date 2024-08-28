module Fixme.Run where

import Prelude hiding (init)
import Fixme.Prelude hiding (indent)
import Fixme.Types
import Fixme.Config
import Fixme.State
import Fixme.Scan.Git.Local as Git
import Fixme.Scan as Scan
import Fixme.Log

import HBS2.Git.Local.CLI

import HBS2.Base58
import HBS2.Merkle
import HBS2.Data.Types.Refs
import HBS2.Storage
import HBS2.Storage.Compact
import HBS2.System.Dir
import DBPipe.SQLite hiding (field)
import Data.Config.Suckless

import Data.Aeson.Encode.Pretty as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Either
import Data.Maybe
import Data.HashSet qualified as HS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.Set qualified as Set
import Data.Generics.Product.Fields (field)
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Text.InterpolatedString.Perl6 (qc)
import Data.Coerce
import Control.Monad.Identity
import Lens.Micro.Platform
import System.Process.Typed
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import System.IO.Temp as Temp
import System.IO qualified as IO



runFixmeCLI :: FixmePerks m => FixmeM m a -> m a
runFixmeCLI m = do
  dbPath <- localDBPath
  git <- findGitDir
  env <- FixmeEnv
            <$>  newMVar ()
            <*>  newTVarIO mempty
            <*>  newTVarIO dbPath
            <*>  newTVarIO Nothing
            <*>  newTVarIO git
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO defCommentMap
            <*>  newTVarIO Nothing
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO defaultCatAction
            <*>  newTVarIO defaultTemplate
            <*>  newTVarIO mempty
            <*>  newTVarIO (1,3)

  -- FIXME: defer-evolve
  --   не все действия требуют БД,
  --   хорошо бы, что бы она не создавалась,
  --   если не требуется
  runReaderT ( setupLogger >> fromFixmeM (handle @_ @SomeException (err . viaShow) evolve >> m) ) env
                 `finally` flushLoggers
  where
    setupLogger = do
      setLogging @ERROR  $ toStderr . logPrefix "[error] "
      setLogging @WARN   $ toStderr . logPrefix "[warn] "
      setLogging @NOTICE $ toStdout . logPrefix ""
      pure ()

    flushLoggers = do
      silence

    -- FIXME: tied-fucking-context
    defaultCatAction = CatAction $ \dict lbs -> do
      LBS.putStr lbs
      pure ()

silence :: FixmePerks m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE


defaultTemplate :: HashMap Id FixmeTemplate
defaultTemplate = HM.fromList [ ("default", Simple (SimpleTemplate short)) ]
  where
    short = parseTop s & fromRight mempty
    s = [qc|
(trim 10  $fixme-key) " "
(align 6  $fixme-tag) " "
(trim 50  ($fixme-title))
(nl)
    |]


readConfig :: FixmePerks m => FixmeM m [Syntax C]
readConfig = do

  user <- userConfigs
  lo   <- localConfig

  w <- for (lo : user) $ \conf -> do
    try @_ @IOException (liftIO $ readFile conf)
      <&> fromRight mempty
      <&> parseTop
      <&> fromRight mempty

  pure $ mconcat w

init :: FixmePerks m => FixmeM m ()
init = do

  lo <- localConfigDir

  let lo0 = takeFileName lo

  mkdir lo
  touch (lo </> "config")

  let gitignore = lo </> ".gitignore"
  here <- doesPathExist gitignore

  unless here do
    liftIO $ writeFile gitignore $ show $
      vcat [ pretty ("." </> localDBName)
           ]

  notice $ yellow "run" <> line <> vcat [
      "git add" <+> pretty (lo0  </> ".gitignore")
    , "git add" <+> pretty (lo0  </> "config")
    ]


runTop :: FixmePerks m => [String] -> FixmeM m ()
runTop args = do

  forms <- parseTop (unlines $ unwords <$> splitForms args)
           & either  (error.show) pure

  -- pure ((unlines . fmap unwords . splitForms) what)
  --          >>= either (error.show) pure . parseTop

  let dict = makeDict @C do

       -- internalEntries

       entry $ bindMatch "--help" $ nil_ \case
         HelpEntryBound what -> helpEntry what
         [StringLike s]      -> helpList False (Just s)
         _                   -> helpList False Nothing

  conf <- readConfig

  run dict (conf <> forms) >>= eatNil display

  -- notice $ red "re-implementing fixme-new"
  -- read refchan
  -- execute settings from refchan
  -- read config


  -- execute config
  -- execute cli
  pure ()
  -- sc <- readConfig

  -- let s0 = fmap (parseTop . unwords) (splitForms what)
  --            & rights
  --            & mconcat

  -- runForms (sc <> s0)



