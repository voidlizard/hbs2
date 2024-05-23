{-# Language MultiWayIf #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module Fixme.Run where

import Prelude hiding (init)
import Fixme.Prelude hiding (indent)
import Fixme.Types
import Fixme.Config
import Fixme.State
import Fixme.Scan.Git.Local as Git
import Fixme.Scan as Scan

import HBS2.Git.Local.CLI

import HBS2.System.Dir
import DBPipe.SQLite hiding (field)
import Data.Config.Suckless

import Data.Aeson.Encode.Pretty as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Either
import Data.Maybe
import Data.HashSet qualified as HS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as Text
import Text.InterpolatedString.Perl6 (qc)
import Data.Coerce
import Control.Monad.Identity
import Lens.Micro.Platform
import System.Process.Typed
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe

import Streaming.Prelude qualified as S

{- HLINT ignore "Functor law" -}

pattern Init :: forall {c}. Syntax c
pattern Init <- ListVal [SymbolVal "init"]

pattern ScanGitLocal :: forall {c}. [ScanGitArgs] -> Syntax c
pattern ScanGitLocal e <- ListVal (SymbolVal "scan-git" : (scanGitArgs -> e))

pattern Update :: forall {c}. [ScanGitArgs] -> Syntax c
pattern Update e <- ListVal (SymbolVal "update" : (scanGitArgs -> e))

pattern ReadFixmeStdin :: forall {c}.  Syntax c
pattern ReadFixmeStdin <- ListVal [SymbolVal "read-fixme-stdin"]

pattern FixmeFiles :: forall {c} . [FilePattern] -> Syntax c
pattern FixmeFiles e  <- ListVal (SymbolVal "fixme-files" : (fileMasks -> e))


pattern FixmePrefix :: forall {c} . FixmeTag -> Syntax c
pattern FixmePrefix s <- ListVal [SymbolVal "fixme-prefix", fixmePrefix -> Just s]

pattern FixmeGitScanFilterDays :: forall {c}. Integer -> Syntax c
pattern FixmeGitScanFilterDays d <- ListVal [ SymbolVal "fixme-git-scan-filter-days", LitIntVal d ]



scanGitArgs :: [Syntax c] -> [ScanGitArgs]
scanGitArgs syn = [ w | ScanGitArgs w <- syn ]


fileMasks :: [Syntax c] -> [FilePattern]
fileMasks what = [ show (pretty s) | s <- what ]

fixmePrefix :: Syntax c -> Maybe FixmeTag
fixmePrefix = \case
  SymbolVal s -> Just (FixmeTag (coerce s))
  _ -> Nothing


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


runFixmeCLI :: FixmePerks m => FixmeM m a -> m a
runFixmeCLI m = do
  db <- newDBPipeEnv dbPipeOptsDef =<< localDBPath
  env <- FixmeEnv Nothing db
            <$>  newTVarIO mempty
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
            <*>  newTVarIO (1,3)

  runReaderT ( setupLogger >> fromFixmeM (evolve >> m) ) env
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



readFixmeStdin :: FixmePerks m => FixmeM m ()
readFixmeStdin = do
  what <- liftIO LBS8.getContents
  fixmies <- Scan.scanBlob Nothing what
  liftIO $ print $ vcat (fmap pretty fixmies)


list_ :: (FixmePerks m, HasPredicate a) => Maybe Id -> a -> FixmeM m ()
list_ tpl a = do
  tpl <- asks fixmeEnvTemplates >>= readTVarIO
            <&> HM.lookup (fromMaybe "default" tpl)

  fixmies <- selectFixmeThin a

  case tpl of
    Nothing-> do
      liftIO $ LBS.putStr $ Aeson.encodePretty fixmies

    Just (Simple (SimpleTemplate simple)) -> do
      for_ fixmies $ \(FixmeThin attr) -> do
        let subst = [ (mksym k, mkstr @C v) | (k,v) <- HM.toList attr ]
        let what = render (SimpleTemplate (inject  subst simple))
                      & fromRight "render error"

        liftIO $ hPutDoc stdout what


catFixmeMetadata :: FixmePerks m => Text -> FixmeM m ()
catFixmeMetadata = cat_ True

catFixme :: FixmePerks m => Text -> FixmeM m ()
catFixme = cat_ False

cat_ :: FixmePerks m => Bool -> Text -> FixmeM m ()
cat_ metaOnly hash = do

  (before,after)  <- asks fixmeEnvCatContext >>= readTVarIO
  gd  <- fixmeGetGitDirCLIOpt

  CatAction action <- asks fixmeEnvCatAction >>= readTVarIO

  void $ flip runContT pure do
    callCC \exit -> do

      mha <- lift $ selectFixmeHash hash

      ha <- ContT $ maybe1 mha (pure ())

      fme' <- lift $ selectFixme ha

      Fixme{..} <- ContT $ maybe1 fme' (pure ())

      when metaOnly do
        for_ (HM.toList fixmeAttr) $ \(k,v) -> do
          liftIO $ print $ (pretty k <+> pretty v)
        exit ()

      let gh' = HM.lookup "blob" fixmeAttr

      -- FIXME: define-fallback-action
      gh <- ContT $ maybe1 gh' none

      let cmd = [qc|git {gd} cat-file blob {pretty gh}|] :: String

      let start = fromMaybe 0 fixmeStart & fromIntegral  & (\x -> x - before) & max 0
      let bbefore = if start > before then before  + 1 else 1
      let origLen = maybe  0 fromIntegral fixmeEnd - maybe 0 fromIntegral fixmeStart & max 1
      let lno   = max 1 $ origLen + after + before

      let dict = [ (mksym k, mkstr @C   v) | (k,v) <- HM.toList fixmeAttr ]
                 <>
                 [ (mksym "before", mkstr @C (FixmeAttrVal $ Text.pack $ show bbefore))
                 ]

      debug (pretty cmd)

      w <- gitRunCommand cmd
            <&> either (LBS8.pack . show) id
            <&> LBS8.lines
            <&> drop start
            <&> take lno

      liftIO $ action  dict (LBS8.unlines w)

delete :: FixmePerks m => Text -> FixmeM m ()
delete txt = do
  acts <- asks fixmeEnvUpdateActions >>= readTVarIO

  void $ runMaybeT do

    ha <- toMPlus =<< lift (selectFixmeHash txt)
    let syn = mkLit @Text [qc|deleted "{pretty ha}"|]

    debug $ red "deleted" <+> pretty ha

    for_ acts $ \(UpdateAction what) -> do
      liftIO $ what (Literal noContext syn)


modify_ :: FixmePerks m => Text -> String -> String -> FixmeM m ()
modify_ txt a b = do
  acts <- asks fixmeEnvUpdateActions >>= readTVarIO

  void $ runMaybeT do

    ha <- toMPlus =<< lift (selectFixmeHash txt)
    let syn = mkLit @Text [qc|modified "{pretty ha}" "{a}" "{b}"|]

    debug $ red $ pretty syn

    for_ acts $ \(UpdateAction what) -> do
      liftIO $ what (Literal noContext syn)



printEnv :: FixmePerks m => FixmeM m ()
printEnv = do
  g <- asks fixmeEnvGitDir
  masks <- asks fixmeEnvFileMask >>= readTVarIO
  tags  <- asks fixmeEnvTags >>= readTVarIO
  days  <- asks fixmeEnvGitScanDays >>= readTVarIO
  comments1 <- asks fixmeEnvDefComments >>= readTVarIO <&> HS.toList

  comments2 <- asks fixmeEnvFileComments >>= readTVarIO
                 <&> HM.toList
                 <&> fmap  (over _2 HS.toList)

  attr <- asks fixmeEnvAttribs >>= readTVarIO <&> HS.toList
  vals <- asks fixmeEnvAttribValues >>= readTVarIO <&> HM.toList

  for_ tags $ \m -> do
    liftIO $ print $ "fixme-prefix" <+> pretty m

  for_ masks $ \m -> do
    liftIO $ print $ "fixme-files" <+> dquotes (pretty m)

  for_ days $ \d -> do
    liftIO $ print $ "fixme-git-scan-filter-days" <+> pretty d

  for_ comments1 $ \d -> do
    liftIO $ print $ "fixme-comments" <+> dquotes (pretty d)

  for_ comments2 $ \(ft, comm') -> do
    for_ comm' $ \comm -> do
      liftIO $ print $ "fixme-file-comments"
                  <+> dquotes (pretty ft) <+> dquotes (pretty  comm)

  for_ attr $ \a -> do
      liftIO $ print $ "fixme-attribs"
                  <+> pretty a

  for_ vals$ \(v, vs) -> do
      liftIO $ print $ "fixme-value-set" <+> pretty v <+> hsep (fmap pretty (HS.toList vs))


  (before,after) <- asks fixmeEnvCatContext >>= readTVarIO

  liftIO $ print $ "fixme-def-context" <+> pretty before <+> pretty after


help :: FixmePerks m => m ()
help = do
  notice "this is help  message"


splitForms :: [String] -> [[String]]
splitForms s0 = runIdentity $ S.toList_ (go mempty s0)
  where
    go acc ( "then" : rest ) = emit acc >> go mempty rest
    go acc ( "and" : rest ) = emit acc >> go mempty rest
    go acc ( x : rest ) = go ( x : acc ) rest
    go acc [] = emit acc

    emit = S.yield . reverse

sanitizeLog :: [Syntax c] -> [Syntax c]
sanitizeLog lls = flip filter lls $ \case
  ListVal (SymbolVal "deleted" : _) -> True
  ListVal (SymbolVal "modified" : _) -> True
  _ -> False

pattern Template :: forall {c}. Maybe Id -> [Syntax c] -> [Syntax c]
pattern Template w syn <- (mbTemplate  -> (w, syn))

mbTemplate :: [Syntax c] -> (Maybe Id, [Syntax c])
mbTemplate = \case
  ( SymbolVal "template" : StringLike w : rest ) -> (Just (fromString w), rest)
  other -> (Nothing, other)

pattern IsSimpleTemplate ::  forall {c} . [Syntax c] -> [Syntax c]
pattern IsSimpleTemplate xs <- [ListVal (SymbolVal "simple" : xs)]

run :: FixmePerks m => [String] -> FixmeM m ()
run what = do

  sc <- readConfig

  let s0 = fmap (parseTop . unwords) (splitForms what)
             & rights
             & mconcat

  runForms (sc <> s0)

  where

    runForms :: forall c m . (IsContext c, Data c, Data (Context c), FixmePerks m)
             => [Syntax c]
             -> FixmeM m ()
    runForms ss = for_  ss $ \s -> do

      debug $ pretty s

      case s of

        FixmeFiles xs -> do
          t <- asks fixmeEnvFileMask
          atomically (modifyTVar t (<> xs))

        FixmePrefix tag -> do
          t <- asks fixmeEnvTags
          atomically (modifyTVar t (HS.insert tag))

        FixmeGitScanFilterDays d -> do
          t <- asks fixmeEnvGitScanDays
          atomically (writeTVar t (Just d))

        ListVal [SymbolVal "fixme-file-comments", StringLike ft, StringLike b] -> do
          let co = Text.pack b & HS.singleton
          t <- asks fixmeEnvFileComments
          atomically (modifyTVar t (HM.insertWith (<>) (commentKey ft) co))

        ListVal (SymbolVal "fixme-comments" : StringLikeList xs) -> do
          t <- asks fixmeEnvDefComments
          let co = fmap Text.pack xs & HS.fromList
          atomically $ modifyTVar t (<> co)

        ListVal (SymbolVal "fixme-attribs" : StringLikeList xs) -> do
          ta <- asks fixmeEnvAttribs
          atomically $ modifyTVar ta (<> HS.fromList (fmap fromString xs))

        ListVal [SymbolVal "fixme-def-context", LitIntVal a, LitIntVal b] -> do
          t <- asks fixmeEnvCatContext
          atomically $ writeTVar t (fromIntegral a, fromIntegral b)

        ListVal [SymbolVal "fixme-pager", ListVal cmd0] -> do
          t <- asks fixmeEnvCatAction
          let action =  CatAction $ \dict lbs -> do

                let ccmd = case inject dict cmd0 of
                             (StringLike p : StringLikeList xs) -> Just (p, xs)
                             _  -> Nothing


                debug $ pretty ccmd

                maybe1 ccmd none $ \(p, args) -> do

                  let input = byteStringInput lbs
                  let cmd = setStdin input $ setStderr closed
                                           $ proc p args
                  void $ runProcess cmd

          atomically $ writeTVar t action

        ListVal (SymbolVal "fixme-value-set" : StringLike n : StringLikeList xs) -> do
          t <- asks fixmeEnvAttribValues
          let name = fromString n
          let vals = fmap fromString xs & HS.fromList
          atomically $ modifyTVar t (HM.insertWith (<>) name vals)

        Init         -> init

        ScanGitLocal args -> scanGitLocal args Nothing

        Update args -> scanGitLocal args Nothing

        ListVal (SymbolVal "list" : (Template n [])) -> do
          debug $ "list" <+> pretty n
          list_ n ()

        ListVal (SymbolVal "list" : (Template n whatever)) -> do
          debug $ "list" <+> pretty n
          list_ n whatever


        ListVal [SymbolVal "cat", SymbolVal "metadata", FixmeHashLike hash] -> do
          catFixmeMetadata hash

        ListVal [SymbolVal "cat", FixmeHashLike hash] -> do
          catFixme hash

        ListVal [SymbolVal "delete", FixmeHashLike hash] -> do
          delete hash

        ListVal [SymbolVal "modify", FixmeHashLike hash, StringLike a, StringLike b] -> do
          modify_ hash a b

        ListVal [SymbolVal "modified", TimeStampLike t, FixmeHashLike hash, StringLike a, StringLike b] -> do
          debug $ green $ pretty s
          updateFixme (Just t) hash (fromString a) (fromString b)

        ListVal [SymbolVal "modified", FixmeHashLike hash, StringLike a, StringLike b] -> do
          debug $ green $ pretty s
          updateFixme Nothing hash (fromString a) (fromString b)

        ListVal [SymbolVal "deleted", FixmeHashLike hash] -> do
          deleteFixme hash

        ReadFixmeStdin -> readFixmeStdin

        ListVal [SymbolVal "print-env"] -> do
          printEnv

        ListVal (SymbolVal "hello" : xs) -> do
          notice $ "hello" <+> pretty xs

        ListVal (SymbolVal "define-template" : SymbolVal who : IsSimpleTemplate xs) -> do
          trace $ "define-template" <+> pretty who <+> "simple" <+> hsep (fmap pretty xs)
          t <- asks fixmeEnvTemplates
          atomically $ modifyTVar t (HM.insert who (Simple (SimpleTemplate xs)))

        ListVal [SymbolVal "set-template", SymbolVal who, SymbolVal w] -> do
          templates <- asks fixmeEnvTemplates
          t <- readTVarIO templates
          for_ (HM.lookup w t) $ \tpl -> do
            atomically $ modifyTVar templates (HM.insert who tpl)

        -- FIXME: maybe-rename-fixme-update-action
        ListVal (SymbolVal "fixme-update-action" : xs) -> do
          debug $ "fixme-update-action" <+> pretty xs
          env <- ask
          t <- asks fixmeEnvUpdateActions
          let repl syn = [ ( "$1", syn ) ]
          let action = UpdateAction @c $ \syn -> do
                           liftIO (withFixmeEnv env (runForms (inject (repl syn) xs)))

          atomically $ modifyTVar t (<> [action])

        ListVal (SymbolVal "fixme-play-log-action" : xs) -> do
          debug $ "fixme-play-log-action" <+> pretty xs
          env <- ask
          t <- asks fixmeEnvReadLogActions
          let action = ReadLogAction @c $ \_ -> liftIO (withFixmeEnv env (runForms  xs))
          atomically $ modifyTVar t (<> [action])

        ListVal (SymbolVal "append-file" : StringLike fn : StringLikeList xs) -> do
          debug "append-file"
          liftIO $ for_ xs $ \x -> do
            appendFile  fn x
            appendFile fn "\n"

        ListVal [SymbolVal "play-log-file", StringLike fn] -> do
          debug $ yellow "play-log-file" <+> pretty fn
          -- FIXME: just-for-in-case-sanitize-input
          --   $workflow: done
          what <- try @_ @IOException (liftIO $ readFile fn)
                    <&> fromRight mempty
                    <&> parseTop
                    <&> fromRight mempty
                    <&> sanitizeLog

          env <- ask
          liftIO $ withFixmeEnv env (runForms what)

        ListVal [SymbolVal "no-debug"] -> do
          setLoggingOff @DEBUG

        ListVal [SymbolVal "silence"] -> do
          silence

        ListVal [SymbolVal "builtin:evolve"] -> do
          evolve

        ListVal [SymbolVal "builtin:cleanup-state"] -> do
          cleanupDatabase

        ListVal [SymbolVal "builtin:update-indexes"] -> do
          updateIndexes


        ListVal [SymbolVal "builtin:select-fixme-hash", FixmeHashLike x] -> do
          w <- selectFixmeHash x
          liftIO $ print $ pretty w

        ListVal [SymbolVal "trace"] -> do
          setLogging @TRACE (logPrefix "[trace] " . toStderr)
          trace "trace on"

        ListVal [SymbolVal "no-trace"] -> do
          trace "trace off"
          setLoggingOff @TRACE

        ListVal [SymbolVal "debug"] -> do
          setLogging @DEBUG  $ toStderr . logPrefix "[debug] "

        w         -> err (pretty w)


