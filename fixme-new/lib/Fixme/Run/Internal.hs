{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module Fixme.Run.Internal where

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


import Streaming.Prelude qualified as S

pattern IsSimpleTemplate ::  forall {c} . [Syntax c] -> Syntax c
pattern IsSimpleTemplate xs <- ListVal (SymbolVal "simple" : xs)

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


printEnv :: FixmePerks m => FixmeM m ()
printEnv = do
  g <- asks fixmeEnvGitDir >>= readTVarIO
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

  for_ g $ \git -> do
    liftIO $ print $ "fixme-git-dir" <+> dquotes (pretty git)

  dbPath <- asks fixmeEnvDbPath >>= readTVarIO
  liftIO $ print $ "fixme-state-path" <+> dquotes (pretty dbPath)

  (before,after) <- asks fixmeEnvCatContext >>= readTVarIO

  liftIO $ print $ "fixme-def-context" <+> pretty before <+> pretty after

  ma <- asks fixmeEnvMacro >>= readTVarIO <&> HM.toList

  for_ ma $ \(n, syn) -> do
    liftIO $ print $ parens ("define-macro" <+> pretty n <+> pretty syn)


exportToLog :: FixmePerks m => FilePath -> FixmeM m ()
exportToLog fn = do
  e <- getEpoch
  warn $ red "EXPORT-FIXMIES" <+> pretty fn
  sto <- compactStorageOpen @HbSync mempty fn
  fx <- selectFixmeThin ()
  for_ fx $ \(FixmeThin m) -> void $ runMaybeT do
    h <- HM.lookup "fixme-hash" m & toMPlus
    loaded <- lift (selectFixme (coerce h)) >>= toMPlus
    let what = Added e loaded
    let k = mkKey what
    get sto k >>= guard . isNothing
    put sto (mkKey what) (LBS.toStrict $ serialise what)
    warn $ red "export" <+> pretty h

  what <- selectStage

  for_ what $ \w -> do
    let k = mkKey w
    v0 <- get sto k <&> fmap (deserialiseOrFail @CompactAction .  LBS.fromStrict)
    case v0 of
      Nothing -> do
        put sto k (LBS.toStrict $ serialise w)

      Just (Left{}) -> do
        put sto k (LBS.toStrict $ serialise w)

      Just (Right prev) | getSequence w > getSequence prev -> do
        put sto k (LBS.toStrict $ serialise w)

      _ -> pure ()

  compactStorageClose sto

  cleanStage

importFromLog :: FixmePerks m => FilePath -> FixmeM m ()
importFromLog fn = do
  fset <- listAllFixmeHashes

  sto <- compactStorageOpen @HbSync readonly fn
  ks   <- keys sto

  toImport <- S.toList_ do
    for_ ks $ \k -> runMaybeT do
      v <- get sto k & MaybeT
      what <- deserialiseOrFail @CompactAction (LBS.fromStrict v) & toMPlus

      case what of
        Added _ fx  -> do
          let ha = hashObject @HbSync (serialise fx) & HashRef
          unless (HS.member ha fset) do
            warn $ red "import" <+> viaShow (pretty ha)
            lift $ S.yield (Right fx)
        w -> lift $ S.yield (Left $ fromRight mempty $ parseTop (show $ pretty w))

  withState $ transactional do
    for_ (rights  toImport) insertFixme

  let w = lefts toImport

  for_ w $ \x -> do
    liftIO $ print $ pretty x
  -- runTop (mconcat w)

  unless (List.null toImport) do
    updateIndexes

  compactStorageClose sto


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
        let subst = [ (mkId k, mkStr @C v) | (k,v) <- HM.toList attr ]
        let what = render (SimpleTemplate (inject  subst simple))
                      & fromRight "render error"

        liftIO $ hPutDoc stdout what

