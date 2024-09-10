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
import Data.Config.Suckless.Script.File

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
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory (getModificationTime)


import Streaming.Prelude qualified as S

pattern IsSimpleTemplate ::  forall {c} . [Syntax c] -> Syntax c
pattern IsSimpleTemplate xs <- ListVal (SymbolVal "simple" : xs)

{- HLINT ignore "Functor law" -}

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
  excl  <- asks fixmeEnvFileExclude >>= readTVarIO
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

  for_ excl $ \m -> do
    liftIO $ print $ "fixme-exclude" <+> dquotes (pretty m)

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


scanFiles :: FixmePerks m => FixmeM m [Fixme]
scanFiles = do
  w <- fixmeWorkDir
  incl <- asks fixmeEnvFileMask >>= readTVarIO
  excl <- asks fixmeEnvFileExclude >>= readTVarIO

  keys <- newTVarIO (mempty :: HashMap Text Integer)

  S.toList_ do

    glob incl excl w  $ \fn -> do

      ts <- liftIO $ getModificationTime fn <&> round . utcTimeToPOSIXSeconds

      let fnShort = makeRelative w fn

      lbs <- liftIO (try @_ @IOException $ LBS.readFile fn)
               <&> fromRight mempty

      fxs0 <- lift $ scanBlob (Just fn) lbs

      for_ fxs0 $ \fme -> do
        let key = fromString (fnShort <> "#") <> coerce (fixmeTitle fme) <> ":" :: Text
        atomically $ modifyTVar keys (HM.insertWith (+) key 1)
        no <- readTVarIO keys <&> HM.lookup key <&> fromMaybe 0
        let keyText = key <> fromString (show no)
        let keyHash = FixmeKey $ fromString $ show $ pretty $ hashObject @HbSync (serialise keyText)
        let f2 = mempty { fixmeTs = Just (fromIntegral ts)
                        , fixmeKey = keyHash
                        , fixmeAttr = HM.fromList
                            [   ( "fixme-key-string", FixmeAttrVal keyText)
                              , ( "file", FixmeAttrVal (fromString fnShort))
                            ]
                        , fixmePlain = fixmePlain fme
                        }
        let fmeNew = (fme <> f2) & fixmeDerivedFields
        S.yield fmeNew

      pure True


report :: (FixmePerks m, HasPredicate q) => Maybe FilePath -> q -> FixmeM m ()
report t q = do

  tpl <- asks fixmeEnvTemplates >>= readTVarIO
            <&> HM.lookup (maybe "default" fromString t)

  fxs <- listFixme q

  case tpl of
    Nothing ->
      liftIO $ LBS.putStr $ Aeson.encodePretty (fmap fixmeAttr fxs)

    Just (Simple (SimpleTemplate simple)) -> do
      for_ fxs $ \(Fixme{..}) -> do
        let subst = [ (mkId k, mkStr @C v) | (k,v) <- HM.toList fixmeAttr ]
        let what = render (SimpleTemplate (inject  subst simple))
                      & fromRight "render error"

        liftIO $ hPutDoc stdout what


import_ :: FixmePerks m => FixmeM m ()
import_ = do
  fxs0 <- scanFiles

  fxs <- flip filterM fxs0 $ \fme -> do
           let fn = fixmeGet "file" fme <&> Text.unpack . coerce
           seen <- maybe1 fn (pure False) selectIsAlreadyScanned
           pure (not seen)

  hashes <- catMaybes <$> flip runContT pure do
      p <- ContT $ bracket startGitHash stopProcess
      let files = mapMaybe (fixmeGet "file") fxs
                      & HS.fromList
                      & HS.toList
                      & fmap (Text.unpack . coerce)
      for files $ \f -> do
        mbHash <- lift $ gitHashPathStdin p f
        case mbHash of
          Just ha ->
            pure $ Just (f, ha)
          Nothing ->
            pure Nothing

  versioned <- listBlobs Nothing <&> HM.fromList
  let commited = HM.elems versioned & HS.fromList

  let blobs = HM.fromList hashes

  let isVersioned = maybe False (`HM.member` versioned)

  withState $ transactional do
    for_ fxs $ \fme -> do
      let fn = fixmeGet "file" fme <&> Text.unpack . coerce
      fmeRich <- lift $ maybe1 fn (pure mempty) (`getMetaDataFromGitBlame` fme)

      let blob = fn >>= flip HM.lookup blobs
                    >>= \b -> pure (fixmeSet "blob" (fromString (show $ pretty $ b)) mempty)

      notice $ "fixme" <+> pretty (fixmeKey fme) <+> pretty fn
      insertFixme (fromMaybe mempty blob <> fmeRich <> fme)

      -- TODO: add-scanned-only-on-commited
      --   $workflow: test
      --   поведение: если файл в гите И закоммичен -- то
      --   добавляем в сканированные.
      --
      --   если не в гите -- то добавляем в сканированные
      --
      --   иначе  не добавляем, wtf?
      --
      --   проверяем
      for_ fn $ \f -> do
        let add = not (isVersioned fn)
                   || maybe False (`HS.member` commited) (HM.lookup f blobs)

        when add do
          notice $ red "SCANNED" <+> pretty f
          insertScanned f

