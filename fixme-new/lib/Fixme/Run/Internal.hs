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

import HBS2.Git.Local.CLI

import HBS2.OrDie
import HBS2.Data.Types.SignedBox
import HBS2.Base58
import HBS2.Merkle
import HBS2.Data.Types.Refs
import HBS2.Storage
import HBS2.Storage.Compact
import HBS2.Peer.Proto.RefChan
import HBS2.Peer.RPC.Client.RefChan
import HBS2.Storage.Operations.ByteString
import HBS2.System.Dir
import HBS2.Net.Auth.Credentials
import DBPipe.SQLite hiding (field)


import HBS2.KeyMan.Keys.Direct

import Data.Config.Suckless
import Data.Config.Suckless.Script.File

import Data.List.Split (chunksOf)
import Control.Applicative
import Data.Aeson.Encode.Pretty as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString qualified as BS
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
import Data.Text.Encoding (encodeUtf8)
import Text.InterpolatedString.Perl6 (qc)
import Data.Coerce
import Data.Word
import Control.Monad.Identity
import Lens.Micro.Platform
import System.Process.Typed
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Control.Concurrent.STM (flushTQueue)
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
           seen <- maybe1 fn (pure False) selectIsAlreadyScannedFile
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
      for_ fn $ \f -> do
        let add = not (isVersioned fn)
                   || maybe False (`HS.member` commited) (HM.lookup f blobs)

        when add do
          insertScannedFile f

cat_ :: FixmePerks m => Text -> FixmeM m ()
cat_ hash = do

  (before,after)  <- asks fixmeEnvCatContext >>= readTVarIO
  gd  <- fixmeGetGitDirCLIOpt

  CatAction action <- asks fixmeEnvCatAction >>= readTVarIO

  dir  <- fixmeWorkDir

  void $ flip runContT pure do
    callCC \exit -> do

      mha <- lift $ selectFixmeKey hash

      ha <- ContT $ maybe1 mha (pure ())

      fme' <- lift $ getFixme ha

      fx@Fixme{..} <- ContT $ maybe1 fme' (pure ())

      let gh' = HM.lookup "blob" fixmeAttr

      -- FIXME: define-fallback-action
      gh <- ContT $ maybe1 gh' none

      let cmd = [qc|git {gd} cat-file blob {pretty gh}|] :: String

      let start = fromMaybe 0 fixmeStart & fromIntegral  & (\x -> x - before) & max 0
      let bbefore = if start > before then before  + 1 else 1
      let origLen = maybe  0 fromIntegral fixmeEnd - maybe 0 fromIntegral fixmeStart & max 1
      let lno   = max 1 $ origLen + after + before

      let dict = [ (mkId k, mkStr @C   v) | (k,v) <- HM.toList fixmeAttr ]
                 <>
                 [ (mkId (FixmeAttrName "before"), mkStr @C (FixmeAttrVal $ Text.pack $ show bbefore))
                 ]

      debug (pretty cmd)

      w <- gitRunCommand cmd
              <&> either (const Nothing) Just

      maybe1 w none $ \lbs -> do
        let piece = LBS8.lines lbs & drop start & take lno
        liftIO $ action  dict (LBS8.unlines piece)
        exit ()

      let fallback = LBS8.unlines $ fmap (LBS8.fromStrict . encodeUtf8 . coerce) fixmePlain

      liftIO $ action  dict fallback



refchanExport :: FixmePerks m => Bool -> FixmeM m ()
refchanExport dry = do
  sto <- getStorage
  rchanAPI <- getClientAPI @RefChanAPI @UNIX


  chan <- asks fixmeEnvRefChan
           >>= readTVarIO
           >>= orThrowUser "refchan not set"

  au <- asks fixmeEnvAuthor
           >>= readTVarIO
           >>= orThrowUser "author's key not set"

  creds <- runKeymanClientRO $ loadCredentials au
             >>= orThrowUser "can't read credentials"


  let (pk,sk) = (view peerSignPk creds, view peerSignSk creds)

  withState do
    -- FIXME: select-only-really-missed-records
    --   сейчас всегда фуллскан. будет всё дольше и дольше
    --   с ростом количества записей. Нужно отбирать
    --   только такие элементы, которые реально отсутствуют
    --   в рефчане
    what <- select_ @_ @FixmeExported [qc|
      select distinct o,0,k,cast (v as text)
      from object obj
      where not exists (select null from scanned where hash = obj.nonce)
      order by o, k, v
      |]

    let chu  = chunksOf 10000 what

    flip runContT pure do

      for_ chu $ \x -> callCC \next -> do

        -- FIXME: encrypt-tree
        h  <- writeAsMerkle sto (serialise x)

        already <- lift $ lift $ selectIsAlreadyScanned (HashRef h)

        when already $ next ()

        let tx = AnnotatedHashRef Nothing (HashRef h)

        lift do

          let lbs = serialise tx

          let box = makeSignedBox @'HBS2Basic @BS.ByteString pk sk (LBS.toStrict lbs)

          warn $ "POST" <+> red "unencrypted!" <+> pretty (hashObject @HbSync (serialise box))

          unless dry do
            r <- callRpcWaitMay @RpcRefChanPropose (TimeoutSec 1) rchanAPI (chan, box)

            when (isNothing r) do
              err $ red "hbs2-peer rpc calling timeout"


refchanImport :: FixmePerks m => FixmeM m ()
refchanImport = do

  sto <- getStorage

  chan <- asks fixmeEnvRefChan
           >>= readTVarIO
           >>= orThrowUser "refchan not set"


  ttsmap  <- newTVarIO HM.empty
  accepts <- newTVarIO HM.empty

  tq <- newTQueueIO

  -- TODO: assume-huge-list
  scanned <- listAllScanned
  let isScanned = pure . not . (`HS.member` scanned)

  walkRefChanTx @UNIX isScanned chan $ \txh u -> do

    case  u of

      A (AcceptTran (Just ts) _ what) -> do
        debug $ red "ACCEPT" <+> pretty ts <+> pretty what
        atomically $ modifyTVar ttsmap (HM.insertWith max what (coerce @_ @Word64 ts))
        atomically $ modifyTVar accepts (HM.insertWith (<>) what (HS.singleton txh))

      A _  -> none

      P orig (ProposeTran _ box) -> void $ runMaybeT do
        (_, bs) <- unboxSignedBox0 box & toMPlus

        AnnotatedHashRef _ href <- deserialiseOrFail @AnnotatedHashRef (LBS.fromStrict bs)
                                    & toMPlus . either (const Nothing) Just

        if HS.member href scanned then do
          atx <- readTVarIO accepts <&> fromMaybe mempty . HM.lookup txh
          lift $ withState $ transactional do
            insertScanned txh
            for_ atx insertScanned

        else do

          -- FIXME: decrypt-tree
          what <- runExceptT (readFromMerkle sto (SimpleKey (coerce href)))
                    <&> either (const Nothing) Just
                    >>= toMPlus

          exported <- deserialiseOrFail @[FixmeExported] what
                        & toMPlus

          for_ exported $ \exported -> do
            unless (HS.member href scanned) do
              atomically $ writeTQueue tq (txh, orig, href, exported)

  imported <- atomically $ flushTQueue tq

  withState $ transactional do
    for_ imported $ \(txh, h, href, i) -> do
      w <- readTVarIO ttsmap <&> fromMaybe (exportedWeight i) . HM.lookup h
      let item = i { exportedWeight = w }

      unless (exportedWeight item == 0) do
        notice $ "import" <+> pretty (exportedKey item) <+> pretty (exportedWeight item)
        insertFixmeExported (localNonce i) item

      atx <- readTVarIO accepts <&> fromMaybe mempty . HM.lookup h
      insertScanned txh
      insertScanned href
      for_ atx insertScanned

