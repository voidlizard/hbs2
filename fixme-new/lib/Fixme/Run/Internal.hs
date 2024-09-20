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
import Fixme.GK

import HBS2.Git.Local.CLI
import HBS2.CLI.Run.MetaData (createTreeWithMetadata,getTreeContents,getGroupKeyHash)
import HBS2.Merkle.MetaData

import HBS2.OrDie
import HBS2.Base58
import HBS2.Net.Auth.GroupKeySymm
import HBS2.Data.Types.SignedBox
import HBS2.Peer.Proto.RefChan as RefChan
import HBS2.Peer.RPC.Client.RefChan
import HBS2.Storage.Operations.ByteString
import HBS2.System.Dir
import HBS2.Net.Auth.Credentials
import DBPipe.SQLite hiding (field)

import HBS2.CLI.Run.KeyMan (keymanNewCredentials)
import HBS2.KeyMan.Keys.Direct

import Data.Config.Suckless.Script.File

import Data.List qualified as L
import Data.List.Split (chunksOf)
import Data.Aeson.Encode.Pretty as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString qualified as BS
import Data.Either
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Maybe
import Data.HashSet qualified as HS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Text.InterpolatedString.Perl6 (qc)
import Data.Coerce
import Data.Word
import Lens.Micro.Platform
import System.Process.Typed
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Control.Concurrent.STM (flushTQueue)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory (getModificationTime)

import System.IO as IO

import Streaming.Prelude qualified as S

pattern IsSimpleTemplate ::  forall {c} . [Syntax c] -> Syntax c
pattern IsSimpleTemplate xs <- ListVal (SymbolVal "simple" : xs)

{- HLINT ignore "Functor law" -}

notEmpty :: [a] -> Maybe [a]
notEmpty = \case
  [] -> Nothing
  x  -> Just x

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


templateExample :: String
templateExample = [qc|

; this is an optional template example
; for nicer fixme list
;(define-template short
;  (quot
;    (simple
;         (trim 10  $fixme-key) " "
;
;         (if (~ FIXME $fixme-tag)
;          (then (fgd red (align 6  $fixme-tag))  )
;          (else (if (~ TODO $fixme-tag)
;                  (then (fgd green (align 6  $fixme-tag)))
;                  (else (align 6  $fixme-tag)) ) )
;          )
;
;
;         (align 10  ("[" $workflow "]"))  " "
;         (align 8  $class)  " "
;         (align 12 $assigned)  " "
;         (align 20 (trim 20 $committer-name)) " "
;         (trim 50  ($fixme-title)) " "
;         (nl))
;    )
;)
; (set-template default short)

|]


init :: FixmePerks m => FixmeM m ()
init = do

  lo <- localConfigDir

  let lo0 = takeFileName lo

  mkdir lo
  touch (lo </> "config")

  let gitignore = lo </> ".gitignore"
  here <- doesPathExist gitignore

  confPath <- localConfig

  unless here do

    liftIO $ appendFile confPath $ show $ vcat
      [ mempty
      , ";; this is a default fixme config"
      , ";;"
      , "fixme-prefix"    <+> "FIXME:"
      , "fixme-prefix"    <+> "TODO:"
      , "fixme-value-set" <+> hsep [":workflow", ":new",":wip",":backlog",":done"]
      , "fixme-file-comments" <+> dquotes "*.scm" <+> dquotes ";"
      , "fixme-comments" <+> dquotes ";"  <+> dquotes "--" <+> dquotes "#"
      , mempty
      ]

    exts <- listBlobs Nothing
              <&> fmap (takeExtension . fst)
              <&> HS.toList . HS.fromList

    for_ exts $ \e -> do
      unless (e `elem` [".gitignore",".local"] )  do
        liftIO $ appendFile confPath $
          show $ ( "fixme-files" <+> dquotes ("**/*" <> pretty e) <> line )

    liftIO $ appendFile confPath $ show $ vcat
      [ "fixme-exclude" <+> dquotes "**/.**"
      ]

    liftIO $ appendFile confPath $ show $ vcat
      [ mempty
      , pretty templateExample
      , ";; uncomment to source any other local settings file"
      , ";; source ./my.local"
      , mempty
      ]

  unless here do
    liftIO $ writeFile gitignore $ show $
      vcat [ pretty ("." </> localDBName)
           ]

  notice $ green "default config created:" <+> ".fixme-new/config" <> line
             <> "edit it for your project" <> line
             <> "typically you need to add it to git"
             <> line
             <> "use (source ./some.local) form to add your personal settings" <> line
             <> "which should not be shared amongst the whole project" <> line
             <> "and add " <> yellow ".fixme-new/some.local" <+> "file to .gitignore"
             <> line

  notice $ "run" <> line <> vcat [
      mempty
    , "git add" <+> pretty (lo0  </> ".gitignore")
    , "git add" <+> pretty (lo0  </> "config")
    , mempty
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

  rchan <- asks fixmeEnvRefChan >>= readTVarIO

  liftIO $ print $ ("refchan" <+> pretty (AsBase58 <$> rchan))

  author <- asks fixmeEnvAuthor >>= readTVarIO

  liftIO $ print $ ("author" <+> pretty (AsBase58 <$> author))

  reader <- asks fixmeEnvReader >>= readTVarIO

  liftIO $ print $ ("reader" <+> pretty (AsBase58 <$> reader))

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

      debug $ red "start"  <+> pretty start
      debug $ red "before" <+> pretty before

      let bbefore = if start == 0 then before  else before + 1
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

class HasRefChanExportOpts a where
  refchanExportDry :: a -> Bool

data RefChanExportOpts =
  RefChanExportDry
  deriving (Eq,Ord,Show,Enum)

instance HasRefChanExportOpts [RefChanExportOpts] where
  refchanExportDry what = RefChanExportDry `elem` what

instance HasRefChanExportOpts () where
  refchanExportDry _ = False

refchanExport :: (FixmePerks m, HasRefChanExportOpts a) => a -> FixmeM m Int
refchanExport opts = do

  let dry = refchanExportDry opts

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

  gk0 <- loadGroupKey

  -- TODO: this-may-cause-to-tx-flood
  --   сделать какой-то период релакса,
  --   что ли
  now <- liftIO $ getPOSIXTime <&> round

  withState do
    what <- select  @FixmeExported [qc|
      select distinct o,?,k,cast (v as text)
      from object obj
      where not exists (select null from scanned where hash = obj.nonce)
      order by o, k, v, w
      |] (Only now)

    let chu  = chunksOf 10000 what

    flip runContT pure do

      for_ chu $ \x -> callCC \next -> do

        -- FIXME: encrypt-tree

        --   6. как делать доступ к историческим данным
        --   6.1 новые ключи в этот же рефчан
        --   6.2 или новые ключи в какой-то еще рефчан

        let s = maybe "[ ]" (const $ yellow "[@]") gk0

        let gk = snd <$> gk0

        href <- liftIO $ createTreeWithMetadata sto gk mempty (serialise x)
                  >>= orThrowPassIO

        let tx = AnnotatedHashRef Nothing href

        lift do

          let lbs = serialise tx

          let box = makeSignedBox @'HBS2Basic @BS.ByteString pk sk (LBS.toStrict lbs)

          warn $ "POST" <+> pretty (length x) <+> s <> "tree" <+> pretty href <+> pretty (hashObject @HbSync (serialise box))

          unless dry do
            r <- callRpcWaitMay @RpcRefChanPropose (TimeoutSec 1) rchanAPI (chan, box)

            when (isNothing r) do
              err $ red "hbs2-peer rpc calling timeout"

      pure $ length what


refchanUpdate :: FixmePerks m => FixmeM m ()
refchanUpdate = do

  refchanImport

  rchan <- asks fixmeEnvRefChan
             >>= readTVarIO
             >>= orThrowUser "refchan not set"

  api   <- getClientAPI @RefChanAPI @UNIX

  sto <- getStorage

  h0 <- callService @RpcRefChanGet api rchan
         >>= orThrowUser "can't request refchan head"

  rch <-  RefChan.getRefChanHead @L4Proto sto (RefChanHeadKey rchan)
              >>= orThrowUser "can't request refchan head"

  let w = view refChanHeadWaitAccept rch

  refchanExportGroupKeys

  txn <- refchanExport ()

  unless (txn == 0) do

    notice $ "wait refchan" <+> pretty (AsBase58 rchan) <+> "to update..."

    -- TODO: implement-refchan-update-notifications

    -- FIXME: use-wait-time-from-refchan-head

    -- TODO: fix-this-lame-polling
    flip fix 0 $ \next -> \case
      n | n >= w -> pure ()
      n  -> do

        h <- callService @RpcRefChanGet api rchan
               >>= orThrowUser "can't request refchan head"

        if h0 /= h then
          pure ()
        else do
          pause @'Seconds 1
          liftIO $ hPutStr stderr (show $ pretty (w - n) <> "    \r")
          next (succ n)

    none

  refchanImport

refchanImport :: FixmePerks m => FixmeM m ()
refchanImport = do

  sto <- getStorage

  chan <- asks fixmeEnvRefChan
           >>= readTVarIO
           >>= orThrowUser "refchan not set"


  ttsmap  <- newTVarIO HM.empty
  accepts <- newTVarIO HM.empty

  tq <- newTQueueIO

  ignCached <- asks fixmeEnvFlags >>= readTVarIO <&> HS.member FixmeIgnoreCached

  let goodToGo x | ignCached = pure True
                 | otherwise  = do
        here <- selectIsAlreadyScanned x
        pure $ not here

  fixmeGkSign <- putBlock sto "FIXMEGROUPKEYBLOCKv1" <&> fmap HashRef
                    >>= orThrowUser "hbs2 storage error. aborted"

  walkRefChanTx @UNIX goodToGo chan $ \txh u -> do

    case  u of

      A (AcceptTran (Just ts) _ what) -> do
        debug $ red "ACCEPT" <+> pretty ts <+> pretty what
        atomically $ modifyTVar ttsmap (HM.insertWith max what (coerce @_ @Word64 ts))
        atomically $ modifyTVar accepts (HM.insertWith (<>) what (HS.singleton txh))

        scanned <- selectIsAlreadyScanned what
        when scanned do
           withState $ insertScanned txh

      A _  -> none

      P orig (ProposeTran _ box) -> void $ runMaybeT do
        (_, bs) <- unboxSignedBox0 box & toMPlus

        AnnotatedHashRef sn href <- deserialiseOrFail @AnnotatedHashRef (LBS.fromStrict bs)
                                    & toMPlus . either (const Nothing) Just

        scanned <- lift $ selectIsAlreadyScanned href

        when (not scanned || ignCached) do

          let isGk = sn == Just fixmeGkSign

          if isGk then do

            atomically $ writeTQueue tq (Left (txh, orig, href, href))

          else do
            what <- liftIO (runExceptT $ getTreeContents sto href)
                      <&> either (const Nothing) Just
                      >>= toMPlus

            let exported = deserialiseOrFail @[FixmeExported] what
                              & either (const Nothing) Just

            case exported of
              Just e -> do
                for_ e $ \x -> do
                  atomically $ writeTQueue tq (Right (txh, orig, href, x))

              Nothing -> do
                lift $ withState $ insertScanned txh

  imported <- atomically $ flushTQueue tq

  withState $ transactional do
    for_ imported $ \case
      Left (txh, orig, href, gk) -> do
        -- hx <- writeAsMerkle sto (serialise gk)
        -- notice $ "import GK" <+> pretty hx <+> "from" <+> pretty href
        -- let tx = AnnotatedHashRef _ href <- deserialiseOrFail @AnnotatedHashRef (LBS.fromStrict bs)
        --                             & toMPlus . either (const Nothing) Just
        insertScanned txh
        -- TODO: ASAP-notify-hbs2-keyman
        --   у нас два варианта:
        --     1. звать runKeymanClient и в нём записывать в БД
        --        с возможностью блокировок
        --     2. каким-то образом делать отложенную запись,
        --        например, писать лог групповых ключей
        --        куда-то, откуда hbs2-keyman сможет
        --        обновить их при запуске
        --
        --        лог групповых ключей мы можем писать:
        --          1.  в рефлог, на который подписан и кейман
        --          2.  в рефчан, на который подписан и кейман
        --                неожиданные плюсы:
        --                  + у нас уже есть такой рефчан!
        --                    всё, что надо сделать  -- это записать ключи туда
        --                    с одной стороны туповато: перекладывать транзы из
        --                    рефчана в рефчан. с другой стороны -- не нужны никакие
        --                    новые механизмы. рефчан, в общем-то, локальный(?),
        --                    блоки никуда за пределы хоста не поедут (?) и сеть
        --                    грузить не будут (?)
        --
        --          3.  в рефчан, используя notify
        --          4.  в еще какую переменную, которая будет
        --              локальна
        --          5.  в какой-то лог. который кейман будет
        --              процессировать при hbs2-keyman update
        --
        --      поскольку БД кеймана блокируется целиком при апдейтах,
        --      единственное, куда писать проблематично -- это сама БД.
        --
        pure ()

      Right (txh, h, href, i) -> do
        w <- readTVarIO ttsmap <&> fromMaybe (exportedWeight i) . HM.lookup h
        let item = i { exportedWeight = w }

        if exportedWeight item /= 0 then do
          notice $ "import" <+> pretty (exportedKey item) <+> pretty (exportedWeight item)
          insertFixmeExported (localNonce (href,i)) item
        else do
          debug $ "SKIP TX!" <+> pretty txh

        atx <- readTVarIO accepts <&> fromMaybe mempty . HM.lookup h
        insertScanned txh
        insertScanned href
        for_ atx insertScanned


fixmeRefChanInit :: FixmePerks m => FixmeM m ()
fixmeRefChanInit = do
   let rch0 = refChanHeadDefault @L4Proto
   sto <- getStorage
   peer <- getClientAPI @PeerAPI @UNIX
   rchanApi <- getClientAPI @RefChanAPI @UNIX

   dir <- localConfigDir
   confFile <- localConfig

   rchan <- asks fixmeEnvRefChan
              >>= readTVarIO

   flip runContT pure $ callCC \done -> do

     when (isJust rchan) do
      warn $ red "refchan is already set" <+> pretty (fmap AsBase58 rchan)
      warn $ "done" <+> pretty (fmap AsBase58 rchan)
      done ()

     poked <- lift $ callRpcWaitMay @RpcPoke (TimeoutSec 1) peer ()
                >>= orThrowUser "hbs2-peer not connected"
                <&> parseTop
                <&> fromRight mempty

     pkey <- [ fromStringMay @(PubKey 'Sign 'HBS2Basic) x
             | ListVal [SymbolVal "peer-key:", StringLike x ] <- poked
             ] & headMay . catMaybes & orThrowUser "hbs2-peer key not set"


     notice $ green "default peer" <+> pretty (AsBase58 pkey)


     signK' <- lift $ runKeymanClientRO $ listCredentials
             <&> headMay

     signK <- ContT $ maybe1 signK' (throwIO $ userError "no default author key found in hbs2-keyman")

     notice $ green "default author" <+> pretty (AsBase58 signK)

     -- TODO: use-hbs2-git-api?
     (_, gkh', _) <- readProcess (shell [qc|git hbs2 key|])
                      <&> over _2 (  (fromStringMay @HashRef)  <=< (notEmpty . headDef "" . lines . LBS8.unpack) )
                      <&> \x ->  case view _1 x of
                            ExitFailure _ -> set _2 Nothing x
                            ExitSuccess   -> x

     notice $ green "group key" <+> maybe "none" pretty gkh'

     readers <- fromMaybe mempty <$> runMaybeT do
                  gh <- toMPlus gkh'
                  gk <- loadGroupKeyMaybe @'HBS2Basic sto gh
                           >>= toMPlus
                  pure $ HM.keys (recipients gk)

     notice $ green "readers" <+> pretty (length readers)

     rk <- lift $ runKeymanClientRO $ loadKeyRingEntries readers
              <&> fmap snd . headMay


     let rch1 = rch0 & set refChanHeadReaders (HS.fromList readers)
                     & set refChanHeadAuthors (HS.singleton signK)
                     & set refChanHeadPeers   (HM.singleton pkey 1)


     let unlucky =    HM.null (view refChanHeadPeers rch1)
                   || HS.null (view refChanHeadAuthors rch1)


     liftIO $ print $ pretty rch1

     if unlucky then do
        warn $ red $ "refchan definition is not complete;" <+>
                     "you may add missed keys, edit the"   <+>
                     "defition and add if manually or repeat init attempt"
                     <> line
     else do
        notice "refchan definition seems okay, adding new refchan"

        refchan <- lift $ keymanNewCredentials (Just "refchan") 0

        creds <- lift $ runKeymanClientRO $ loadCredentials refchan
                       >>= orThrowUser "can't load credentials"

        let box = makeSignedBox @'HBS2Basic (view peerSignPk creds) (view peerSignSk creds) rch1

        href <- writeAsMerkle sto  (serialise box)

        callService @RpcPollAdd peer (refchan, "refchan", 17)
            >>= orThrowUser "can't subscribe to refchan"

        callService @RpcRefChanHeadPost rchanApi (HashRef href)
            >>= orThrowUser "can't post refchan head"


        let nonce = take 6 $ show $ pretty (AsBase58 refchan)
        let rchanFile = "refchan-" <> nonce <> ".local"
        let rchanFilePath = dir </> rchanFile

        let note =    ";; author and reader are inferred automatically" <> line
                   <> ";; from hbs2-keyman data" <> line
                   <> ";; edit them if needed" <> line
                   <> ";; reader is *your* reading public key." <> line
                   <> ";; author is *your* signing public key." <> line

        let refChanClause = mkList @C [ mkSym "refchan"
                                      , mkSym (show $ pretty (AsBase58 refchan))
                                      ]

        let theirReaderKeyClause = maybe1 rk ";; reader ..."$ \(KeyringEntry pk _ _) -> do
               pretty $ mkList @C [ mkSym "reader", mkSym (show $ pretty (AsBase58 pk) )  ]

        let theirAuthorClause  = mkList @C [ mkSym "author", mkSym (show $ pretty (AsBase58 signK) ) ]

        let content =    line
                      <> note
                      <> line
                      <> vcat [ theirReaderKeyClause
                              , pretty theirAuthorClause
                              ]

        liftIO do
          writeFile rchanFilePath $
            show content

          appendFile confFile $ show $
              line
              <> vcat [ pretty refChanClause ]
              <> line <>
              pretty (mkList @C [ mkSym "source", mkSym ( "." </> rchanFile ) ])

        notice $ green "refchan added" <+> pretty (AsBase58 refchan)


refchanExportGroupKeys :: FixmePerks m => FixmeM m ()
refchanExportGroupKeys = do

  let gkHash x = hashObject @HbSync ("GKSCAN" <> serialise x) & HashRef

  sto <- getStorage

  chan <- asks fixmeEnvRefChan
           >>= readTVarIO
           >>= orThrowUser "refchan not set"

  ignCached <- asks fixmeEnvFlags >>= readTVarIO <&> HS.member FixmeIgnoreCached

  let goodToGo x | ignCached = pure True
                 | otherwise = do
        here <- selectIsAlreadyScanned (gkHash x)
        pure $ not here

  debug "refchanExportGroupKeys"

  skip <- newTVarIO HS.empty
  gkz <- newTVarIO HS.empty

  fixmeSign <- putBlock sto "FIXMEGROUPKEYBLOCKv1" <&> fmap HashRef

  walkRefChanTx @UNIX goodToGo chan $ \txh u -> do

    case u of
      P orig (ProposeTran _ box) -> void $ runMaybeT do
        (_, bs) <- unboxSignedBox0 box & toMPlus

        AnnotatedHashRef _ href <- deserialiseOrFail @AnnotatedHashRef (LBS.fromStrict bs)
                                    & toMPlus . either (const Nothing) Just

        result <- lift $ try @_ @OperationError  (getGroupKeyHash href)

        case result of
          Right (Just gk,_) -> do
            atomically do
              modifyTVar gkz  (HS.insert gk)
              modifyTVar skip (HS.insert txh)

          Right (Nothing,_) -> do
            atomically $ modifyTVar skip (HS.insert txh)

          Left UnsupportedFormat -> do
            debug $ "unsupported" <+> pretty href
            atomically $ modifyTVar skip (HS.insert txh)

          Left e -> do
             debug $ "other error" <+> viaShow e

      _ -> none

  l <- readTVarIO skip <&> HS.toList
  r <- readTVarIO gkz <&> HS.toList

  withState $ transactional do
    for_ l (insertScanned . gkHash)

  rchan <- asks fixmeEnvRefChan
             >>= readTVarIO
             >>= orThrowUser "refchan not set"

  api   <- getClientAPI @RefChanAPI @UNIX

  rch <-  RefChan.getRefChanHead @L4Proto sto (RefChanHeadKey rchan)
              >>= orThrowUser "can't request refchan head"

  au <- asks fixmeEnvAuthor
           >>= readTVarIO
           >>= orThrowUser "author's key not set"

  creds <- runKeymanClientRO $ loadCredentials au
             >>= orThrowUser "can't read credentials"


  let (pk,sk) = (view peerSignPk creds, view peerSignSk creds)

  keyz <- Set.fromList <$> S.toList_ do
    for_ r $ \gkh -> void $ runMaybeT do

      debug $ red $ "FOR GK" <+> pretty gkh

      gk <- loadGroupKeyMaybe @'HBS2Basic sto gkh >>= toMPlus

      -- the original groupkey should be indexed as well
      lift $ S.yield gkh

      gks <- liftIO (runKeymanClientRO $ findMatchedGroupKeySecret sto gk)

      when (isNothing gks) do
      --   lift $ withState (insertScanned (gkHash txh))
        warn $ "unaccessible group key" <+> pretty gkh
        mzero

      gk1 <- generateGroupKey @'HBS2Basic gks (HS.toList $ view refChanHeadReaders rch)
      let lbs = serialise gk1
      gkh1 <- writeAsMerkle sto lbs  <&> HashRef
      debug $  red "prepare new gk0" <+> pretty (LBS.length lbs) <+> pretty gkh <+> pretty (groupKeyId gk)
      lift $ S.yield gkh1

  notice $ yellow $ "new gk:" <+> pretty (Set.size keyz)

  -- let nitems = 262144 `div` (125 * HS.size (view refChanHeadReaders rch) )
  -- let chunks = Map.elems keyz & chunksOf nitems

  -- TODO: gk:performance-vs-reliability
  --   ситуация такова: групповой ключ это меркл-дерево
  --   для одного и того же блоба могут быть разные меркл-деревья,
  --   так как могут быть разные настройки.
  --
  --   если распространять ключи по-одному, то хотя бы тот же ключ,
  --   который мы создали изначально -- будет доступен по своему хэшу,
  --   как отдельный артефакт.
  --
  --   Если писать их пачками, где каждый ключ представлен непосредственно,
  --   то на принимающей стороне нет гарантии, что меркл дерево будет писаться
  --   с таким же параметрами, хотя и может.
  --
  --   Решение:  делать групповой ключ БЛОКОМ. тогда его размер будет ограничен,
  --   но он хотя бы будет всегда однозначно определён хэшем.
  --
  --   Решение: ссылаться не на групповой ключ, а на хэш его секрета
  --   что ломает текущую схему и обратная совместимость будет морокой.
  --
  --   Решение: добавить в hbs2-keyman возможно индексации единичного
  --   ключа, и индексировать таким образом *исходные* ключи.
  --
  --   Тогда можно эти вот ключи писать пачками, их хэши не имеют особого значения,
  --   если мы проиндексируем оригинальный ключ и будем знать, на какой секрет он
  --   ссылается.
  --
  --   Заметим, что в один блок поместится аж >2000 читателей, что должно быть
  --   более, чем достаточно => при таких группах вероятность утечки секрета
  --   стремится к 1.0, так как большинство клало болт на меры безопасности.
  --
  --   Кстати говоря, проблема недостаточного количества авторов в ключе легко
  --   решается полем ORIGIN, т.к мы можем эти самые ключи разделять.
  --
  --   Что бы не стоять перед такой проблемой, мы всегда можем распостранять эти ключи
  --   по-одному, ЛИБО добавить в производный ключ поле
  --   ORIGIN: где будет хэш изначального ключа.
  --
  --   Это нормально, так как мы сможем проверить, что у этих ключей
  --   (текущий и ORIGIN) одинаковые хэши секретов.
  --
  --   Это всё равно оставляет возможность еще одной DoS атаки на сервис,
  --   с распространением кривых ключей, но это хотя бы выяснимо, ну и атака
  --   может быть только в рамках рефчана, т.е лечится выкидыванием пиров /
  --   исключением зловредных авторов.

  for_ (Set.toList keyz) $ \href -> do

      let tx = AnnotatedHashRef fixmeSign href

      let lbs = serialise tx

      let box = makeSignedBox @'HBS2Basic @BS.ByteString pk sk (LBS.toStrict lbs)

      warn $ "post gk tx" <+> "tree" <+> pretty href

      result <- callRpcWaitMay @RpcRefChanPropose (TimeoutSec 1) api (chan, box)

      when (isNothing result) do
        err $ red "hbs2-peer rpc calling timeout"


