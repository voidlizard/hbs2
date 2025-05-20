{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language MultiWayIf #-}
{-# Language RecordWildCards #-}
{-# Language ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Clock
import HBS2.Merkle

import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Storage.Operations.ByteString
import HBS2.Net.Auth.Credentials
import HBS2.Peer.Proto.RefLog
import HBS2.Peer.Proto.LWWRef
import HBS2.Data.Types.SignedBox

import HBS2.System.Logger.Simple.ANSI

import HBS2.Storage.NCQ
import HBS2.Data.Log.Structured.NCQ

import HBS2.CLI.Run.Internal.Merkle

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Script as SC
import Data.Config.Suckless.System
import Data.Config.Suckless.Script.File as SF

import DBPipe.SQLite hiding (field)

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as TE
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Builder
import Data.Maybe
import Data.Word
import Data.List qualified as List
import Data.Vector qualified as V
import Data.Vector ((!))
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.Except (runExceptT)
import Network.ByteOrder qualified as N
import Data.Coerce
import Data.HashPSQ qualified as HPSQ
import Data.HashSet qualified as HS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.Fixed
import System.Environment
import System.Directory
import System.Posix.Fcntl
import System.Posix.IO
import System.IO.MMap
import System.IO qualified as IO
import System.Exit (exitSuccess, exitFailure)
import System.Random
import Safe
import Lens.Micro.Platform
import Control.Concurrent.STM qualified as STM

import UnliftIO

import Text.InterpolatedString.Perl6 (qc)

import Streaming.Prelude qualified as S
import System.TimeIt

import System.IO.Unsafe (unsafePerformIO)

{- HLINT ignore "Functor law" -}

setupLogger :: MonadIO m => m ()
setupLogger = do
  -- setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStdout . logPrefix ""

flushLoggers :: MonadIO m => m ()
flushLoggers = do
  silence

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE
  setLoggingOff @TRACE


data TCQError =
     TCQAlreadyOpen FilePath
   | TCQGone FilePath
  deriving stock (Show,Typeable)

instance Exception TCQError

newtype TCQ =
  TCQ FilePath
  deriving newtype (Eq,Ord,Show,Typeable)


main :: IO ()
main = do

  instances <- newTVarIO (mempty :: HashMap FilePath (NCQStorage, Async ()))

  tvd <- newTVarIO mempty

  let runScript dict argz what = liftIO do
        script <- either (error.show) pure $ parseTop what
        runM dict do
          bindCliArgs argz
          void $ evalTop script

  let finalizeStorages = do
        debug "finalize ncq"
        r <- readTVarIO instances <&> HM.toList
        mapM_ ncqStorageStop (fmap (fst.snd) r)
        mapM_ wait (fmap (snd.snd) r)

  let getNCQ (TCQ p) = do
        readTVarIO instances
          <&> HM.lookup p
          <&> fmap fst
          >>= orThrow  (TCQGone p)

  let getTCQ (TCQ p) = do
        readTVarIO instances
          <&> HM.lookup p
          >>= orThrow  (TCQGone p)

  let dict = makeDict @C do

        entry $ bindMatch "--help" $ nil_ \case
          HelpEntryBound what -> helpEntry what
          [StringLike s]      -> helpList False (Just s)
          _                   -> helpList False Nothing

        internalEntries
        SF.entries

        entry $ bindMatch "#!" $ nil_ $ const none

        entry $ bindMatch "stdin" $ nil_ $ \case
          argz  -> do
            liftIO getContents >>= runScript dict argz

        entry $ bindMatch "file" $ nil_ $ \case
          ( StringLike fn : argz ) -> do
            liftIO (readFile fn) >>= runScript dict argz

          e -> error (show $ pretty $ mkList e)


        entry $ bindMatch "debug" $ nil_ \case

          [ LitBoolVal False ] -> do
             setLoggingOff @DEBUG

          [ StringLike "off" ] -> do
             setLoggingOff @DEBUG

          _ ->
             setLogging @DEBUG  $ toStderr . logPrefix "[debug] "

        entry $ bindMatch "ncq:open" $ \case
          [ StringLike path ] -> do
             debug $ "ncq:open" <+> pretty path
             ncq <- ncqStorageOpen path
             r   <- async (ncqStorageRun ncq)

             e <- atomically do
                    already <- readTVar instances <&> HM.member path
                    if already then
                      pure $ Left $ TCQAlreadyOpen path
                    else do
                      modifyTVar instances (HM.insert path (ncq,r))
                      pure $ Right ()

             either throwIO pure e

             mkOpaque (TCQ path)

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:poke" $ \case
          [ isOpaqueOf @TCQ -> Just tcq ] -> lift do
            ncq <- getNCQ tcq
            pure $ mkSym "okay"

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:fossilize" $ nil_ \case
          [ isOpaqueOf @TCQ -> Just tcq ] -> lift do
            ncq <- getNCQ tcq
            ncqIndexRightNow ncq

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:merge:step" $ \syn -> lift do

          tcq <- case syn of
            [ isOpaqueOf @TCQ -> Just tcq ] -> do
              pure tcq

            e -> throwIO $ BadFormException @C (mkList e)

          ncq <- getNCQ tcq
          ncqStorageMergeStep ncq

          pure nil

        entry $ bindMatch "ncq:merge" $ \syn -> lift do

          tcq <- case syn of
            [ isOpaqueOf @TCQ -> Just tcq ] -> do
              pure tcq

            e -> throwIO $ BadFormException @C (mkList e)

          ncq <- getNCQ tcq
          ncqStorageMerge ncq

          pure nil

        entry $ bindMatch "ncq:close" $ nil_ \case
          [ isOpaqueOf @TCQ -> Just tcq ] -> lift do
            ncq <- getNCQ tcq
            ncqStorageStop ncq

            void $ runMaybeT do
              (s,r) <- readTVarIO instances
                      <&> HM.lookup (coerce tcq)
                      >>= toMPlus

              wait r
              atomically $ modifyTVar instances (HM.delete (coerce tcq))

          e -> throwIO $ BadFormException @C (mkList e)


        entry $ bindMatch "ncq:cached:entries" $ \case
          [ isOpaqueOf @TCQ -> Just tcq ] -> lift do
            NCQStorage{..} <- getNCQ tcq
            readTVarIO ncqCachedEntries <&> mkInt

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:locate" $ \case
          [ isOpaqueOf @TCQ -> Just tcq, HashLike hash ] -> lift do
            ncq <- getNCQ tcq
            ncqLocate ncq hash >>= \case
              Just x -> do
                parseSyntax (show $ pretty x) & either (error.show) pure

              _      -> pure nil

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:has" $ \case
          [ isOpaqueOf @TCQ -> Just tcq, HashLike hash ] -> lift do
            ncq <- getNCQ tcq
            ncqStorageHasBlock ncq hash <&> maybe nil mkInt

          e -> throwIO $ BadFormException @C (mkList e)


        entry $ bindMatch "ncq:del" $ nil_ \case
          [ isOpaqueOf @TCQ -> Just tcq, HashLike hash ] -> lift do
            ncq <- getNCQ tcq
            ncqStorageDel ncq hash

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:flush" $ nil_ \case
          [ isOpaqueOf @TCQ -> Just tcq  ] -> lift do
            ncq <- getNCQ tcq
            ncqStorageFlush ncq

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:stop" $ nil_ \case
          [ isOpaqueOf @TCQ -> Just tcq  ] -> lift do
            (ncq, w) <- getTCQ tcq
            ncqStorageStop ncq
            debug "wait storage to stop"
            wait w

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:set:ref" $ \case
          [ isOpaqueOf @TCQ -> Just tcq, HashLike ref , HashLike val ] -> lift do
              ncq <- getNCQ tcq
              ncqStorageSetRef ncq ref val
              pure nil

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:del:ref" $ \case
          [ isOpaqueOf @TCQ -> Just tcq , HashLike ref ] -> lift do
              ncq <- getNCQ tcq
              ncqStorageDelRef ncq ref
              pure nil

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:get:ref" $ \case
            [ isOpaqueOf @TCQ -> Just tcq, HashLike w ] -> lift do
              ncq <- getNCQ tcq
              ref <- ncqStorageGetRef ncq w
              debug $ "ref" <+> pretty w <+> pretty ref
              pure $ maybe nil (mkSym . show . pretty) ref

            e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:get:reflog" $ \case
            [ isOpaqueOf @TCQ -> Just tcq,  SignPubKeyLike reflog ] -> lift do
              ncq <- getNCQ tcq
              let sto = AnyStorage ncq
              let ha = hashObject @HbSync (RefLogKey @HBS2Basic reflog)
              debug $ "refhash" <+> pretty ha
              ref <- getRef sto (RefLogKey @HBS2Basic reflog)
              pure $ maybe nil (mkSym . show . pretty) ref

            e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:get:lwwref" $ \case
            [ isOpaqueOf @TCQ -> Just tcq,  SignPubKeyLike lww ] -> lift do
              ncq <- getNCQ tcq
              let sto = AnyStorage ncq
              val <- runMaybeT do
                rv <- getRef sto (LWWRefKey @HBS2Basic lww) >>= toMPlus
                getBlock sto rv >>= toMPlus
                      <&> unboxSignedBox @(LWWRef 'HBS2Basic) @HBS2Basic
                      >>= toMPlus
                      <&> snd

              pure $ maybe nil (mkSym . show . pretty) val

            e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:refhash" $ \case
            [ isOpaqueOf @TCQ -> Just tcq, HashLike w ] -> lift do
              ncq <- getNCQ tcq
              let rf = ncqRefHash ncq w
              pure $ mkSym ( show $ pretty $ rf )

            e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:hash" $ \case
            [ isOpaqueOf @ByteString -> Just bs ] -> lift do
              pure $ mkSym ( show $ pretty $ hashObject @HbSync bs )

            [ StringLike s ] -> lift do
              pure $ mkSym ( show $ pretty $ hashObject @HbSync (BS8.pack s) )

            e -> pure nil

        entry $ bindMatch "ncq:get" $ \case
          [ isOpaqueOf @TCQ -> Just tcq, HashLike hash ] -> lift do
            ncq <- getNCQ tcq
            ncqStorageGetBlock ncq hash >>= maybe (pure nil) mkOpaque

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:put" $ \syn -> do
          (tcq,bs) <- case syn of
            [ isOpaqueOf @TCQ -> Just tcq, isOpaqueOf @ByteString -> Just bs ] -> lift do
              pure (tcq, LBS.fromStrict bs)

            [ isOpaqueOf @TCQ -> Just tcq, TextLike s ] -> lift do
              pure (tcq, LBS.fromStrict (TE.encodeUtf8 s))

            e -> throwIO $ BadFormException @C (mkList e)

          lift do
            ncq <- getNCQ tcq
            r <- ncqStoragePutBlock ncq bs
            pure $ maybe nil (mkSym . show . pretty) r

        entry $ bindMatch "ncq:merkle:hashes" $ \case
            [ isOpaqueOf @TCQ -> Just tcq, HashLike h ] -> lift do
                ncq <- getNCQ tcq
                liftIO do
                  let sto = AnyStorage ncq
                  mkList <$> S.toList_ do
                    walkMerkle (coerce h) (getBlock sto) $ \case
                      Left{} -> throwIO MissedBlockError
                      Right (hrr :: [HashRef]) -> do
                          forM_ hrr $ \hx -> do
                            S.yield (mkSym $ show $ pretty hx)

            e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "sqlite:merkle:write" $ nil_ \case
            [ StringLike dbf, StringLike fname ] -> lift do
              db <- newDBPipeEnv dbPipeOptsDef dbf


              withDB db do
                ddl "create table if not exists block (hash blob not null primary key, value blob)"
                commitAll

              withDB db do
                ddl [qc|
pragma journal_mode=WAL;
pragma synchronous=normal;
                |]

              flip runContT pure do
                pipe <- ContT $ withAsync (runPipe db)

                lbs <- liftIO $ LBS.readFile fname

                chu <- S.toList_ (readChunkedBS lbs (256*1024))

                let sql = [qc|insert into block (hash, value) values(?,?) on conflict (hash) do nothing |]

                withDB db do
                  hashes <- for chu $ \chunk -> do
                      let ha = hashObject @HbSync chunk
                      insert sql (coerce @_ @ByteString ha, chunk)
                      pure ha

                  let pt = toPTree (MaxSize 1024) (MaxNum 256) hashes

                  m <- makeMerkle 0 pt $ \(ha,_,bss) -> do
                         insert sql (coerce @_ @ByteString ha, bss)

                  withDB db do
                    commitAll

                  pure $ mkSym @C (show $ pretty m)

            e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "ncq:merkle:write" $ \syn -> do
          (tcq,fname) <- case syn of
            [ isOpaqueOf @TCQ -> Just tcq, StringLike f ] -> lift do
              pure (tcq, f)

            e -> throwIO $ BadFormException @C (mkList e)

          lift do
            ncq <- getNCQ tcq

            lbs <- liftIO $ LBS.readFile fname

            chu <- S.toList_ (readChunkedBS lbs (256*1024))
            hashes <- forConcurrently chu $ \chunk -> do
              ncqStoragePutBlock ncq chunk >>= orThrowUser "can't save"

            -- FIXME: handle-hardcode
            let pt = toPTree (MaxSize 1024) (MaxNum 256) hashes -- FIXME: settings

            m <- makeMerkle 0 pt $ \(_,_,bss) -> liftIO do
                   void $ ncqStoragePutBlock ncq bss >>= orThrowUser "can't save"

            pure $ mkSym (show $ pretty m)

        entry $ bindMatch "ncq:merkle:read:stdout" $ nil_ \syn -> do
          (tcq,h) <- case syn of
            [ isOpaqueOf @TCQ -> Just tcq, HashLike f ] -> lift do
              pure (tcq, f)

            e -> throwIO $ BadFormException @C (mkList e)

          lift do
            ncq <- getNCQ tcq

            lbs <- runExceptT (getTreeContents (AnyStorage ncq) h)
                    >>= orThrowPassIO

            LBS.putStr lbs

        entry $ bindMatch "ncq:nway:stats" $ \case
          [StringLike fn] -> liftIO do

            mt_    <- newTVarIO 0
            total_ <- newTVarIO 0

            (mmaped,meta@NWayHash{..}) <- nwayHashMMapReadOnly fn >>= orThrow (NWayHashInvalidMetaData fn)

            let emptyKey = BS.replicate nwayKeySize 0
            nwayHashScanAll meta mmaped $ \o k v -> do
              atomically do
                modifyTVar total_ succ
                when (k == emptyKey) do
                  modifyTVar mt_ succ

            mt    <- readTVarIO mt_
            total <- readTVarIO total_
            let used = total - mt

            let ratio = realToFrac @_ @(Fixed E3) (realToFrac used / realToFrac total)

            let stats = mkForm @C "stats" [ mkForm "empty" [mkInt mt]
                                          , mkForm "used"  [mkInt used]
                                          , mkForm "total" [mkInt total]
                                          , mkForm "ratio" [mkDouble ratio]
                                          ]

            pure $ mkList [mkForm "metadata" [mkSyntax meta], stats]

          e -> throwIO $ BadFormException @C (mkList e)


  setupLogger

  argz <- liftIO getArgs

  forms <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  atomically $ writeTVar tvd dict

  flip runContT pure do

    ContT $ bracket none $ const do
      finalizeStorages
      flushLoggers

    lift do
      case forms of

        ( cmd@(ListVal [StringLike "file", StringLike fn]) : _ ) -> do
          void $ run dict [cmd]

        ( cmd@(ListVal [StringLike "stdin"]) : _ ) -> do
          void $ run dict [cmd]

        ( cmd@(ListVal [StringLike "--help"]) : _ ) -> do
          void $ run dict [cmd]

        [] -> do
          eof <- liftIO IO.isEOF
          if eof then
            void $ run dict [mkForm  "help" []]
          else do
            what <- liftIO getContents
                      >>= either (error.show) pure . parseTop

            run dict what >>= eatNil display

        e -> void $ run dict e

  -- (runEval tvd forms >>= eatNil display)
  --   `finally` (finalizeStorages >> flushLoggers)



