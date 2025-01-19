{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git3.Repo ( initRepo, waitRepo
                      , getRepoRefMaybe
                      , getRepoManifest
                      , HasGitRemoteKey(..)
                      ) where

import HBS2.Git3.Prelude
import HBS2.Git3.State

import HBS2.CLI.Run.MetaData
import HBS2.Net.Auth.Credentials

import HBS2.Data.Detect ( readLogThrow )
import HBS2.CLI.Run.Internal.Merkle (getTreeContents)
import HBS2.CLI.Run.RefLog (getCredentialsForReflog,mkRefLogUpdateFrom)

import HBS2.Git3.Config.Local

import HBS2.KeyMan.Keys.Direct

import Data.Config.Suckless.Script
import Data.Config.Suckless.Almost.RPC

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Word
import Lens.Micro.Platform

import System.Random hiding (next)

{- HLINT ignore "Functor law"-}

data CInit =
    CheckRepoKeyExist
  | CreateRepoKey
  | CheckRepoKeyStart GitRepoKey
  | CheckRepoKeyWait TimeSpec (Timeout 'Seconds) GitRepoKey
  | CheckRepoDefBlock GitRepoKey (LWWRef 'HBS2Basic)
  | CreateRepoDefBlock GitRepoKey


data ReflogWaitTimeout =
  ReflogWaitTimeout
  deriving stock (Show,Typeable)

instance Exception ReflogWaitTimeout

waitRepo :: forall m . HBS2GitPerks m => Git3 m ()
waitRepo = do
  repoKey <- getGitRepoKey >>= orThrow GitRepoRefNotSet

  lwwAPI   <- getClientAPI @LWWRefAPI @UNIX
  peerAPI  <- getClientAPI @PeerAPI @UNIX
  reflogAPI <- getClientAPI @RefLogAPI @UNIX

  env <- ask

  callRpcWaitMay @RpcLWWRefFetch (TimeoutSec 1) lwwAPI (LWWRefKey repoKey)
    >>= orThrowUser "rpc timeout while subscribing to LWWRef"

  let maxTimeout = ceiling 30e9 -- Максимальное время ожидания (30 секунд)
  startTime <- getTimeCoarse

  flip runContT pure do

    let periodicFetch reflog = forever $ do
          callRpcWaitMay @RpcRefLogFetch (TimeoutSec 1) reflogAPI reflog
            >>= orThrowUser "rpc timeout while fetching reflog"
          pause @'Seconds 10 -- Засыпаем на 10 секунд

    let waitForReflog till reflog = do
          now <- getTimeCoarse

          if now > till
            then throwIO ReflogWaitTimeout
            else do
              mhead <- callRpcWaitMay @RpcRefLogGet (TimeoutSec 1) reflogAPI (coerce reflog)
              case mhead of
                Just headVal -> do
                  debug $ "waitRepo: Reflog data arrived" <+> pretty headVal

                Nothing -> pause  @'Seconds 1 >> waitForReflog till reflog

    let waitForLWWRef till = liftIO $ withGit3Env env do
          now <- getTimeCoarse

          if now > till
            then throwIO RpcTimeout
            else do
              rv <- getRepoRefMaybe
              maybe1 rv (pause @'Seconds 1 >> waitForLWWRef till) $ \LWWRef{..} -> do
                debug $ "waitRepo: LWWRef arrived" <+> pretty lwwValue

                -- Парсим манифест репозитория
                repo <- getRepoManifest

                -- Достаём `reflog`
                reflog <- [ x | ListVal [SymbolVal "reflog", SignPubKeyLike x] <- repo ]
                            & headMay
                            & orThrowUser "malformed repo manifest"

                -- Подписываемся на `reflog`
                callRpcWaitMay @RpcPollAdd (TimeoutSec 1) peerAPI (reflog, "reflog", 17)
                  >>= orThrowUser "rpc timeout while subscribing to reflog"

                debug $ "waitRepo: Subscribed to reflog" <+> pretty (AsBase58 reflog)

                -- Запускаем асинхронную задачу для периодического вызова RpcRefLogFetch
                withAsync (periodicFetch reflog) $ \_ -> do
                  -- Ждём появления значений в `reflog`
                  waitForReflog till reflog

    liftIO $ withGit3Env env $ waitForLWWRef (startTime + fromNanoSecs maxTimeout)

initRepo :: forall m . HBS2GitPerks m => [Syntax C] -> Git3 m ()
initRepo syn = do

  let (opts, _) = splitOpts [("--new",0)] syn

  let new = or [ True | ListVal [SymbolVal "--new"] <- opts ]

  root <- getConfigRootFile

  sto  <- getStorage
  lwwAPI    <- getClientAPI @LWWRefAPI @UNIX
  refLogAPI <- getClientAPI @RefLogAPI @UNIX
  peerAPI   <- getClientAPI @PeerAPI @UNIX

  flip fix CheckRepoKeyExist $ \next -> \case
    CheckRepoKeyExist -> do
      debug "initRepo:CheckRepoKey"
      mbk <- getGitRepoKey
      next $ maybe CreateRepoKey CheckRepoKeyStart mbk

    CreateRepoKey -> do
      debug "initRepo:CreateRepoKey"

      answ <- callProc "hbs2-cli" [] [mkSym "hbs2:lwwref:create"]

      pk <- [ x | ListVal [SymbolVal "pk", SignPubKeyLike x] <- answ ]
              & lastMay
              & orThrowUser "can't create new lwwref"

      liftIO $ appendFile root (show $ pretty $ mkForm "repo:key" [mkSym @C (show $ pretty (AsBase58 pk))])

    CheckRepoKeyStart pk -> do
      debug $ "initRepo:CheckRepoKeyStart" <+> pretty (AsBase58 pk)

      callRpcWaitMay @RpcLWWRefFetch (TimeoutSec 1) lwwAPI (LWWRefKey pk)
        >>= orThrowUser "rpc timeout"

      now <- getTimeCoarse

      let waity = if new then ceiling 0.5e9 else ceiling 30e9

      let till = TimeoutTS (now + fromNanoSecs waity )

      next $ CheckRepoKeyWait (coerce till) 1.0 pk

    CheckRepoKeyWait till w pk -> do

      debug $ "initRepo:CheckRepoKeyWait" <+> pretty (AsBase58 pk)

      rv <- getRepoRefMaybe

      now <- getTimeCoarse

      if now > till then do
        next $ CreateRepoDefBlock pk
      else do
        maybe1 rv (pause w >> next (CheckRepoKeyWait till (1.10 * w) pk))
                  (next . CheckRepoDefBlock pk)

    CheckRepoDefBlock pk LWWRef{..} -> do
      debug $ "init:CheckRepoDefBlock" <+> pretty (AsBase58 pk) <+> pretty lwwValue

      repo <- getRepoManifest

      reflog <- [ x | ListVal [SymbolVal "reflog", SignPubKeyLike x] <- repo ]
                  & headMay & orThrowUser "malformed repo manifest"

      callRpcWaitMay @RpcPollAdd (TimeoutSec 1) peerAPI (reflog, "reflog", 17)
         >>= orThrowUser "rpc timeout"

      liftIO $ print $ pretty repo

    CreateRepoDefBlock pk  -> do

      debug $ "init:CreateRepoDefBlock" <+> pretty (AsBase58 pk)

      seed <- randomIO @Word64

      creds <- runKeymanClientRO (loadCredentials pk)
                 >>= orThrowUser ("not found credentials for"  <+> pretty (AsBase58 pk))

      let (wsk,wpk) = (view peerSignSk creds, view peerSignPk creds)

      let sk = view peerSignSk creds
      (rpk,rsk) <- derivedKey @'HBS2Basic @'Sign seed sk

      let manifest = [
             mkForm @C "hbs2-git" [mkInt 3]
           , mkForm "seed" [mkInt seed]
           , mkForm "public" []
           , mkForm "reflog" [mkSym (show $ pretty (AsBase58 rpk))]
           ] & vcat . fmap pretty

      tree <- createTreeWithMetadata sto Nothing mempty (LBS8.pack (show $ manifest))
                >>= orThrowPassIO

      liftIO $ print tree

      let pt = toPTree (MaxSize defHashListChunk) (MaxNum defTreeChildNum) [tree]

      blk <- makeMerkle 0 pt $ \(_,_,bs) -> do
                void $ putBlock sto bs

      notice $ "current root" <+> pretty blk <+> pretty tree

      let box =  makeSignedBox wpk wsk (LWWRef 3 (coerce blk) Nothing)

      callRpcWaitMay @RpcLWWRefUpdate (TimeoutSec 1) lwwAPI box
         >>= orThrowUser "rpc timeout"

      callRpcWaitMay @RpcPollAdd (TimeoutSec 1) peerAPI (rpk, "reflog", 17)
         >>= orThrowUser "rpc timeout"

