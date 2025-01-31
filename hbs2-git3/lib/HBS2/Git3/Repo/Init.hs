{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git3.Repo.Init (initRepo,newRepoOpt,encryptedNewOpt) where

import HBS2.Git3.Prelude
import HBS2.Git3.State
import HBS2.Git3.Repo.Types
import HBS2.Git3.Repo.Tools
import HBS2.Git3.Export

import HBS2.System.Dir

import HBS2.CLI.Run.MetaData
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.GroupKeySymm

import HBS2.KeyMan.Keys.Direct

import Data.Config.Suckless.Script
import Data.Config.Suckless.Almost.RPC

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Word
import Data.Maybe
import Data.Text qualified as Text
import Data.HashMap.Strict qualified as HM
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

newRepoOpt :: Syntax C
newRepoOpt = mkSym "--new"

encryptedNewOpt :: Syntax C
encryptedNewOpt = mkSym "--encrypted"



initRepo :: forall m . HBS2GitPerks m => [Syntax C] -> Git3 m ()
initRepo syn = do

  let (opts, _) = splitOpts [("--new",0),("--encrypted",1)] syn

  let new = or [ True | ListVal [SymbolVal "--new"] <- opts ]
  let gkh = lastMay [ gk | ListVal [SymbolVal "--encrypted", HashLike gk] <- opts ]

  callProc "git" ["init"] []

  root <- getConfigRootFile

  touch root

  sto  <- getStorage
  lwwAPI    <- getClientAPI @LWWRefAPI @UNIX
  refLogAPI <- getClientAPI @RefLogAPI @UNIX
  peerAPI   <- getClientAPI @PeerAPI @UNIX

  debug $ "initRepo" <+> pretty opts <+> pretty syn

  flip fix CheckRepoKeyExist $ \next -> \case
    CheckRepoKeyExist -> do
      debug "initRepo:CheckRepoKey"
      mbk <- getGitRepoKey
      next $ maybe CreateRepoKey CheckRepoKeyStart mbk

    CreateRepoKey -> do

      debug $ "initRepo:CreateRepoKey" <+> pretty root

      answ <- callProc "hbs2-cli" [] [mkSym "hbs2:lwwref:create"]

      pk <- [ x | ListVal [SymbolVal "pk", SignPubKeyLike x] <- answ ]
              & lastMay
              & orThrowUser "can't create new lwwref"

      -- liftIO $ appendFile root (show $ pretty $ mkForm "repo:ref" [mkSym @C (show $ pretty (AsBase58 pk))])

      setGitRepoKey pk

      next $ CheckRepoKeyStart pk

    CheckRepoKeyStart pk -> do

      debug $ "initRepo:CheckRepoKeyStart" <+> pretty new <+> pretty opts <+> pretty (AsBase58 pk)

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

      reflog <- getRefLog repo & orThrow GitRepoManifestMalformed

      callRpcWaitMay @RpcPollAdd (TimeoutSec 1) peerAPI (reflog, "reflog", 17)
         >>= orThrowUser "rpc timeout"

      -- FIXME: remove-this
      liftIO $ print $ pretty $ mkForm "manifest" (coerce repo)


    CreateRepoDefBlock pk  -> do

      debug $ "init:CreateRepoDefBlock" <+> pretty (AsBase58 pk)

      seed <- randomIO @Word64

      creds <- runKeymanClientRO (loadCredentials pk)
                 >>= orThrowUser ("not found credentials for"  <+> pretty (AsBase58 pk))

      let sk = view peerSignSk creds
      (rpk,rsk) <- derivedKey @'HBS2Basic @'Sign seed sk

      callRpcWaitMay @RpcPollAdd (TimeoutSec 1) peerAPI (rpk, "reflog", 17)
         >>= orThrowUser "rpc timeout"

      gkRefs <- case gkh of
               Nothing -> pure []
               Just h  -> do
                 _ <- loadGroupKeyMaybe @'HBS2Basic sto h >>= orThrow (GroupKeyNotFound 1)
                 pure [h]

      let manifest = [
             mkForm @C "hbs2-git" [mkInt 3]
           , mkForm "seed" [mkInt seed]
           , mkForm "public" []
           , mkForm "reflog" [mkSym (show $ pretty (AsBase58 rpk))]
           ] <> map (\h -> mkForm "gk" [mkSym (show $ pretty (AsBase58 h))]) gkRefs

      updateRepoHead pk manifest gkRefs

      remoteName <- newRemoteName pk <&> show . pretty
      let remoteVal  = Text.unpack $ remoteRepoURL pk

      r <- callProc "git" ["remote", "add", remoteName, remoteVal] mempty
      liftIO $ print $ pretty "added git remote" <+> pretty remoteName <+> pretty remoteVal

      updateRepoKey pk

      when new postNullTx

