{-# Language MultiWayIf #-}
module Fixme.Run.Internal.RefChan (fixmeRefChanInit) where

import Prelude hiding (init)
import Fixme.Prelude hiding (indent)
import Fixme.Types
import Fixme.Config

import HBS2.OrDie
import HBS2.Base58
import HBS2.Net.Auth.GroupKeySymm
import HBS2.Data.Types.SignedBox
import HBS2.Peer.Proto.RefChan as RefChan
import HBS2.Storage.Operations.ByteString
import HBS2.System.Dir
import HBS2.Net.Auth.Credentials

import HBS2.CLI.Run.KeyMan (keymanNewCredentials)
import HBS2.KeyMan.Keys.Direct


import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Either
import Data.Maybe
import Data.List qualified as List
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Text.InterpolatedString.Perl6 (qc)
import Lens.Micro.Platform
import System.Process.Typed
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Word
import System.IO qualified as IO

{- HLINT ignore "Functor law"-}

notEmpty :: [a] -> Maybe [a]
notEmpty = \case
  [] -> Nothing
  x  -> Just x


data RefChanInitFSM =
    InitInit
  | SetupNewRefChan
  | SetupExitFailure
  | CheckRefChan (PubKey 'Sign 'HBS2Basic)
  | RefChanHeadFound (PubKey 'Sign 'HBS2Basic) (RefChanHeadBlock L4Proto)
  | WaitRefChanHeadStart (PubKey 'Sign 'HBS2Basic) Word64
  | WaitRefChanHead (PubKey 'Sign 'HBS2Basic) Word64

fixmeRefChanInit :: FixmePerks m => Maybe (PubKey 'Sign 'HBS2Basic) -> FixmeM m ()
fixmeRefChanInit mbRc = do
   let rch0 = refChanHeadDefault @L4Proto
   sto <- getStorage
   peer <- getClientAPI @PeerAPI @UNIX
   rchanApi <- getClientAPI @RefChanAPI @UNIX

   dir <- localConfigDir
   confFile <- localConfig

   rchan <- asks fixmeEnvRefChan
              >>= readTVarIO

   poked <- callRpcWaitMay @RpcPoke (TimeoutSec 1) peer ()
              >>= orThrowUser "hbs2-peer not connected"
              <&> parseTop
              <&> fromRight mempty

   pkey <- [ fromStringMay @(PubKey 'Sign 'HBS2Basic) x
           | ListVal [SymbolVal "peer-key:", StringLike x ] <- poked
           ] & headMay . catMaybes & orThrowUser "hbs2-peer key not set"


   let refChanClause r = mkList @C [ mkSym "refchan"
                                   , mkSym (show $ pretty (AsBase58 r))
                                   ]

   flip runContT pure $ callCC \done -> do

    flip fix InitInit $ \next -> \case
      InitInit -> do

        case (rchan, mbRc) of
          (Nothing, Nothing) -> next SetupNewRefChan
          (_, Just r2)  -> next (CheckRefChan r2)
          (Just r1, Nothing) -> next (CheckRefChan r1)

      CheckRefChan rc -> do
        notice $ "check refchan:" <+> pretty (AsBase58 rc)

        notice $ "subscribe to refchan" <+> pretty (AsBase58 rc)

        -- FIXME: poll-time-hardcode
        --   $class: hardcode
        void $ callService @RpcPollAdd peer (rc, "refchan", 17)

        notice $ "fetch refchan head" <+> pretty (AsBase58 rc)
        void $ lift $ callRpcWaitMay  @RpcRefChanHeadFetch (TimeoutSec 1) rchanApi rc

        now <- liftIO $ getPOSIXTime <&> round
        pause @'Seconds 1
        next $ WaitRefChanHead rc now

      WaitRefChanHeadStart rc t -> do
        notice $ "wait for refchan head" <+> pretty (AsBase58 rc)
        next (WaitRefChanHead rc t)

      WaitRefChanHead rc t -> do
        now <- liftIO $ getPOSIXTime <&> round
        let s = 60 - (now -t)
        hd <- getRefChanHead @L4Proto sto (RefChanHeadKey rc)

        liftIO $ IO.hPutStr stderr $ show $ "waiting" <+> pretty s <+> "      \r"

        if | now - t < 60 && isNothing hd -> do
             pause @'Seconds 1
             next $ WaitRefChanHead rc t

           | now - t > 60 && isNothing hd -> do
             err "refchan wait timeout"
             next $ SetupExitFailure

           | isJust hd -> do
              next $ RefChanHeadFound rc (fromJust hd)

           | otherwise -> next $ SetupExitFailure

      RefChanHeadFound rc hd -> do
        notice $ "found refchan head for" <+> pretty (AsBase58 rc)
        void $ lift $ callRpcWaitMay  @RpcRefChanFetch (TimeoutSec 1) rchanApi rc

        author <- lift $ asks fixmeEnvAuthor >>= readTVarIO

        let readers = view refChanHeadReaders hd
        let authors = view refChanHeadAuthors hd

        -- hbs2-keyman/hbs2-keyman-direct-lib/HBS2/KeyMan/Keys/Direct.hs
        rs <- liftIO (runKeymanClientRO $ loadKeyRingEntries (HS.toList readers))

        let isReader = case rs of
                           [] -> False
                           _  -> True

        let canRead = if isReader then
                        green "yes"
                      else
                        red "no"

        notice $ "reader:" <+> canRead

        let isAuthor = maybe1 author False (`HS.member` authors)

        let canWrite = if isAuthor
                        then green "yes"
                        else red "no"

        notice $ "author:" <+> canWrite

        unless isReader do
          warn $ yellow "no reader key found" <> line
                 <>  "it's may be ok, if this refchan is not encrypted" <> line
                 <>  "otherwise, make your encryption key a member of this refchan head"
                 <> line

        unless isAuthor do
          warn $ red "no author key found" <> line
                 <>  "it's may be ok if you have only read-only access to this refchan" <> line
                 <>  "otherwise, use" <+> yellow "author KEY" <+> "settings in the .fixme-new/config" <> line
                 <>  "and make sure it is added to the refchan head"
                 <> line

        unless (isJust rchan) do
          notice $ "adding refchan to" <+> pretty confFile
          liftIO do
            appendFile confFile $ show $
                line
                <> vcat [ pretty (refChanClause rc) ]

      SetupExitFailure -> do
        err "refchan init failed"

      SetupNewRefChan -> do

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

           notice $ "adding refchan to" <+> pretty confFile
           liftIO do
             appendFile confFile $ show $
                 line
                 <> vcat [ pretty (refChanClause refchan) ]

           next $ CheckRefChan refchan


