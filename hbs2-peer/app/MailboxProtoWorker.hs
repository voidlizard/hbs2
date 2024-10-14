{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language MultiWayIf #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language PatternSynonyms #-}
module MailboxProtoWorker ( mailboxProtoWorker
                          , createMailboxProtoWorker
                          , MailboxProtoWorker
                          , IsMailboxProtoAdapter
                          , MailboxProtoException(..)
                          , hbs2MailboxDirOpt
                          ) where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Actors.Peer
import HBS2.Data.Types.Refs
import HBS2.Data.Detect
import HBS2.Net.Proto
import HBS2.Base58
import HBS2.Storage
import HBS2.Storage.Operations.Missed
import HBS2.Merkle
import HBS2.Hash
import HBS2.Data.Types.SignedBox
import HBS2.Peer.Proto
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Entry
import HBS2.Net.Messaging.Unix
import HBS2.Net.Auth.Credentials

import HBS2.Polling
import HBS2.System.Dir
import HBS2.Misc.PrettyStuff

import Brains
import PeerConfig
import PeerTypes
import BlockDownload()

import DBPipe.SQLite as Q

import Control.Concurrent.STM qualified as STM
-- import Control.Concurrent.STM.TBQueue
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Except (throwError)
import Data.Coerce
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List qualified as L
import Data.Maybe
import Data.Word
import Data.Hashable
import Codec.Serialise
import Lens.Micro.Platform
import Text.InterpolatedString.Perl6 (qc)
import Streaming.Prelude qualified as S
import UnliftIO

newtype PolicyHash = PolicyHash HashRef
                     deriving newtype (Eq,Ord,Show,Hashable,Pretty)

instance FromField PolicyHash where
  fromField s = PolicyHash . fromString <$> fromField @String s

instance ToField PolicyHash where
  toField f = toField (show $ pretty f)

data MailboxProtoException =
    MailboxProtoWorkerTerminatedException
  | MailboxProtoCantAccessMailboxes FilePath
  | MailboxProtoMailboxDirNotSet
  deriving stock (Show,Typeable)

instance Exception MailboxProtoException

hbs2MailboxDirOpt :: String
hbs2MailboxDirOpt = "hbs2:mailbox:dir"

{- HLINT ignore "Functor law" -}

data PolicyDownload s =
  PolicyDownload
  { policyDownloadWhen :: Word64
  , policyDownloadWhat :: SetPolicyPayload s
  , policyDownloadBox  :: HashRef
  }
  deriving stock (Generic)

instance ForMailbox s => Serialise (PolicyDownload s)

deriving instance ForMailbox s => Eq (PolicyDownload s)

instance ForMailbox s => Hashable (PolicyDownload s) where
  hashWithSalt s p = hashWithSalt s (serialise p)

data MailboxDownload s =
  MailboxDownload
  { mailboxRef         :: MailboxRefKey s
  , mailboxStatusRef   :: HashRef
  , mailboxDownWhen    :: Word64
  , mailboxDownPolicy  :: Maybe PolicyVersion
  , mailboxDownDone    :: Bool
  }
  deriving stock (Generic)

deriving stock instance ForMailbox s => Eq (MailboxDownload s)

instance ForMailbox s => Hashable (MailboxDownload s)

data MailboxProtoWorker (s :: CryptoScheme) e =
  MailboxProtoWorker
  { mpwPeerEnv            :: PeerEnv e
  , mpwDownloadEnv        :: DownloadEnv e
  , mpwStorage            :: AnyStorage
  , mpwCredentials        :: PeerCredentials s
  , mpwFetchQ             :: TVar (HashSet (MailboxRefKey s))
  , inMessageQueue        :: TBQueue (Message s, MessageContent s)
  , inMessageMergeQueue   :: TVar (HashMap (MailboxRefKey s) (HashSet HashRef))
  , inPolicyDownloadQ     :: TVar (HashMap HashRef (PolicyDownload s))
  , inMailboxDownloadQ    :: TVar (HashMap HashRef (MailboxDownload s))
  , inMessageQueueInNum   :: TVar Int
  , inMessageQueueOutNum  :: TVar Int
  , inMessageQueueDropped :: TVar Int
  , inMessageDeclined     :: TVar Int
  , mailboxDB             :: TVar (Maybe DBPipeEnv)
  }

okay :: Monad m => good -> m (Either bad good)
okay good = pure (Right good)

pattern PlainMessageDelete :: forall {s :: CryptoScheme} . HashRef -> DeleteMessagesPayload s
pattern PlainMessageDelete x <- DeleteMessagesPayload (MailboxMessagePredicate1 (Op (MessageHashEq x)))

instance (s ~ HBS2Basic, e ~ L4Proto, s ~ Encryption e) => IsMailboxProtoAdapter s (MailboxProtoWorker s e) where

  mailboxGetCredentials = pure . mpwCredentials

  mailboxGetStorage = pure . mpwStorage

  mailboxAcceptMessage MailboxProtoWorker{..} m c = do
    atomically do
      full <- isFullTBQueue inMessageQueue
      if full then do
        modifyTVar inMessageQueueDropped succ
      else do
        writeTBQueue inMessageQueue (m,c)
        modifyTVar   inMessageQueueInNum succ

  mailboxAcceptDelete MailboxProtoWorker{..} mbox dmp box = do
    debug $ red "<<>> mailbox: mailboxAcceptDelete" <+> pretty mbox

    let sto = mpwStorage
    -- TODO: add-policy-reference

    flip runContT pure do

      h' <- putBlock sto (serialise box)

      h <- ContT $ maybe1 h' storageFail

      let proof = ProofOfDelete (Just (HashRef h))

      let what' = case dmp of
                   PlainMessageDelete x -> Just x
                   _ -> Nothing

      what <- ContT $ maybe1 what' unsupportedPredicate

      let de = Deleted proof what

      deh' <- enqueueBlock sto (serialise (Deleted proof what))
               <&> fmap HashRef

      deh <- ContT $ maybe1 deh' storageFail

      atomically $ modifyTVar inMessageMergeQueue (HM.insert mbox (HS.singleton deh))

    where
      storageFail = err $ red "mailbox (storage:critical)" <+> "block writing failure"
      unsupportedPredicate = err $ red "mailbox (unsuported-predicate)"

instance ( s ~ Encryption e, e ~ L4Proto
         ) => IsMailboxService s (MailboxProtoWorker s e) where
  mailboxCreate MailboxProtoWorker{..} t p = do
    debug $ "mailboxWorker.mailboxCreate" <+> pretty (AsBase58 p) <+> pretty t

    flip runContT pure $ callCC \exit -> do

      mdbe <- readTVarIO mailboxDB

      dbe <- ContT $ maybe1 mdbe (pure $ Left (MailboxCreateFailed "database not ready"))

      r <- liftIO $ try @_ @SomeException $ withDB dbe do
             insert [qc|
             insert into mailbox (recipient,type)
             values (?,?)
             on conflict (recipient) do nothing
                       |] (show $ pretty $ AsBase58 p, show $ pretty t)

      case r of
        Right{} -> pure $ Right ()
        Left{}  -> pure $ Left (MailboxCreateFailed "database operation")

  mailboxSetPolicy me@MailboxProtoWorker{..} sbox = do
    -- check policy version
    -- check policy has peers
    -- write policy block
    -- update reference to policy block
    --
    -- test: write policy, check mailboxGetStatus

    debug $ red "mailboxSetPolicy"

    runExceptT do

      -- check policy signature
      (who, spp) <- unboxSignedBox0 sbox
                      & orThrowError (MailboxAuthError "invalid signature")

      dbe <- readTVarIO mailboxDB
                >>= orThrowError (MailboxSetPolicyFailed "database not ready")

      loaded <- loadPolicyPayloadFor dbe mpwStorage (MailboxRefKey @s who)
                  <&> fmap ( unboxSignedBox0 @(SetPolicyPayload s) @s .  snd )
                  <&> join

      what <- case loaded of
        Nothing -> do
          err $ red "mailboxSetPolicy FUCKED"
          putBlock mpwStorage (serialise sbox)
            >>= orThrowError (MailboxSetPolicyFailed "storage error")
            <&> HashRef

        Just (k, spp0) | sppPolicyVersion spp > sppPolicyVersion spp0 || k /= who -> do
          putBlock mpwStorage (serialise sbox)
            >>= orThrowError (MailboxSetPolicyFailed "storage error")
            <&> HashRef

        _ -> do
         throwError (MailboxSetPolicyFailed "too old")

      liftIO $ withDB dbe $ Q.transactional do
        insert [qc| insert into policy (mailbox,hash) values(?,?)
                    on conflict (mailbox) do update set hash = excluded.hash
                  |] (MailboxRefKey @s who, PolicyHash what)


      void $ runMaybeT do
        msp <- mailboxGetStatus me (MailboxRefKey @s who)
                     >>= toMPlus
                     >>= toMPlus

        creds <- mailboxGetCredentials @s me
        let box = makeSignedBox @s (view peerSignPk creds) (view peerSignSk creds) msp

        liftIO $ withPeerM mpwPeerEnv do
          gossip (MailBoxProtoV1 @s @e (MailboxStatus box))

      pure what

  mailboxDelete MailboxProtoWorker{..} mbox = do

    flip runContT pure do

      mdbe <- readTVarIO mailboxDB

      dbe <- ContT $ maybe1 mdbe (pure $ Left (MailboxOperationError "database not ready"))

      debug $ red "delete fucking mailbox" <+> pretty (MailboxRefKey @s mbox)

      -- TODO: actually-purge-messages-and-attachments
      withDB dbe do
        insert [qc| delete from mailbox where recipient = ? |] (Only (MailboxRefKey @s mbox))

      delRef mpwStorage (MailboxRefKey @s mbox)

      pure $ Right ()

  mailboxSendDelete w@MailboxProtoWorker{..} box = do
    debug $ red "mailboxSendDelete"

    flip runContT pure do

      -- 1. unpack-and-check
      let r = unboxSignedBox0 box

      (k, _) <- ContT $ maybe1 r authFailed

      mdbe <- readTVarIO mailboxDB

      dbe <- ContT $ maybe1 mdbe dbNotReady

      t <- getMailboxType_ dbe (MailboxRefKey @s k)

      void $ ContT $ maybe1 t (noMailbox k)

      -- 2. what?
      -- gossip and shit

      liftIO $ withPeerM mpwPeerEnv do
        me <- ownPeer @e
        runResponseM me $ do
          mailboxProto @e True w (MailBoxProtoV1 (DeleteMessages box))

      okay ()

    where
      dbNotReady = pure $ Left (MailboxOperationError "database not ready")
      authFailed = pure $ Left (MailboxAuthError "inconsistent signature")
      noMailbox k = pure $
        Left (MailboxOperationError (show $ "no mailox" <+> pretty (AsBase58 k)))


  mailboxSendMessage w@MailboxProtoWorker{..} mess = do
    -- we do not check message signature here
    -- because it will be checked in the protocol handler anyway
    liftIO $ withPeerM mpwPeerEnv do
      me <- ownPeer @e
      runResponseM me $ do
        mailboxProto @e True w (MailBoxProtoV1 (SendMessage mess))

    pure $ Right ()

  mailboxListBasic  MailboxProtoWorker{..} = do

    flip runContT pure do

      mdbe <- readTVarIO mailboxDB

      dbe <- ContT $ maybe1 mdbe (pure $ Left (MailboxCreateFailed "database not ready"))

      debug $ red "mailboxListBasic"

      r <- listMailboxes dbe

      pure $ Right r

  mailboxAcceptStatus me@MailboxProtoWorker{..} ref who s2@MailBoxStatusPayload{..} = do
    -- TODO: implement-policy-first
    --   итак, мы не можем двигаться, пока не будет реализована policy.


    flip runContT pure $ callCC \stop -> do

      s0 <- runMaybeT do
        MailBoxStatusPayload{..} <- mailboxGetStatus me ref
                                      >>= toMPlus
                                      >>= toMPlus
        toMPlus mbsMailboxHash

      now <- liftIO $ getPOSIXTime <&> round

      mdbe <- readTVarIO mailboxDB

      dbe <- ContT $ maybe1 mdbe (pure $ Left (MailboxCreateFailed "database not ready"))

      p0 <- loadPolicyPayloadFor dbe mpwStorage ref
              <&> fmap (sppPolicyVersion . snd) . ((unboxSignedBox0 . snd) =<<)
              <&> fromMaybe 0

      let bogusPolicyMessage  =
              err $ red "!!! arrived invalid policy signature for"
                  <+> pretty ref
                  <+> "from"
                  <+> pretty (AsBase58 who)

      let downloadStatus v = do
            maybe1 mbsMailboxHash (okay ()) $ \h -> do
              when (s0 /= Just h) do
                startDownloadStuff me h
                -- one download per version per hash
                let downKey = HashRef $ hashObject (serialise (v,h))
                atomically $ modifyTVar inMailboxDownloadQ (HM.insert downKey (MailboxDownload ref h now v False))
              okay ()

      case mbsMailboxPolicy of
        Nothing -> downloadStatus Nothing

        Just newPolicy -> do

          -- TODO: handle-invalid-policy-error
          --   not "okay" actually

          (rcptKey, pNew) <- ContT $ maybe1 (unboxSignedBox0 newPolicy)
                                            (bogusPolicyMessage >> okay ())

          when (coerce rcptKey /= ref) $ lift bogusPolicyMessage >> stop (Right ())

          when (sppPolicyVersion pNew > p0) do
            startDownloadStuff me (sppPolicyRef pNew)

            mph <- putBlock mpwStorage (serialise newPolicy)

            for_ mph $ \ph -> do
              let insActually = HM.insert (sppPolicyRef pNew) (PolicyDownload now pNew (HashRef ph))
              atomically $ modifyTVar inPolicyDownloadQ insActually

          let v = Just $ max p0 (sppPolicyVersion pNew)

          downloadStatus v

  mailboxGetStatus MailboxProtoWorker{..} ref = do
    -- TODO: support-policy-ASAP

    now <- liftIO $ getPOSIXTime <&> round

    flip runContT pure do

      mdbe <- readTVarIO mailboxDB

      dbe <- ContT $ maybe1 mdbe (pure $ Left (MailboxCreateFailed "database not ready"))

      t' <- getMailboxType_ dbe ref

      t <- ContT $ maybe1 t' (pure $ Right Nothing)

      v <- getRef mpwStorage ref <&> fmap HashRef

      spp <- loadPolicyPayloadFor dbe mpwStorage ref
               <&> fmap snd

      pure $ Right $ Just $ MailBoxStatusPayload @s now (coerce ref) t v spp

  mailboxFetch MailboxProtoWorker{..} ref = do
    debug $ red "mailboxFetch" <+> pretty ref
    atomically (modifyTVar mpwFetchQ (HS.insert ref))
    okay ()

startDownloadStuff :: forall s e m . (ForMailbox s, s ~ Encryption e, MyPeer e, MonadIO m)
              => MailboxProtoWorker s e
              -> HashRef
              -> m ()

startDownloadStuff MailboxProtoWorker{..} href = do
  liftIO $ withPeerM mpwPeerEnv $ withDownload mpwDownloadEnv
    $ do
      debug $ "startDownloadStuff" <+> pretty href
      addDownload @e Nothing (coerce href)

listMailboxes :: forall s m . (ForMailbox s, MonadIO m)
              => DBPipeEnv
              -> m [(MailboxRefKey s, MailboxType)]
listMailboxes dbe = do
  withDB dbe do
   select_ [qc|select recipient,type from mailbox|]

loadPolicyPayloadFor :: forall s m . (ForMailbox s, MonadIO m)
              => DBPipeEnv
              -> AnyStorage
              -> MailboxRefKey s
              -> m (Maybe (HashRef, SignedBox (SetPolicyPayload s) s))
loadPolicyPayloadFor dbe sto who = do
  phash <- withDB dbe do
             select @(Only PolicyHash) [qc|select hash from policy where mailbox = ?|] (Only who)
             <&> fmap (coerce @_ @HashRef . fromOnly)
             <&> headMay

  runMaybeT do
     ha <- toMPlus phash
     what <- getBlock sto (coerce ha)
                >>= toMPlus
                <&> deserialiseOrFail
                >>= toMPlus
     pure (ha, what)


loadPolicyPayloadUnboxed :: forall s m . (ForMailbox s, MonadIO m)
              => DBPipeEnv
              -> AnyStorage
              -> MailboxRefKey s
              -> m (Maybe (SetPolicyPayload s))
loadPolicyPayloadUnboxed dbe sto mbox = do
  loadPolicyPayloadFor dbe sto mbox
   <&> fmap snd
   <&> fmap unboxSignedBox0
   <&> join
   <&> fmap snd

getMailboxType_ :: (ForMailbox s, MonadIO m) => DBPipeEnv -> MailboxRefKey s -> m (Maybe MailboxType)
getMailboxType_ d r = do
  let sql = [qc|select type from mailbox where recipient = ? limit 1|]
  withDB d do
   select @(Only String) sql (Only r)
     <&> fmap (fromStringMay @MailboxType  . fromOnly)
     <&> headMay . catMaybes

createMailboxProtoWorker :: forall s e m . ( MonadIO m
                                           , s ~ Encryption e
                                           , ForMailbox s
                                           )
                         => PeerCredentials s
                         -> PeerEnv e
                         -> DownloadEnv e
                         -> AnyStorage
                         -> m (MailboxProtoWorker s e)
createMailboxProtoWorker pc pe de sto = do
  -- FIXME: queue-size-hardcode
  --   $class: hardcode
  MailboxProtoWorker pe de sto pc
    <$> newTVarIO mempty
    <*> newTBQueueIO 8000
    <*> newTVarIO mempty
    <*> newTVarIO mempty
    <*> newTVarIO mempty
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO Nothing

mailboxProtoWorker :: forall e s m . ( MonadIO m
                                     , MonadUnliftIO m
                                     , MyPeer e
                                     , HasStorage m
                                     , Sessions e (KnownPeer e) m
                                     , HasGossip e (MailBoxProto s e) m
                                     , Signatures s
                                     , s ~ Encryption e
                                     , IsRefPubKey s
                                     , ForMailbox s
                                     , m ~ PeerM e IO
                                     , e ~ L4Proto
                                     )
             => m [Syntax C]
             -> MailboxProtoWorker s e
             -> m ()

mailboxProtoWorker readConf me@MailboxProtoWorker{..} = do

  pause @'Seconds 1

  flip runContT pure do

    dbe <- lift $ mailboxStateEvolve readConf me

    dpipe <- ContT $ withAsync (runPipe dbe)

    inq <- ContT $ withAsync (mailboxInQ dbe)

    mergeQ <- ContT $ withAsync mailboxMergeQ

    mCheckQ <- ContT $ withAsync (mailboxCheckQ dbe)

    mFetchQ <- ContT $ withAsync (mailboxFetchQ dbe)

    pDownQ <- ContT $ withAsync (policyDownloadQ dbe)

    sDownQ <- ContT $ withAsync stateDownloadQ

    bs <- ContT $ withAsync do

      forever do
        pause @'Seconds 10
        debug $ "I'm" <+> yellow "mailboxProtoWorker"

    void $ waitAnyCancel [bs,dpipe,inq,mergeQ,pDownQ,sDownQ,mCheckQ,mFetchQ]

    `catch` \( e :: MailboxProtoException ) -> do
      err $ red "mailbox protocol worker terminated" <+> viaShow e

    `finally` do
      warn $ yellow "mailbox protocol worker exited"

  where

    mailboxInQ dbe = do
      let sto = mpwStorage
      forever do
        pause @'Seconds 10
        mess <- atomically $ STM.flushTBQueue inMessageQueue
        for_ mess $ \(m,s) -> do
          atomically $ modifyTVar inMessageQueueInNum pred

          -- TODO: process-with-policy

          for_ (messageRecipients s) $ \rcpt -> void $ runMaybeT do
            mbox <- getMailboxType_ @s dbe (MailboxRefKey rcpt)
                       >>= toMPlus

            -- TODO: ASAP-block-accounting
            ha' <- putBlock sto (serialise m) <&> fmap HashRef

            ha <- case ha' of
                    Just x -> pure x
                    Nothing -> do
                      err $ red "storage error, can't store message"
                      mzero

            let ref = MailboxRefKey @s rcpt

            debug $ yellow "mailbox: message stored" <+> pretty ref <+> pretty ha

            -- TODO: add-policy-reference
            let proof = ProofOfExist mzero
            h' <- enqueueBlock sto (serialise (Exists proof ha))

            for_ h' $ \h -> do
              atomically do
                modifyTVar inMessageMergeQueue  (HM.insertWith (<>) ref (HS.singleton (HashRef h)))

            -- TODO: check-attachment-policy-for-mailbox

            -- TODO: ASAP-block-accounting-for-attachment
            for_ (messageParts s) (startDownloadStuff me)

            -- read current mailbox
            -- merge messages into
            -- write current mailbox
            -- put attachments to download

    mailboxMergeQ = do
      let sto = mpwStorage
      -- FIXME: poll-timeout-hardcode?
      let mboxes = readTVarIO inMessageMergeQueue
                    <&> fmap (,2) . HM.keys . HM.filter ( not . HS.null )

      polling (Polling 2 5) mboxes $ \r -> void $ runMaybeT do
        debug $ yellow "mailbox: merge-poll" <+> pretty r

        -- NOTE: reliability
        --   в случае отказа сторейджа все эти сообщения будут потеряны
        --   однако, ввиду дублирования -- они рано или поздно будут
        --   восстановлены с других реплик, если таковые имеются.
        --
        --   Кроме того, мы можем писать WAL.
        --
        newTx <- atomically do
                   n <- readTVar inMessageMergeQueue
                            <&>  fromMaybe mempty . HM.lookup r
                   modifyTVar inMessageMergeQueue  (HM.delete r)
                   pure n

        wipTx <- newTVarIO HS.empty

        newTxProvenL <- S.toList_ $
          for_ newTx $ \th -> void $ runMaybeT do

            tx <- getBlock sto (coerce th)
                    >>= toMPlus

            case deserialiseOrFail tx of

              Left{} -> do
                -- here, but lame
                void $ putBlock sto (serialise (MergedEntry r th))

              -- maybe to something more sophisticated
              Right (Exists{}) -> lift $ S.yield th

              Right (Deleted (ProofOfDelete{..}) _) -> do
                h   <- toMPlus deleteMessage

                box <- getBlock sto (coerce h)
                         >>= toMPlus
                         <&> deserialiseOrFail @(SignedBox (DeleteMessagesPayload s) s)
                         >>= toMPlus

                debug $ red "<<***>> mailbox:" <+> "found proof of message deleting" <+> pretty h

                (pk,_) <- unboxSignedBox0 box & toMPlus

                guard (MailboxRefKey pk == r)

                debug $ red "<<***>> mailbox:" <+> "PROVEN message deleting" <+> pretty h

                lift $ S.yield th

        let newTxProven = HS.fromList newTxProvenL

        v <- getRef sto r <&> fmap HashRef
        txs <- maybe1 v (pure mempty) (readLog (liftIO . getBlock sto) )

        let mergedTx = HS.fromList txs <> newTxProven & HS.toList

        -- FIXME: size-hardcode-again
        let pt = toPTree (MaxSize 6000) (MaxNum 1024) mergedTx
        nref <- makeMerkle 0 pt $ \(_,_,bss) -> void $ liftIO $ putBlock sto bss

        updateRef sto r nref
        debug $ yellow "mailbox updated" <+> pretty r <+> pretty nref

        for_ newTxProven $ \t -> do
          -- FIXME: use-bloom-filter-or-something
          --  $class: leak
          putBlock sto (serialise (MergedEntry r t))

    policyDownloadQ dbe = do

      -- FIXME: too-often-checks-affect-performance
      --   $class: performance
      let policies = readTVarIO inPolicyDownloadQ
                        <&> HM.toList
                        <&> fmap (,1)

      polling (Polling 10 10) policies $ \(pk,PolicyDownload{..}) -> do
        done <- findMissedBlocks mpwStorage pk <&> L.null

        when done $ flip runContT pure do

          let mbox = MailboxRefKey (sppMailboxKey policyDownloadWhat)

          current <- loadPolicyPayloadUnboxed @s dbe mpwStorage mbox
                       <&> fmap sppPolicyVersion
                       <&> fromMaybe 0

          let downloaded = sppPolicyVersion policyDownloadWhat

          mlbs <- getBlock mpwStorage (coerce policyDownloadBox)

          lbs  <- ContT $ maybe1 mlbs (err $ red "storage fail: missed block" <+> pretty pk)

          let msp = deserialiseOrFail @(SignedBox (SetPolicyPayload s) s) lbs
                     & either (const Nothing) Just

          spb <- ContT $ maybe1 msp (err $ red "storage fail: corrupted block" <+> pretty pk)

          when (downloaded > current) do
            void $ mailboxSetPolicy  me spb

          atomically $ modifyTVar inPolicyDownloadQ (HM.delete pk)

    stateDownloadQ = do

      let mail = readTVarIO inMailboxDownloadQ
                        <&> HM.toList
                        <&> fmap (,10)

      polling (Polling 2 2) mail $ \(pk, down@MailboxDownload{..}) -> do
        done <- findMissedBlocks mpwStorage mailboxStatusRef <&> L.null

        fails <- newTVarIO 0

        when (done && not mailboxDownDone) do
          atomically $ modifyTVar inMailboxDownloadQ (HM.insert pk (down { mailboxDownDone = True }))
          debug $ "mailbox state downloaded" <+> pretty pk

        when done do
          debug $ "mailbox/debug: drop state" <+> pretty pk <+> pretty mailboxStatusRef

          -- FIXME: assume-huge-mailboxes

          walkMerkle @[HashRef] (coerce mailboxStatusRef) (getBlock mpwStorage) $ \case
            Left what -> do
              err $ red "mailbox: missed block for tree" <+> pretty mailboxStatusRef <+> pretty what
              atomically $ modifyTVar fails succ

            Right hs  ->  do
              for_ hs $ \h -> void $ runMaybeT do
                debug $ red ">>>" <+> "MERGE MAILBOX ENTRY" <+> pretty h

                -- FIXME: invent-better-filter
                --  $class: leak
                let mergedEntry = serialise (MergedEntry mailboxRef h)
                let mergedH = mergedEntry & hashObject

                already <- getBlock mpwStorage mergedH

                when (isJust already) do
                  debug $ red "!!!" <+> "skip already merged tx" <+> pretty h
                  mzero

                entry' <- getBlock mpwStorage (coerce h)

                when (isNothing entry') do
                  startDownloadStuff me h
                  atomically $ modifyTVar fails succ
                  mzero

                entry <- toMPlus entry'
                           <&> deserialiseOrFail @MailboxEntry
                           >>= toMPlus

                case entry of
                  Deleted{} -> do
                    atomically $ modifyTVar inMessageMergeQueue (HM.insert mailboxRef (HS.singleton h))
                    -- write-already-merged

                  Exists _ w -> do
                    debug $ red ">>>" <+> blue "TX: Exists" <+> pretty w
                    msg' <- getBlock mpwStorage (coerce w)

                    case msg' of
                      Nothing -> do
                        debug $ red  "START DOWNLOAD" <+> pretty w
                        startDownloadStuff me w
                        atomically $ modifyTVar fails succ
                        mzero

                      Just msg -> do
                        let mess = deserialiseOrFail @(Message s) msg

                        case mess of
                          Left{} -> do
                            warn $ "malformed message" <+> pretty w
                            void $ putBlock mpwStorage mergedEntry

                          Right normal -> do
                            let checked = unboxSignedBox0 (messageContent normal)

                            case checked of
                              Nothing -> do
                                warn $ "invalid signature for message" <+> pretty w
                                void $ putBlock mpwStorage mergedEntry

                              Just (_, content) -> do
                                -- FIXME: what-if-message-queue-full?
                                mailboxAcceptMessage me normal content
                                pure ()

          failNum <- readTVarIO fails

          when (failNum == 0) do
            debug $ "mailbox state process succeed" <+> pretty mailboxStatusRef
            atomically $ modifyTVar inMailboxDownloadQ (HM.delete pk)

    mailboxFetchQ dbe = forever do
      toFetch <- atomically $ do
        q <- readTVar mpwFetchQ
        when (HS.null q) STM.retry
        writeTVar mpwFetchQ mempty
        pure q

      for_ toFetch $ \r -> do
        t <- getMailboxType_ dbe r
        maybe1 t none $ \_ -> do
          debug $ yellow "mailbox: SEND FETCH REQUEST FOR" <+> pretty r
          now <- liftIO (getPOSIXTime <&> round)
          gossip (MailBoxProtoV1 @s @e (CheckMailbox  (Just now) (coerce r)))

    mailboxCheckQ dbe = do

      -- FIXME: mailbox-check-period
      --   right now  it's 60 seconds for debug purposes
      --   remove hardcode to smth reasonable
      let mboxes = liftIO (listMailboxes @s dbe <&> fmap (set _2 60) )

      polling (Polling 10 10) mboxes $ \r -> do
        debug $ yellow "mailbox: SEND FETCH REQUEST FOR" <+> pretty r
        now <- liftIO (getPOSIXTime <&> round)
        gossip (MailBoxProtoV1 @s @e (CheckMailbox  (Just now) (coerce r)))

mailboxStateEvolve :: forall e s m . ( MonadIO m
                                     , MonadUnliftIO m
                                     , HasStorage m
                                     , s ~ Encryption e
                                     )
                   => m [Syntax C]
                   -> MailboxProtoWorker s e -> m DBPipeEnv

mailboxStateEvolve readConf MailboxProtoWorker{..}  = do

  conf <- readConf

  debug $ red "mailboxStateEvolve" <> line <> pretty conf

  mailboxDir <- lastMay [ dir
                        | ListVal [StringLike o, StringLike dir] <- conf
                        , o == hbs2MailboxDirOpt
                        ]
                  & orThrow MailboxProtoMailboxDirNotSet

  r <- try @_ @SomeException (mkdir mailboxDir)

  either (const $ throwIO (MailboxProtoCantAccessMailboxes mailboxDir)) dontHandle r

  dbe <- newDBPipeEnv dbPipeOptsDef (mailboxDir </> "state.db")

  atomically $ writeTVar mailboxDB (Just dbe)

  withDB dbe $ Q.transactional do
    ddl [qc|create table if not exists
             mailbox ( recipient text not null
                     , type      text not null
                     , primary key (recipient)
                     )
           |]

    ddl [qc|create table if not exists
             policy ( mailbox   text not null
                    , hash      text not null
                    , primary key (mailbox)
                    )
           |]

  pure dbe


instance ForMailbox s => ToField (MailboxRefKey s) where
  toField (MailboxRefKey a) = toField (show $ pretty (AsBase58 a))

instance ForMailbox s => FromField (MailboxRefKey s) where
  fromField w = fromField @String w <&> fromString @(MailboxRefKey s)

instance FromField MailboxType where
  fromField w = fromField @String w <&> fromString @MailboxType

-- TODO: test-multiple-recipients

-- TODO: implement-basic-policy

-- TODO: test-basic-policy



