{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
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

import DBPipe.SQLite

import Control.Concurrent.STM qualified as STM
-- import Control.Concurrent.STM.TBQueue
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe
import Lens.Micro.Platform
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO

data MailboxProtoException =
    MailboxProtoWorkerTerminatedException
  | MailboxProtoCantAccessMailboxes FilePath
  | MailboxProtoMailboxDirNotSet
  deriving stock (Show,Typeable)

instance Exception MailboxProtoException

hbs2MailboxDirOpt :: String
hbs2MailboxDirOpt = "hbs2:mailbox:dir"

{- HLINT ignore "Functor law" -}

data MailboxProtoWorker (s :: CryptoScheme) e =
  MailboxProtoWorker
  { mpwPeerEnv            :: PeerEnv e
  , mpwDownloadEnv        :: DownloadEnv e
  , mpwStorage            :: AnyStorage
  , mpwCredentials        :: PeerCredentials s
  , inMessageQueue        :: TBQueue (Message s, MessageContent s)
  , inMessageMergeQueue   :: TVar (HashMap (MailboxRefKey s) (HashSet HashRef))
  , inMessageQueueInNum   :: TVar Int
  , inMessageQueueOutNum  :: TVar Int
  , inMessageQueueDropped :: TVar Int
  , inMessageDeclined     :: TVar Int
  , mailboxDB             :: TVar (Maybe DBPipeEnv)
  }

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

  mailboxDelete MailboxProtoWorker{..} mbox = do

    flip runContT pure $ callCC \exit -> do

      mdbe <- readTVarIO mailboxDB

      dbe <- ContT $ maybe1 mdbe (pure $ Left (MailboxCreateFailed "database not ready"))

      debug $ red "delete fucking mailbox" <+> pretty (MailboxRefKey @s mbox)

      -- TODO: actually-purge-messages-and-attachments
      withDB dbe do
        insert [qc| delete from mailbox where recipient = ? |] (Only (MailboxRefKey @s mbox))

      delRef mpwStorage (MailboxRefKey @s mbox)

      pure $ Right ()

  -- FIXME: refactor
  mailboxSendDelete w@MailboxProtoWorker{..} ref predicate = do
    pure $ Right ()

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

      r <- withDB dbe do
             select_ @_ @(MailboxRefKey s, MailboxType) [qc|select recipient,type from mailbox|]

      pure $ Right r

  mailboxGetStatus MailboxProtoWorker{..} ref = do
    -- TODO: support-policy-ASAP

    now <- liftIO $ getPOSIXTime <&> round

    flip runContT pure do

      mdbe <- readTVarIO mailboxDB

      dbe <- ContT $ maybe1 mdbe (pure $ Left (MailboxCreateFailed "database not ready"))

      t' <- getMailboxType_ dbe ref

      t <- ContT $ maybe1 t' (pure $ Right Nothing)

      v <- getRef mpwStorage ref <&> fmap HashRef

      pure $ Right $ Just $ MailBoxStatusPayload @s now (coerce ref) t v mzero mzero

getMailboxType_ :: (ForMailbox s, MonadIO m) => DBPipeEnv -> MailboxRefKey s -> m (Maybe MailboxType)
getMailboxType_ d r = do
  let sql = [qc|select type from mailbox where recipient = ? limit 1|]
  withDB d do
   select @(Only String) sql (Only r)
     <&> fmap (fromStringMay @MailboxType  . fromOnly)
     <&> headMay . catMaybes

createMailboxProtoWorker :: forall s e m . (MonadIO m, s ~ Encryption e, ForMailbox s)
                         => PeerCredentials s
                         -> PeerEnv e
                         -> DownloadEnv e
                         -> AnyStorage
                         -> m (MailboxProtoWorker s e)
createMailboxProtoWorker pc pe de sto = do
  -- FIXME: queue-size-hardcode
  --   $class: hardcode
  inQ        <- newTBQueueIO 1000
  mergeQ     <- newTVarIO mempty
  inDroppped <- newTVarIO 0
  inNum      <- newTVarIO 0
  outNum     <- newTVarIO 0
  decl       <- newTVarIO 0
  dbe        <- newTVarIO Nothing
  pure $ MailboxProtoWorker pe de sto pc inQ mergeQ inNum outNum inDroppped decl dbe

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
                                     )
             => m [Syntax C]
             -> MailboxProtoWorker s e
             -> m ()

mailboxProtoWorker readConf me@MailboxProtoWorker{..} = do

  pause @'Seconds 1

  flip runContT pure do

    dbe <- lift $ mailboxStateEvolve readConf me

    pipe <- ContT $ withAsync (runPipe dbe)

    inq <- ContT $ withAsync (mailboxInQ dbe)

    mergeQ <- ContT $ withAsync mailboxMergeQ

    bs <- ContT $ withAsync do

      forever do
        pause @'Seconds 10
        debug $ "I'm" <+> yellow "mailboxProtoWorker"

    void $ waitAnyCancel [bs,pipe,inq,mergeQ]

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
            h' <- enqueueBlock sto (serialise (Existed proof ha))

            for_ h' $ \h -> do
              atomically do
                modifyTVar inMessageMergeQueue  (HM.insertWith (<>) ref (HS.singleton (HashRef h)))

            -- TODO: check-attachment-policy-for-mailbox

            -- TODO: ASAP-block-accounting-for-attachment
            for_ (messageParts s) $ \part -> do

              liftIO $ withPeerM mpwPeerEnv $ withDownload mpwDownloadEnv
                $ addDownload @e Nothing (fromHashRef part)

              pure ()

            -- read current mailbox
            -- merge messages into
            -- write current mailbox
            -- put attachments to download

    mailboxMergeQ = do
      let sto = mpwStorage
      -- FIXME: poll-timeout-hardcode?
      let mboxes = readTVarIO inMessageMergeQueue
                    <&> fmap (,2) . HM.keys . HM.filter ( not . HS.null )

      polling (Polling 2 2) mboxes $ \r -> void $ runMaybeT do
        debug $ yellow "mailbox: merge-poll" <+> pretty r

        -- NOTE: reliability
        --   в случае отказа сторейджа все эти сообщения будут потеряны
        --   однако, ввиду дублирования -- они рано или поздно будут
        --   восстановлены с других реплик, если таковые имеются
        newTx <- atomically do
                   n <- readTVar inMessageMergeQueue
                            <&>  fromMaybe mempty . HM.lookup r
                   modifyTVar inMessageMergeQueue  (HM.delete r)
                   pure n

        v <- getRef sto r <&> fmap HashRef
        txs <- maybe1 v (pure mempty) (readLog (liftIO . getBlock sto) )

        let mergedTx = HS.fromList txs <> newTx & HS.toList

        -- FIXME: size-hardcode-again
        let pt = toPTree (MaxSize 6000) (MaxNum 1024) mergedTx
        nref <- makeMerkle 0 pt $ \(_,_,bss) -> void $ liftIO $ putBlock sto bss

        updateRef sto r nref
        debug $ yellow "mailbox updated" <+> pretty r <+> pretty nref

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

  withDB dbe do
    ddl [qc|create table if not exists
             mailbox ( recipient text not null
                     , type      text not null
                     , primary key (recipient)
                     )
           |]

  pure dbe


instance ForMailbox s => ToField (MailboxRefKey s) where
  toField (MailboxRefKey a) = toField (show $ pretty (AsBase58 a))

instance ForMailbox s => FromField (MailboxRefKey s) where
  fromField w = fromField @String w <&> fromString @(MailboxRefKey s)

instance FromField MailboxType where
  fromField w = fromField @String w <&> fromString @MailboxType


