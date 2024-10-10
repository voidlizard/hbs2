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
import HBS2.Net.Proto
import HBS2.Base58
import HBS2.Storage
import HBS2.Storage.Operations.Missed
import HBS2.Hash
import HBS2.Peer.Proto
import HBS2.Peer.Proto.Mailbox
import HBS2.Net.Messaging.Unix
import HBS2.Net.Auth.Credentials

import HBS2.System.Dir
import HBS2.Misc.PrettyStuff

import Brains
import PeerConfig
import PeerTypes

import DBPipe.SQLite

import Control.Monad.Trans.Cont
import Data.Maybe
import UnliftIO
import Control.Concurrent.STM qualified as STM
-- import Control.Concurrent.STM.TBQueue
import Lens.Micro.Platform
import Text.InterpolatedString.Perl6 (qc)

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
  , inMessageQueue        :: TBQueue (Message s, MessageContent s)
  , inMessageQueueInNum   :: TVar Int
  , inMessageQueueOutNum  :: TVar Int
  , inMessageQueueDropped :: TVar Int
  , inMessageDeclined     :: TVar Int
  , mailboxDB             :: TVar (Maybe DBPipeEnv)
  }

instance (s ~ HBS2Basic, e ~ L4Proto, s ~ Encryption e) => IsMailboxProtoAdapter s (MailboxProtoWorker s e) where
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

  mailboxSendMessage w@MailboxProtoWorker{..} mess = do
    -- we do not check message signature here
    -- because it will be checked in the protocol handler anyway
    liftIO $ withPeerM mpwPeerEnv do
      me <- ownPeer @e
      runResponseM me $ do
        mailboxProto @e True w (MailBoxProtoV1 (SendMessage mess))

    pure $ Right ()

getMailboxType_ :: (ForMailbox s, MonadIO m) => DBPipeEnv -> Recipient s -> m (Maybe MailboxType)
getMailboxType_ d r = do
  let sql = [qc|select type from mailbox where recipient = ? limit 1|]
  withDB d do
   select @(Only String) sql (Only (show $ pretty (AsBase58 r)))
     <&> fmap (fromStringMay @MailboxType  . fromOnly)
     <&> headMay . catMaybes

createMailboxProtoWorker :: forall e m . MonadIO m
                         => PeerEnv e
                         -> DownloadEnv e
                         -> AnyStorage
                         -> m (MailboxProtoWorker (Encryption e) e)
createMailboxProtoWorker pe de sto = do
  -- FIXME: queue-size-hardcode
  --   $class: hardcode
  inQ        <- newTBQueueIO 1000
  inDroppped <- newTVarIO 0
  inNum      <- newTVarIO 0
  outNum     <- newTVarIO 0
  decl       <- newTVarIO 0
  dbe        <- newTVarIO Nothing
  pure $ MailboxProtoWorker pe de sto inQ inNum outNum inDroppped decl dbe

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
                                     )
             => m [Syntax C]
             -> MailboxProtoWorker s e
             -> m ()

mailboxProtoWorker readConf me@MailboxProtoWorker{..} = do

  pause @'Seconds 10

  flip runContT pure do

    dbe <- lift $ mailboxStateEvolve readConf me

    pipe <- ContT $ withAsync (runPipe dbe)

    inq <- ContT $ withAsync (mailboxInQ dbe)

    sendq <- ContT $ withAsync $ mailboxSendQ

    bs <- ContT $ withAsync do

      forever do
        pause @'Seconds 10
        debug $ "I'm" <+> yellow "mailboxProtoWorker"

    void $ waitAnyCancel [bs,pipe,inq,sendq]

    `catch` \( e :: MailboxProtoException ) -> do
      err $ red "mailbox protocol worker terminated" <+> viaShow e

    `finally` do
      warn $ yellow "mailbox protocol worker exited"

  where

    mailboxSendQ = do
      forever do
        pause @'Seconds 10
        debug $ yellow "send mail loop"

    mailboxInQ dbe = do
      forever do
        pause @'Seconds 10
        mess <- atomically $ STM.flushTBQueue inMessageQueue
        for_ mess $ \(m,s) -> do
          atomically $ modifyTVar inMessageQueueInNum pred
          -- FIXME: remove
          let ha = hashObject @HbSync (serialise  m)
          -- сохраняем или нет?
          -- по госсипу уже послали. сохранять надо, только если
          -- у нас есть ящик
          debug $ yellow "received message" <+> pretty (AsBase58 (HashRef ha))

          -- TODO: process-with-policy

          for_ (messageRecipients s) $ \rcpt -> do
            mbox <- getMailboxType_ @s dbe rcpt
            pure ()

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

