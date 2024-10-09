{-# Language AllowAmbiguousTypes #-}
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
import HBS2.Net.Auth.Credentials

import HBS2.System.Dir
import HBS2.Misc.PrettyStuff

import Brains
import PeerConfig
import PeerTypes

import DBPipe.SQLite

import Control.Monad.Trans.Cont
import UnliftIO
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
  { mpwStorage            :: AnyStorage
  , inMessageQueue        :: TBQueue (Message s, MessageContent s)
  , inMessageQueueDropped :: TVar Int
  , mailboxDB             :: TVar (Maybe DBPipeEnv)
  }

instance (s ~ HBS2Basic) => IsMailboxProtoAdapter s (MailboxProtoWorker s e) where
  mailboxGetStorage = pure . mpwStorage

  mailboxAcceptMessage MailboxProtoWorker{..} m c = do
    atomically do
      full <- isFullTBQueue inMessageQueue
      if full then do
        modifyTVar inMessageQueueDropped succ
      else do
        writeTBQueue inMessageQueue (m,c)

instance (s ~ HBS2Basic) => IsMailboxService s (MailboxProtoWorker s e) where
  mailboxCreate _ t p = do
    debug $ "mailboxWorker.mailboxCreate" <+> pretty (AsBase58 p) <+> pretty t
    pure $ Right ()

createMailboxProtoWorker :: forall e m . MonadIO m
                         => AnyStorage
                         -> m (MailboxProtoWorker (Encryption e) e)
createMailboxProtoWorker sto = do
  -- FIXME: queue-size-hardcode
  --   $class: hardcode
  inQ        <- newTBQueueIO 1000
  inDroppped <- newTVarIO 0
  dbe        <- newTVarIO Nothing
  pure $ MailboxProtoWorker sto inQ inDroppped dbe

mailboxProtoWorker :: forall e s m . ( MonadIO m
                                     , MonadUnliftIO m
                                     , MyPeer e
                                     , HasStorage m
                                     , Sessions e (KnownPeer e) m
                                     , HasGossip e (MailBoxProto s e) m
                                     , Signatures s
                                     , s ~ Encryption e
                                     , IsRefPubKey s
                                     )
             => m [Syntax C]
             -> MailboxProtoWorker s e
             -> m ()

mailboxProtoWorker readConf me = do

  pause @'Seconds 10

  flip runContT pure do

    dbe <- lift $ mailboxStateEvolve readConf me

    pipe <- ContT $ withAsync (runPipe dbe)

    inq <- ContT $ withAsync mailboxInQ

    bs <- ContT $ withAsync do

      forever do
        pause @'Seconds 10
        debug $ "I'm" <+> yellow "mailboxProtoWorker"

    void $ waitAnyCancel [bs,pipe]

    `catch` \( e :: MailboxProtoException ) -> do
      err $ "mailbox protocol worker terminated" <+> viaShow e

    `finally` do
      warn $ yellow "mailbox protocol worker exited"

  where
    mailboxInQ = do
      forever do
        pause @'Seconds 10
        debug "mailbox check inQ"


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

