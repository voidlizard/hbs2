{-# Language AllowAmbiguousTypes #-}
module MailboxProtoWorker ( mailboxProtoWorker
                          , createMailboxProtoWorker
                          , MailboxProtoWorker
                          , IsMailboxProtoAdapter
                          ) where

import HBS2.Prelude.Plated
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

import HBS2.Misc.PrettyStuff

import Brains
import PeerConfig
import PeerTypes

import Control.Monad
import UnliftIO
-- import Control.Concurrent.STM.TBQueue
import Lens.Micro.Platform

{- HLINT ignore "Functor law" -}

data MailboxProtoWorker (s :: CryptoScheme) e =
  MailboxProtoWorker
  { mpwStorage            :: AnyStorage
  , inMessageQueue        :: TBQueue (Message s, MessageContent s)
  , inMessageQueueDropped :: TVar Int
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

createMailboxProtoWorker :: forall e m . MonadIO m => AnyStorage -> m (MailboxProtoWorker (Encryption e) e)
createMailboxProtoWorker sto = do
  -- FIXME: queue-size-hardcode
  --   $class: hardcode
  inQ        <- newTBQueueIO 1000
  inDroppped <- newTVarIO 0
  pure $ MailboxProtoWorker sto inQ inDroppped

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
             => MailboxProtoWorker s e
             -> m ()

mailboxProtoWorker me = do
  forever do
    pause @'Seconds 10
    debug $ "I'm" <+> yellow "mailboxProtoWorker"

--    let listRefs = listPolledRefs @e brains (Just "lwwref")
--                    <&> fmap (\(a,_,b) -> (a,b))
--                    <&> fmap (over _2 ( (*60) . fromIntegral) )

--    polling (Polling 5 5) listRefs $ \ref -> do
--     debug $ yellow "POLLING LWWREF" <+> pretty (AsBase58 ref)
--     gossip (LWWRefProto1 @e (LWWProtoGet (LWWRefKey ref)))


