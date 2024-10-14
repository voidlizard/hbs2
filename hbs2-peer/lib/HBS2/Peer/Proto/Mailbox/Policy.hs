{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2.Peer.Proto.Mailbox.Policy where

import HBS2.Prelude.Plated

import HBS2.Peer.Proto.Mailbox.Types
-- import HBS2.Peer.Proto.Mailbox


class ForMailbox s => IsAcceptPolicy s a where

  policyAcceptPeer :: forall m . MonadIO m
                   => a
                   -> PubKey 'Sign s -- ^ peer
                   -> m Bool

  policyAcceptMessage :: forall m . MonadIO m
                      => a
                      -> Sender s
                      -> MessageContent s
                      -> m Bool



data AnyPolicy s = forall a . (ForMailbox s, IsAcceptPolicy s a) => AnyPolicy { thePolicy :: a }

instance ForMailbox s => IsAcceptPolicy s (AnyPolicy s) where
  policyAcceptPeer (AnyPolicy p) = policyAcceptPeer @s p
  policyAcceptMessage (AnyPolicy p) = policyAcceptMessage @s p

