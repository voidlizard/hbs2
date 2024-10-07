{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.Proto.Mailbox.Types
  ( ForMailbox
  , MailboxKey
  , Recipient
  , Sender
  , PolicyVersion
  , MailboxMessagePredicate(..)
  , SimplePredicate(..)
  , SimplePredicateExpr(..)
  , module HBS2.Net.Proto.Types
  , HashRef
  ) where

import HBS2.Prelude.Plated

import HBS2.Net.Proto.Types
import HBS2.Data.Types.Refs (HashRef)

import HBS2.Data.Types.SignedBox
import HBS2.Net.Auth.GroupKeySymm

import Data.Word (Word32)
import Codec.Serialise

type MailboxKey s = PubKey 'Sign s

type Sender s = PubKey 'Sign s

type Recipient s = PubKey 'Sign s

type PolicyVersion = Word32

data SimplePredicateExpr =
    And SimplePredicateExpr SimplePredicateExpr
  | Or  SimplePredicateExpr SimplePredicateExpr
  | Op  SimplePredicate
  | End
  deriving stock (Generic)

data SimplePredicate =
    Nop
  | MessageHashEq HashRef
  deriving stock (Generic)

data MailboxMessagePredicate =
  MailboxMessagePredicate1 SimplePredicateExpr
  deriving stock (Generic)


type ForMailbox s = ( ForGroupKeySymm s
                    , Ord (PubKey 'Sign s)
                    , ForSignedBox s
                    )

instance Serialise SimplePredicate
instance Serialise SimplePredicateExpr
instance Serialise MailboxMessagePredicate

