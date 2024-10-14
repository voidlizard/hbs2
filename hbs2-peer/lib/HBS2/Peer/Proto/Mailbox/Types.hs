{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.Proto.Mailbox.Types
  ( ForMailbox
  , MailboxKey
  , MailboxType(..)
  , MailboxServiceError(..)
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

import HBS2.Base58
import HBS2.Net.Proto.Types
import HBS2.Data.Types.Refs (HashRef)

import HBS2.Data.Types.SignedBox
import HBS2.Net.Auth.GroupKeySymm

import Data.Word (Word32)
import Codec.Serialise
import Data.Maybe
import Control.Exception

data MailboxServiceError =
    MailboxCreateFailed String
  | MailboxOperationError String
  | MailboxSetPolicyFailed String
  | MailboxAuthError String
  deriving stock (Typeable,Show,Generic)

instance Serialise MailboxServiceError
instance Exception MailboxServiceError

data MailboxType =
  MailboxHub | MailboxRelay
  deriving stock (Eq,Ord,Show,Generic)

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
                    , Pretty (AsBase58 (PubKey 'Sign s))
                    )

instance Serialise SimplePredicate
instance Serialise SimplePredicateExpr
instance Serialise MailboxMessagePredicate
instance Serialise MailboxType

instance Pretty MailboxType where
  pretty = \case
    MailboxHub -> "hub"
    MailboxRelay -> "relay"

instance FromStringMaybe MailboxType where
  fromStringMay = \case
    "hub"   -> Just MailboxHub
    "relay" -> Just MailboxRelay
    _       -> Nothing

instance IsString MailboxType where
  fromString s = fromMaybe (error "invalid MailboxType value") (fromStringMay s)

