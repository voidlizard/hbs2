{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.Proto.Mailbox.Types
  ( ForMailbox
  , MailboxKey
  , MailboxType(..)
  , MailBoxStatusPayload(..)
  , MailboxServiceError(..)
  , Recipient
  , Sender
  , PolicyVersion
  , MailboxMessagePredicate(..)
  , SimplePredicateExpr(..)
  , SimplePredicate(..)
  , MailBoxProto(..)
  , MailBoxProtoMessage(..)
  , Message(..)
  , MessageContent(..)
  , MessageCompression(..)
  , MessageFlags(..)
  , MessageTimestamp(..)
  , MessageTTL(..)
  , DeleteMessagesPayload(..)
  , SetPolicyPayload(..)
  , module HBS2.Net.Proto.Types
  , HashRef
  ) where

import HBS2.Prelude.Plated

import HBS2.Base58
import HBS2.Hash
import HBS2.Net.Proto.Types
import HBS2.Data.Types.Refs (HashRef(..))

import HBS2.Data.Types.SignedBox
import HBS2.Data.Types.SmallEncryptedBlock(SmallEncryptedBlock(..))
import HBS2.Net.Auth.GroupKeySymm

import Codec.Serialise
import Control.Exception
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Set
import Data.Set qualified as Set
import Data.Word

data MailboxType =
  MailboxHub | MailboxRelay
  deriving stock (Eq,Ord,Show,Generic)

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

type MailboxKey s = PubKey 'Sign s

type Sender s = PubKey 'Sign s

type Recipient s = PubKey 'Sign s

type PolicyVersion = Word32

type ForMailbox s = ( ForGroupKeySymm s
                    , Ord (PubKey 'Sign s)
                    , ForSignedBox s
                    , Pretty (AsBase58 (PubKey 'Sign s))
                    )

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


instance Serialise SimplePredicate
instance Serialise SimplePredicateExpr
instance Serialise MailboxMessagePredicate

newtype MessageTimestamp =
  MessageTimestamp Word64
  deriving newtype (Eq,Ord,Num,Enum,Integral,Real,Pretty,Show,Hashable)
  deriving stock Generic


newtype MessageTTL = MessageTTL Word32
  deriving newtype (Eq,Ord,Num,Enum,Integral,Real,Pretty,Show,Hashable)
  deriving stock Generic


data MessageCompression = GZip
  deriving stock (Eq,Ord,Generic,Show)

data MessageFlags =
  MessageFlags1
  { messageCreated     :: MessageTimestamp
  , messageTTL         :: Maybe MessageTTL
  , messageCompression :: Maybe MessageCompression
  , messageSchema      :: Maybe HashRef -- reserved
  }
  deriving stock (Eq,Ord,Generic,Show)

type MessageRecipient s = PubKey 'Sign s

data SetPolicyPayload s =
  SetPolicyPayload
  { sppMailboxKey    :: MailboxKey s
  , sppPolicyVersion :: PolicyVersion
  , sppPolicyRef     :: HashRef -- ^ merkle tree hash of policy description file
  }
  deriving stock (Generic)

-- for Hashable
deriving instance ForMailbox s => Eq (SetPolicyPayload s)

data MailBoxStatusPayload s =
  MailBoxStatusPayload
  { mbsMailboxPayloadNonce  :: Word64
  , mbsMailboxKey           :: MailboxKey s
  , mbsMailboxType          :: MailboxType
  , mbsMailboxHash          :: Maybe HashRef
  , mbsMailboxPolicy        :: Maybe (SignedBox (SetPolicyPayload s) s)
  }
  deriving stock (Generic)

data DeleteMessagesPayload (s :: CryptoScheme) =
  DeleteMessagesPayload
  { dmpPredicate     :: MailboxMessagePredicate
  }
  deriving stock (Generic)

data MailBoxProtoMessage s e =
    SendMessage      (Message s) -- already has signed box
  | CheckMailbox     (Maybe Word64) (MailboxKey s)
  | MailboxStatus    (SignedBox (MailBoxStatusPayload s) s) -- signed by peer
  | DeleteMessages   (SignedBox (DeleteMessagesPayload s ) s)
  deriving stock (Generic)

data MailBoxProto s e =
  MailBoxProtoV1 { mailBoxProtoPayload :: MailBoxProtoMessage s e }
  deriving stock (Generic)

instance ForMailbox s => Serialise (MailBoxStatusPayload s)
instance ForMailbox s => Serialise (SetPolicyPayload s)
instance ForMailbox s => Serialise (DeleteMessagesPayload s)
instance ForMailbox s => Serialise (MailBoxProtoMessage s e)
instance ForMailbox s => Serialise (MailBoxProto s e)

instance ForMailbox s => Pretty (MailBoxStatusPayload s) where
  pretty MailBoxStatusPayload{..} =
    parens $ "mailbox-status" <> line <> st
    where
      st = indent 2 $
             brackets $
             align $ vcat
                  [ parens ("nonce" <+> pretty mbsMailboxPayloadNonce)
                  , parens ("key"   <+> pretty (AsBase58 mbsMailboxKey))
                  , parens ("type"  <+> pretty mbsMailboxType)
                  , element "mailbox-tree"   mbsMailboxHash
                  , element "set-policy-payload-hash" (HashRef . hashObject . serialise <$> mbsMailboxPolicy)
                  , maybe mempty pretty spp
                  ]

      element el = maybe mempty ( \v -> parens (el <+> pretty v) )

      spp = mbsMailboxPolicy >>= unboxSignedBox0 <&> snd


instance ForMailbox s => Pretty (SetPolicyPayload s) where
  pretty SetPolicyPayload{..} = parens ( "set-policy-payload" <> line <> indent 2 (brackets w) )
    where
      w = align $
            vcat [ parens ( "version" <+> pretty sppPolicyVersion )
                 , parens ( "ref" <+> pretty sppPolicyRef )
                 ]


data MessageContent s =
  MessageContent
  { messageFlags      :: MessageFlags
  , messageRecipients :: Set (MessageRecipient s)
  , messageGK0        :: Either HashRef (GroupKey 'Symm s)
  , messageParts      :: Set HashRef
  , messageData       :: SmallEncryptedBlock ByteString
  }
  deriving stock Generic

data Message s =
  MessageBasic
  { messageContent :: SignedBox (MessageContent s) s
  }
  deriving stock Generic

deriving stock instance ForMailbox s => Eq (MessageContent s)
deriving stock instance ForMailbox s => Eq (Message s)

instance Serialise MessageTimestamp
instance Serialise MessageTTL
instance Serialise MessageCompression
instance Serialise MessageFlags
instance ForMailbox s => Serialise (MessageContent s)
instance ForMailbox s => Serialise (Message s)


data MailboxServiceError =
    MailboxCreateFailed String
  | MailboxOperationError String
  | MailboxSetPolicyFailed String
  | MailboxAuthError String
  deriving stock (Typeable,Show,Generic)

instance Serialise MailboxServiceError
instance Exception MailboxServiceError


