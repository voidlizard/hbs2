module FakeMessaging
  ( module FakeMessaging
  , module HBS2.Net.Messaging.Fake
  )
  where

import HBS2.Net.Proto
import HBS2.Net.Messaging.Fake

import Data.Hashable
import Prettyprinter


data Fake

instance HasPeer Fake where
  newtype instance Peer Fake = FakePeer Int
                               deriving newtype (Hashable,Num,Enum)
                               deriving stock (Eq,Ord,Show)


instance Pretty (Peer Fake) where
  pretty (FakePeer n) = parens ("peer" <+> pretty n)
