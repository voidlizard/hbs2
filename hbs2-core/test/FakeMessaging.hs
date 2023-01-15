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

instance IsPeer Fake where
  newtype instance Peer Fake = FakePeer Int
                               deriving stock (Eq,Ord,Show)
                               deriving newtype (Hashable,Num,Enum,Real,Integral)

instance Pretty (Peer Fake) where
  pretty (FakePeer n) = parens ("peer" <+> pretty n)
