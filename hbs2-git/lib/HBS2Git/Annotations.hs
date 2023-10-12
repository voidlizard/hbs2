module HBS2Git.Annotations where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs

import HBS2Git.Encryption

import Codec.Serialise

data Annotation =
  GK1 HashRef (GroupKey 'Symm HBS2Basic)
  deriving (Generic)

data Annotations =
    NoAnnotations
  | SmallAnnotations [Annotation]
  deriving (Generic)

instance Serialise Annotation
instance Serialise Annotations




