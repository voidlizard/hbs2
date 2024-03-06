module HBS2Git.Annotations where

import HBS2Git.Prelude
import HBS2Git.Encryption

data Annotation =
  GK1 HashRef (GroupKey 'Symm HBS2Basic)
  deriving (Generic)

data Annotations =
    NoAnnotations
  | SmallAnnotations [Annotation]
  deriving (Generic)

instance Serialise Annotation
instance Serialise Annotations




