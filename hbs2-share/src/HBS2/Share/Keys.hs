module HBS2.Share.Keys where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Net.Proto.Types

type GK0 s = GroupKey 'Symm s

newtype GK0Key = GK0Key HashRef
                 deriving stock (Generic,Data)
                 deriving newtype (Pretty, Hashed HbSync)


