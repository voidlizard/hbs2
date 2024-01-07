{-# Language TemplateHaskell #-}
module HBS2.Share.MetaData where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs

import HBS2.Share.LocalHash

import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict (HashMap)
import Codec.Serialise
import System.FilePath
import Data.List qualified as List
import Data.Maybe
import Data.Text qualified as Text
import Lens.Micro.Platform

newtype PathEntry = PathEntry Text
                    deriving stock (Eq,Ord,Data,Generic,Show)
                    deriving newtype (Hashable,Pretty)

newtype EntryKey = EntryKey { entryKey :: [PathEntry] }
                   deriving stock (Eq,Ord,Data,Generic,Show)
                   deriving newtype (Hashable,Semigroup,Monoid)


data FileEntry =
  FileEntry
  { _feKey       :: EntryKey
  , _feLocalHash :: LocalHash
  , _feTree      :: HashRef
  }
  deriving stock (Show,Data,Generic)

makeLenses ''FileEntry

instance IsString EntryKey where
  fromString p = EntryKey [ PathEntry (fromString s) | s <- splitDirectories p ]

instance Pretty EntryKey where
  pretty (EntryKey ps) = pretty $ joinPath [ Text.unpack p | PathEntry p <- ps ]


toFilePath :: EntryKey -> FilePath
toFilePath = show . pretty

data MetaData =
  MetaData
  { mdBase  :: Maybe HashRef    -- ^ reference to state TX
  , mdGK1   :: HashMap HashRef  HashRef
  , mdFiles :: [FileEntry]
  }
  deriving stock (Show,Generic)

instance Serialise PathEntry
instance Serialise EntryKey
instance Serialise FileEntry
instance Serialise MetaData


makeEntryKey :: EntryKey -> FilePath -> EntryKey
makeEntryKey (EntryKey prefix) path = EntryKey pnew
  where
    pp = entryKey $ fromString path
    pnew  = List.stripPrefix prefix pp & fromMaybe pp

getDirs :: EntryKey -> [FilePath]
getDirs ek = fmap (joinPath . fmap unPathEntry) $ init $ tailSafe $ List.inits $ entryKey ek
  where
    unPathEntry (PathEntry p) = Text.unpack p

