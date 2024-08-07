module Fixme.Log where

import Fixme.Prelude
import Fixme.Types

import HBS2.Storage.Compact

import Data.Config.Suckless

import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
import Data.Either

{- HLINT ignore "Functor law"-}

loadAllEntriesFromLog :: FixmePerks m
                      => CompactStorage HbSync
                      -> FixmeM m [Syntax C]
loadAllEntriesFromLog sto = do
    ks <- keys sto

    entries <- mapM (get sto) ks
                <&> catMaybes
                <&> fmap (deserialiseOrFail @CompactAction . LBS.fromStrict)
                <&> rights

    let top = show $ vcat (fmap pretty entries)
    let theLog = parseTop top & fromRight mempty

    pure theLog

