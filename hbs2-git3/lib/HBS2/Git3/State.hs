{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git3.State
  ( module HBS2.Git3.State
  , module Exported
  ) where

import HBS2.Git3.Prelude

import HBS2.Git3.State.Internal.Types as Exported
import HBS2.Git3.State.Internal.LWWBlock as Exported
import HBS2.Git3.State.Internal.RefLog as Exported
import HBS2.Git3.State.Internal.Segment as Exported
import HBS2.Git3.State.Internal.Index as Exported

import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.System.Dir
import HBS2.Peer.CLI.Detect

import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Kind

import Codec.Compression.Zstd (maxCLevel)

