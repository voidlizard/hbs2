module Main where

import HBS2.Prelude.Plated
import HBS2.System.Logger.Simple
import HBS2.Clock
import HBS2.Hash
import HBS2.Base58

import RaftAlgo.Proto

import Data.Ord
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Control.Monad.Reader
import System.Random
import Data.Hashable hiding (Hashed)
import Data.Word
import Lens.Micro.Platform
import Data.Fixed
import Options.Applicative as O
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Maybe
import Data.Graph
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet

import Codec.Serialise

import UnliftIO

