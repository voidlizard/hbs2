module HBS2.Net.Messaging.Fake
  ( FakeP2P
  , newFakeP2P
  -- , Messaging(..)
  ) where

import HBS2.Net.Proto
import HBS2.Net.Messaging

import Control.Concurrent.STM (atomically) -- as STM
import Control.Concurrent.STM.TChan qualified as Chan
import Control.Concurrent.STM.TChan (TChan,newTChanIO)
import Control.Monad.IO.Class
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.List qualified as List
import Data.Maybe
import Data.Hashable


data FakeP2P proto msg =
  FakeP2P
  {
    blocking :: Bool
  , fakeP2p  :: Cache (Peer proto) (TChan (From proto,msg))
  }

newFakeP2P :: Bool -> IO (FakeP2P peer msg)
newFakeP2P block =
  FakeP2P block <$> Cache.newCache Nothing

instance ( (HasPeer proto, Hashable (Peer proto))
         ) => Messaging (FakeP2P proto msg) proto msg where

  sendTo bus (To whom) who msg = liftIO do
    chan <- Cache.fetchWithCache (fakeP2p bus) whom $ const newTChanIO
    atomically $ Chan.writeTChan chan (who, msg)

  receive bus (To me) = liftIO do
    readChan =<< Cache.fetchWithCache (fakeP2p bus) me (const newTChanIO)

    where
      readChan | blocking bus = atomically . (List.singleton <$>) . Chan.readTChan
               | otherwise    = atomically . (maybeToList <$>) . Chan.tryReadTChan
