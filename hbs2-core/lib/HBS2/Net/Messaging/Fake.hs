module HBS2.Net.Messaging.Fake
  ( FakeP2P
  , newFakeP2P
  , Messaging(..)
  ) where

import Control.Concurrent.STM (atomically) -- as STM
import Control.Concurrent.STM.TChan qualified as Chan
import Control.Concurrent.STM.TChan (TChan,newTChanIO)
import Control.Monad.IO.Class
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.List qualified as List
import Data.Maybe

import HBS2.Net.Proto
import HBS2.Net.Messaging

data FakeP2P peer msg =
  FakeP2P
  {
    blocking :: Bool
  , fakeP2p  :: Cache peer (TChan msg)
  }

newFakeP2P :: Bool -> IO (FakeP2P peer msg)
newFakeP2P block = FakeP2P block <$> Cache.newCache Nothing

instance ( IsPeer peer
         ) => Messaging (FakeP2P peer msg) peer msg where

  sendTo bus (To whom) _ msg = liftIO do
    chan <- Cache.fetchWithCache (fakeP2p bus) whom $ const newTChanIO
    atomically $ Chan.writeTChan chan msg

  -- NOTE: non-blocking version!
  receive bus (To me) = liftIO do
    Cache.fetchWithCache (fakeP2p bus)
                         me
                         (const newTChanIO)

                         >>= readChan

    where
      readChan | blocking bus = atomically . (List.singleton <$>) . Chan.readTChan
               | otherwise    = atomically . (maybeToList <$>) . Chan.tryReadTChan

