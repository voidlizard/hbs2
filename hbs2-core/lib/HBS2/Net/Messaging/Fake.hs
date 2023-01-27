module HBS2.Net.Messaging.Fake
  ( FakeP2P
  , newFakeP2P
  -- , Messaging(..)
  ) where

import HBS2.Net.Proto
import HBS2.Net.Messaging

import Control.Concurrent.STM (atomically) -- as STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan qualified as Chan
import Control.Concurrent.STM.TChan (TChan,newTChanIO)
import Control.Monad.IO.Class
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.List qualified as List
import Data.Maybe
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict as HashMap



data FakeP2P proto msg =
  FakeP2P
  {
    blocking :: Bool
  , fakeP2p  :: TVar (HashMap (Peer proto) (TChan (From proto,msg)))
  }

newFakeP2P :: (Eq (Peer peer), Hashable (Peer peer)) => Bool -> IO (FakeP2P peer msg)
newFakeP2P block =
  FakeP2P block <$> newTVarIO mempty

getChan bus whom = do
  ch <- newTChanIO
  atomically $ stateTVar t (alter ch)

  where
    t = fakeP2p bus
    alter ch x = case HashMap.lookup whom x of
                          Just c -> (c, x)
                          Nothing -> (ch, HashMap.insert whom ch x)

instance ( (HasPeer proto, Hashable (Peer proto))
         ) => Messaging (FakeP2P proto msg) proto msg where

  sendTo bus (To whom) who msg = liftIO do
    ch <- newTChanIO
    chan <- getChan bus whom
    atomically $ Chan.writeTChan chan (who, msg)

  receive bus (To me) = liftIO do
    readChan =<< getChan bus me

    where
      readChan | blocking bus = atomically . (List.singleton <$>) . Chan.readTChan
               | otherwise    = atomically . (maybeToList <$>) . Chan.tryReadTChan
