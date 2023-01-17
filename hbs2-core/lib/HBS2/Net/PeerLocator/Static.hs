module HBS2.Net.PeerLocator.Static where

import HBS2.Prelude
import HBS2.Net.Proto
import HBS2.Net.PeerLocator

import Control.Concurrent.STM.TVar
import Data.Set (Set)
import Data.Set qualified as Set

newtype StaticPeerLocator p =
  StaticPeerLocator (TVar (Set (Peer p)))


newStaticPeerLocator :: (Ord (Peer p), HasPeer p, MonadIO m) => [Peer p] -> m (StaticPeerLocator p)
newStaticPeerLocator seeds = do
  tv <- liftIO $ newTVarIO (Set.fromList seeds)
  pure $ StaticPeerLocator tv

instance PeerLocator  (StaticPeerLocator p)  where

  knownPeers _ = pure mempty

