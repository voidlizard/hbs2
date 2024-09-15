module HBS2.Peer.Proto.RefChan.Adapter where

import HBS2.Peer.Proto.RefChan.Types
import HBS2.Net.Proto.Notify
-- import HBS2.Peer.Notify
import HBS2.Data.Types.Refs

data RefChanAdapter e m =
  RefChanAdapter
  { refChanOnHead                 :: RefChanId e -> RefChanHeadBlockTran e -> m ()
  , refChanSubscribed             :: RefChanId e -> m Bool
  , refChanWriteTran              :: HashRef -> m ()
  , refChanValidatePropose        :: RefChanId e -> HashRef -> m Bool
  , refChanNotifyRely             :: RefChanId e -> RefChanNotify e -> m ()
  -- , refChanNotifySink             :: SomeNotifySource (RefChanEvents L4Proto)
  }


