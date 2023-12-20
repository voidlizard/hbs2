
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2.Peer.RefChanNotifyLog where

import HBS2.Data.Types.Refs
import HBS2.Net.Proto.Definition()
import HBS2.Net.Proto.RefChan


type RefChanNotifyLogKey e = SomeRefKey (String, RefChanId e)


makeRefChanNotifyLogKey :: RefChanId e -> RefChanNotifyLogKey e
makeRefChanNotifyLogKey rc = SomeRefKey ("refchan-notify-log-key", rc)


