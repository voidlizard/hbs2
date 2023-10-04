module RPC2.API
  ( module RPC2.API
  , module RPC2.Poke
  , module RPC2.Ping
  , module RPC2.Peers
  , module RPC2.PexInfo
  , module RPC2.Announce
  , module RPC2.Fetch
  , module RPC2.Die
  , module RPC2.LogLevel
  , module RPC2.RefLog
  , module RPC2.RefChan
  , module RPC2.Types
  ) where

import RPC2.Announce
import RPC2.Die
import RPC2.Fetch
import RPC2.Poke
import RPC2.Ping
import RPC2.Peers
import RPC2.PexInfo
import RPC2.LogLevel
import RPC2.RefLog
import RPC2.RefChan
import RPC2.Types

type RPC2 = '[ RpcPoke
             , RpcPing
             , RpcAnnounce
             , RpcFetch
             , RpcPeers
             , RpcPexInfo
             , RpcRefLogGet
             , RpcRefLogFetch
             , RpcRefLogPost
             , RpcRefChanHeadGet
             , RpcRefChanHeadFetch
             , RpcRefChanHeadPost
             , RpcRefChanGet
             , RpcRefChanFetch
             , RpcRefChanPropose
             , RpcRefChanNotify
             , RpcLogLevel
             , RpcDie
             ]

