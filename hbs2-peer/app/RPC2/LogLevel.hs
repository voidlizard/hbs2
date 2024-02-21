{-# OPTIONS_GHC -fno-warn-orphans #-}
module RPC2.LogLevel where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Service

import Log

import HBS2.Peer.RPC.API.Peer


instance (MonadIO m) => HandleMethod m RpcLogLevel where

  handleMethod = \case
    DebugOn True  -> do
      setLogging @DEBUG debugPrefix
      debug "DebugOn"

    DebugOn False -> do
      debug "DebugOff"
      setLoggingOff @DEBUG

    TraceOn True -> do
      setLogging @TRACE tracePrefix
      trace "TraceOn"

    TraceOn False -> do
      trace "TraceOff"
      setLoggingOff @TRACE



