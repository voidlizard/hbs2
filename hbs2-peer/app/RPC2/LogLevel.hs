module RPC2.LogLevel where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Service

import Log

import HBS2.Peer.RPC.Internal.Types
import RPC2.Peer.API

import HBS2.System.Logger.Simple
import Codec.Serialise

data SetLogging =
    DebugOn Bool
  | TraceOn Bool
  deriving (Generic,Eq,Show)

instance Serialise SetLogging


instance (MonadIO m) => HandleMethod m RpcLogLevel where
  type instance Input RpcLogLevel = SetLogging
  type instance Output RpcLogLevel = ()

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



