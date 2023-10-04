module HBS2.Peer.RPC.Class where

class HasRpcContext a m where
  getRpcContext :: m a


