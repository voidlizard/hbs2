{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.RPC.Class where

import Data.Kind

class HasRpcContext (api :: [Type]) ctx m where
  -- type family RpcContext api  :: Type
  getRpcContext :: m ctx -- (RpcContext api)


