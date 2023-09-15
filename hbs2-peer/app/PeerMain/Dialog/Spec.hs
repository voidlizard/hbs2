{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}

module PeerMain.Dialog.Spec where

-- import Codec.Serialise
-- import Streaming
import Data.Text (Text)
import GHC.Generics (Generic)

import HBS2.Hash
import HBS2.Net.Dialog.Core
import HBS2.Net.Proto.Types


data DialogRPCSpec r = DialogRPCSpec
  { drpcPing :: r &- "ping" &/ SingleRespCBOR Text
  , drpcSpec :: r &- "spec" &/ SingleRespCBOR Text
  , drpcReflog :: r &- "reflog" &// RPCReflogSpec
  }
  deriving (Generic)

data RPCReflogSpec r = RPCReflogSpec
  { reflogGet :: r &- "get"
        &/ ReqCBOR (PubKey 'Sign (Encryption L4Proto))
        &/ SingleRespCBOR (Hash HbSync)

  , reflogFetch :: r &- "fetch"
        &/ ReqCBOR (PubKey 'Sign (Encryption L4Proto))
        &/ SingleAck

  }
  deriving (Generic)

