{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}
module HBS2.Net.Proto.RefChan where

import HBS2.Prelude.Plated
-- import HBS2.Hash
-- import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Net.Auth.Credentials
import HBS2.Base58
-- import HBS2.Events
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Data.Types.Refs

-- import HBS2.System.Logger.Simple

-- import Data.Maybe
-- import Data.Hashable
import Data.ByteString (ByteString)
-- import Type.Reflection (someTypeRep)
import Lens.Micro.Platform
-- import Codec.Serialise

{- HLINT ignore "Use newtype instead of data" -}

data SignedBox p e =
  SignedBox (PubKey 'Sign e) ByteString (Signature (Encryption e))
  deriving stock (Generic)

data RefChanHeadBlock e =
  RefChanHeadBlock
  { _refChanHeadVersion     :: Integer
  , _refChanHeadQuorum      :: Integer
  , _refChanHeadWaitAccept  :: Integer
  , _refChanHeadPeers       :: [PubKey 'Sign e]
  , _refChanHeadAuthors     :: [PubKey 'Sign e]
  }
  deriving stock (Generic)

makeLenses 'RefChanHeadBlock

newtype RefChanHeadBlockTran e =
  RefChanHeadBlockTran HashRef
  deriving stock (Generic)

type RefChanId e = PubKey 'Sign e

data RefChanHead e =
    RefChanHead (RefChanId e) (RefChanHeadBlockTran e)
  | RefChanGetHead (RefChanId e)
  deriving stock (Generic)



refChanHeadProto :: forall e s m . ( MonadIO m
                                    , Request e (RefChanHead e) m
                                    , Response e (RefChanHead e) m
                                    , IsPeerAddr e m
                                    , Pretty (Peer e)
                                    , Sessions e (KnownPeer e) m
                                    , Signatures s
                                    , Pretty (AsBase58 (PubKey 'Sign s))
                                    , s ~ Encryption e
                                    )
                  => RefChanHead e -> m ()

refChanHeadProto _ = pure ()


-- type RefChanAuthor e = PubKey 'Sign (Encryption e)
-- type RefChan e = PubKey 'Sign (Encryption e)

-- type ForRefChan e = ( Serialise (RefChan e)
--                     , Serialise (Signature (Encryption e))
--                     )

-- data RefChanACL e =
--   RefChanACLImmediate [RefChanAuthor e] -- ^ authorized authors
--   deriving stock (Generic)

-- instance ForRefChan e => Serialise (RefChanACL e)

-- data RefChanHeadBlock e =
--   RefChanHeadBlock
--   { _refChanHeadPrev    :: Maybe HashRef
--   , _refChanHeadVersion :: Integer
--   , _refChanHeadHistory :: Maybe HashRef
--   , _refChanHeadACL     :: RefChanACL e
--   }
--   deriving stock (Generic)

-- makeLenses ''RefChanHeadBlock

-- instance ForRefChan e => Serialise (RefChanHeadBlock e)

-- data RefChanMsgEnvelope e =
--   RefChanMessage
--   { _refChanMsgChan    :: RefChan e
--   , _refChanMsgAuthor  :: RefChanAuthor e
--   , _refChanMsgData    :: ByteString
--   , _refChanMsgSign    :: Signature (Encryption e)
--   }
--   deriving stock (Generic)

-- makeLenses ''RefChanMsgEnvelope

-- instance ForRefChan e => Serialise (RefChanMsgEnvelope e)

-- newtype RefChanHeadMsg e =
--   RefChanHeadMsg HashRef
--   deriving stock Generic

-- instance Serialise (RefChanHeadMsg e)

-- data RefChanHead e =
--     RefChanGetHead (RefChanMsgEnvelope e)
--   | RefChanHead    (RefChanMsgEnvelope e)
--   deriving stock (Generic)

-- instance ForRefChan e => Serialise (RefChanHead e)













