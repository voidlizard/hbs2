{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2.Git.Data.LWWBlock
  ( module HBS2.Git.Data.LWWBlock
  , module HBS2.Peer.Proto.LWWRef
  ) where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Net.Proto.Types
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Net.Auth.Schema()
import HBS2.Net.Auth.Credentials
import HBS2.Storage
import HBS2.Peer.Proto.LWWRef

import Data.Word
import Codec.Serialise
import System.Random

import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans.Maybe

-- NOTE: on-lww-block-data
--   HKDF ( SK(LWWRef) , lwwRefNonce ) ==> SK( RefLog )
--   lwwRefLogPubKey ==  PK ( SK (RefLog ) )
--
--   LWWBlock is required to make repo reference "stable",
--   i.e. it should remains the same even if the structure
--   of repository has been changed or it was, say, "trimmed".
--
--   Therefore, there is the root key and the LWWRef, pointing
--   to a block, which contains actual seed data for the "current"
--   repo and it's possible to support permanent reference (LWWRef)
--   to a repo, while it's actual structure may be changed
--   (hbs2-git repo structure changes or garbage collecting (removing old
--   transactions, etc).
--
--   (LWWRef PK) -> (LWWBlockData) -> (RefLog : [TX])
--

data LWWBlockData s =
  LWWBlockData
  { lwwRefSeed       :: Word64
  , lwwRefLogPubKey  :: PubKey 'Sign s
  }
  deriving stock Generic

data LWWBlock s =
  LWWBlock1 { lwwBlockData :: LWWBlockData s }
  deriving stock Generic

instance Serialise (PubKey 'Sign s) => Serialise (LWWBlockData s)
instance Serialise (PubKey 'Sign s) => Serialise (LWWBlock s)


data LWWBlockOpError =
    LWWBlockOpSkNotAvail
  | LWWBlockOpStorageError
  | LWWBlockOpSomeError
  deriving stock (Show,Typeable,Generic)

instance Exception LWWBlockOpError

{- HLINT ignore "Functor law" -}

readLWWBlock :: forall  s m . ( MonadIO m
                              , Signatures s
                              , ForLWWRefProto s
                              , IsRefPubKey s
                              )
             => AnyStorage
             -> LWWRefKey s
             -> m (Maybe (LWWRef s, LWWBlockData s))

readLWWBlock sto k = runMaybeT do

  w@LWWRef{..} <- runExceptT (readLWWRef @s sto k)
                  >>= toMPlus
                  >>= toMPlus

  getBlock sto (fromHashRef lwwValue)
    >>= toMPlus
    <&> deserialiseOrFail @(LWWBlock s)
    >>= toMPlus
    <&> lwwBlockData
    <&> (w,)

initLWWRef :: forall  s m . ( MonadIO m
                             , MonadError LWWBlockOpError m
                             , IsRefPubKey s
                             , ForSignedBox s
                             , HasDerivedKey s 'Sign Word64 m
                             , Signatures s
                             )
           => AnyStorage
           -> Maybe Word64
           -> ( PubKey 'Sign s -> m (Maybe (PrivKey 'Sign s) ) )
           -> LWWRefKey s
           -> m HashRef
initLWWRef sto seed' findSk lwwKey  = do
  -- let k0 = fromLwwRefKey lww
  seed   <- maybe1 seed' randomIO pure

  let pk0 = fromLwwRefKey lwwKey
  sk0 <- findSk pk0
           >>= orThrowError LWWBlockOpSkNotAvail

  lww0 <- runMaybeT do
            getRef sto lwwKey >>= toMPlus
              >>= getBlock sto >>= toMPlus
              <&> deserialiseOrFail @(SignedBox (LWWRef s) s)
              >>= toMPlus
              <&> unboxSignedBox0
              >>= toMPlus
              <&> snd

  (pk1, _) <- derivedKey @s @'Sign seed sk0

  let newLwwData = LWWBlock1 @s (LWWBlockData seed pk1)

  hx <- putBlock sto (serialise newLwwData)
          >>= orThrowError LWWBlockOpStorageError
          <&> HashRef

  let lww :: LWWRef e
      lww = LWWRef { lwwSeq = succ (maybe 0 lwwSeq lww0)
                   , lwwValue = hx
                   , lwwProof = Nothing
                   }

  updateLWWRef @s sto lwwKey sk0 lww
    >>= orThrowError LWWBlockOpStorageError


