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

import Data.Config.Suckless

-- import HBS2.System.Logger.Simple

-- import Data.Maybe
-- import Data.Hashable
import Data.Text qualified as Text
import Data.ByteString (ByteString)
-- import Type.Reflection (someTypeRep)
import Data.Either
import Data.Maybe
import Lens.Micro.Platform
import Codec.Serialise

{- HLINT ignore "Use newtype instead of data" -}

type RefChanId e = PubKey 'Sign (Encryption e)
type RefChanOwner e = PubKey 'Sign (Encryption e)
type RefChanAuthor e = PubKey 'Sign (Encryption e)

data SignedBox p e =
  SignedBox (PubKey 'Sign e) ByteString (Signature (Encryption e))
  deriving stock (Generic)

type Weight = Integer

data RefChanHeadBlock e =
  RefChanHeadBlockSmall
  { _refChanHeadVersion     :: Integer
  , _refChanHeadQuorum      :: Integer
  , _refChanHeadWaitAccept  :: Integer
  , _refChanHeadPeers       :: [(PubKey 'Sign (Encryption e),Weight)]
  , _refChanHeadAuthors     :: [PubKey 'Sign (Encryption e)]
  }
  deriving stock (Generic)

makeLenses 'RefChanHeadBlockSmall

type ForRefChans e = ( Serialise ( PubKey 'Sign (Encryption e))
                     , Pretty (AsBase58 (PubKey 'Sign (Encryption e)))
                     , FromStringMaybe (PubKey 'Sign (Encryption e))
                     )

-- блок головы может быть довольно большой.
-- поэтому посылаем его, как merkle tree
newtype RefChanHeadBlockTran e =
  RefChanHeadBlockTran HashRef
  deriving stock (Generic)

instance Serialise (RefChanHeadBlockTran e)


data RefChanHead e =
    RefChanHead (RefChanId e) (RefChanHeadBlockTran e)
  | RefChanGetHead (RefChanId e)
  deriving stock (Generic)

instance ForRefChans e => Serialise (RefChanHead e)

data RefChanHeadAdapter e m =
  RefChanHeadAdapter
  { _refChanHeadOnHead :: RefChanHeadBlockTran e -> m ()
  }

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
                  => RefChanHeadAdapter e m
                  -> RefChanHead e
                  -> m ()

refChanHeadProto adapter msg = do
  -- авторизовать пира

  case msg of
    RefChanHead pkt _ -> do
      pure ()

    RefChanGetHead _ -> do
      -- прочитать ссылку
      -- послать хэш головы
      pure ()


instance ForRefChans e => FromStringMaybe (RefChanHeadBlock e) where
  fromStringMay str = RefChanHeadBlockSmall <$> version
                                          <*> quorum
                                          <*> wait
                                          <*> pure peers
                                          <*> pure authors
    where
      parsed = parseTop str & fromRight mempty
      version = lastMay [ n | (ListVal [SymbolVal "version", LitIntVal n] ) <- parsed ]
      quorum  = lastMay [ n | (ListVal [SymbolVal "quorum", LitIntVal n] ) <- parsed ]
      wait    = lastMay [ n | (ListVal [SymbolVal "wait", LitIntVal n] ) <- parsed ]
      peers   = catMaybes [ (,) <$> fromStringMay (Text.unpack s) <*> pure w | (ListVal [SymbolVal "peer", LitStrVal s, LitIntVal w] ) <- parsed  ]
      authors = catMaybes [ fromStringMay (Text.unpack s) | (ListVal [SymbolVal "author", LitStrVal s] ) <- parsed  ]

instance ForRefChans e => Pretty (RefChanHeadBlock e) where
  pretty blk = parens ("version" <+> pretty (view refChanHeadVersion blk)) <> line
               <>
               parens ("quorum" <+> pretty (view refChanHeadQuorum blk)) <> line
               <>
               parens ("wait" <+> pretty (view refChanHeadWaitAccept blk)) <> line
               <>
               vcat (fmap peer (view refChanHeadPeers blk)) <> line
               <>
               vcat (fmap author (view refChanHeadAuthors blk)) <> line

    where
      peer (p,w) = parens ("peer" <+> dquotes (pretty (AsBase58 p)) <+> pretty w)
      author p   = parens ("author" <+> dquotes (pretty (AsBase58 p)))


