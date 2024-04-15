{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}
{-# Language FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module HBS2.Peer.Proto.RefChan.Types where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Data.Detect
import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Net.Auth.Credentials
import HBS2.Base58
import HBS2.Defaults
import HBS2.Events
import HBS2.Peer.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Storage

import Data.Config.Suckless

import HBS2.System.Logger.Simple

import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Either
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe
import Data.Either
import Data.Text qualified as Text
import Lens.Micro.Platform
import Data.Hashable hiding (Hashed)
import Data.Coerce
import Data.List qualified as List
import Codec.Serialise

{- HLINT ignore "Use newtype instead of data" -}

type RefChanId e = PubKey 'Sign (Encryption e)
type RefChanOwner e = PubKey 'Sign (Encryption e)
type RefChanAuthor e = PubKey 'Sign (Encryption e)

type Weight = Integer

data ACLType = ACLUpdate | ACLNotify
              deriving stock (Eq,Ord,Generic,Data,Show)

data RefChanHeadBlock e =
  RefChanHeadBlockSmall
   { _refChanHeadVersion     :: Integer
   , _refChanHeadQuorum      :: Integer
   , _refChanHeadWaitAccept  :: Integer
   , _refChanHeadPeers       :: HashMap (PubKey 'Sign (Encryption e)) Weight
   , _refChanHeadAuthors     :: HashSet (PubKey 'Sign (Encryption e))
   }
  | RefChanHeadBlock1
  {  _refChanHeadVersion     :: Integer
   , _refChanHeadQuorum      :: Integer
   , _refChanHeadWaitAccept  :: Integer
   , _refChanHeadPeers       :: HashMap (PubKey 'Sign (Encryption e)) Weight
   , _refChanHeadAuthors     :: HashSet (PubKey 'Sign (Encryption e))
   , _refChanHeadReaders'    :: HashSet (PubKey 'Encrypt (Encryption e))
   , _refChanHeadExt         :: ByteString
  }
  | RefChanHeadBlock2
  {  _refChanHeadVersion     :: Integer
   , _refChanHeadQuorum      :: Integer
   , _refChanHeadWaitAccept  :: Integer
   , _refChanHeadPeers       :: HashMap (PubKey 'Sign (Encryption e)) Weight
   , _refChanHeadAuthors     :: HashSet (PubKey 'Sign (Encryption e))
   , _refChanHeadReaders'    :: HashSet (PubKey 'Encrypt (Encryption e))
   , _refChanHeadNotifiers'  :: HashSet (PubKey 'Sign (Encryption e))
   , _refChanHeadExt         :: ByteString
  }
  deriving stock (Generic)

makeLenses ''RefChanHeadBlock

data RefChanActionRequest =
    RefChanAnnounceBlock HashRef
  | RefChanFetch HashRef
  deriving stock (Generic)

instance Serialise RefChanActionRequest

type DisclosedCredentials e = PeerCredentials (Encryption e)

newtype RefChanHeadExt e =
  RefChanHeadExt [LBS.ByteString]
  deriving stock Generic
  deriving newtype (Semigroup, Monoid)

data  RefChanDisclosedCredentials e =
  RefChanDisclosedCredentials (TaggedHashRef (DisclosedCredentials e))
  deriving stock (Eq,Generic)

instance Pretty (AsBase58 (RefChanDisclosedCredentials e)) where
  pretty (AsBase58 (RefChanDisclosedCredentials x)) = pretty x

instance Pretty (RefChanDisclosedCredentials e) where
  pretty (RefChanDisclosedCredentials x) = pretty x

instance Serialise (RefChanHeadExt e)

instance SerialisedCredentials (Encryption e) => Serialise (RefChanDisclosedCredentials e)

data RefChanNotify e =
    Notify (RefChanId e) (SignedBox ByteString (Encryption e)) -- подписано ключом автора
  -- довольно уместно будет добавить эти команды сюда -
  -- они постоянно нужны, и это сильно упростит коммуникации
  | ActionRequest (RefChanId e) RefChanActionRequest

  deriving stock (Generic)

instance ForRefChans e => Serialise (RefChanNotify e)


newtype instance EventKey e (RefChanNotify e) =
  RefChanNotifyEventKey (RefChanId e)

deriving stock instance ForRefChans e => Typeable (EventKey e (RefChanNotify e))
deriving stock instance ForRefChans e => Eq  (EventKey e (RefChanNotify e))
deriving newtype instance ForRefChans e => Hashable  (EventKey e (RefChanNotify e))

data instance Event e (RefChanNotify e) =
  RefChanNotifyEvent HashRef (RefChanNotify e)

-- FIXME: remove-default-instance?
instance EventType (Event e (RefChanNotify e)) where
  isPersistent = True

instance Expires (EventKey e (RefChanNotify e)) where
  expiresIn = const Nothing -- (Just defCookieTimeoutSec)



type ForRefChans e = ( Serialise (PubKey 'Sign (Encryption e))
                     , Serialise (PrivKey 'Sign (Encryption e))
                     , Pretty (AsBase58 (PubKey 'Sign (Encryption e)))
                     , FromStringMaybe (PubKey 'Sign (Encryption e))
                     , FromStringMaybe (PubKey 'Encrypt (Encryption e))
                     , Signatures (Encryption e)
                     , Serialise (Signature (Encryption e))
                     , Serialise (PubKey 'Encrypt (Encryption e))
                     , Hashable (PubKey 'Encrypt (Encryption e))
                     , Hashable (PubKey 'Sign (Encryption e))
                     )


refChanHeadReaders :: ForRefChans e
                    => Lens (RefChanHeadBlock e)
                            (RefChanHeadBlock e)
                            (HashSet (PubKey 'Encrypt (Encryption e)))
                            (HashSet (PubKey 'Encrypt (Encryption e)))

refChanHeadReaders = lens g s
  where
    g (RefChanHeadBlockSmall{}) = mempty
    g (RefChanHeadBlock1{..})   = _refChanHeadReaders'
    g (RefChanHeadBlock2{..})   = _refChanHeadReaders'
    s v@(RefChanHeadBlock1{}) x = v { _refChanHeadReaders' = x }
    s v@(RefChanHeadBlock2{}) x = v { _refChanHeadReaders' = x }
    s x _ = x


refChanHeadNotifiers :: ForRefChans e
                    => Lens (RefChanHeadBlock e)
                            (RefChanHeadBlock e)
                            (HashSet (PubKey 'Sign (Encryption e)))
                            (HashSet (PubKey 'Sign (Encryption e)))

refChanHeadNotifiers = lens g s
  where
    g (RefChanHeadBlockSmall{}) = mempty
    g (RefChanHeadBlock1{})     = mempty
    g (RefChanHeadBlock2{..})   = _refChanHeadNotifiers'

    s v@(RefChanHeadBlock2{}) x = v { _refChanHeadNotifiers' = x }
    s x _ = x

instance ForRefChans e => Serialise (RefChanHeadBlock e)

type instance SessionData e (RefChanHeadBlock e) = RefChanHeadBlock e

newtype instance SessionKey e  (RefChanHeadBlock e) =
  RefChanHeadBlockKey (RefChanHeadKey (Encryption e))

deriving newtype instance ForRefChans L4Proto
  => Hashable (SessionKey L4Proto (RefChanHeadBlock L4Proto))

deriving stock instance ForRefChans L4Proto
  => Eq (SessionKey L4Proto (RefChanHeadBlock L4Proto))

-- TODO: define-expiration-time
instance Expires (SessionKey L4Proto (RefChanHeadBlock L4Proto)) where
  expiresIn = const (Just defCookieTimeoutSec)

newtype RefChanHeadKey s = RefChanHeadKey (PubKey 'Sign s)

instance RefMetaData (RefChanHeadKey s)

deriving stock instance IsRefPubKey s => Eq (RefChanHeadKey s)

instance IsRefPubKey s => Hashable (RefChanHeadKey s) where
  hashWithSalt s k = hashWithSalt s (hashObject @HbSync k)

instance IsRefPubKey s => Hashed HbSync (RefChanHeadKey s) where
  hashObject (RefChanHeadKey pk) = hashObject ("refchanhead|" <> serialise pk)

instance IsRefPubKey s => FromStringMaybe (RefChanHeadKey s) where
  fromStringMay s = RefChanHeadKey <$> fromStringMay s

instance IsRefPubKey s =>  IsString (RefChanHeadKey s) where
  fromString s = fromMaybe (error "bad public key base58") (fromStringMay s)

instance Pretty (AsBase58 (PubKey 'Sign s )) => Pretty (AsBase58 (RefChanHeadKey s)) where
  pretty (AsBase58 (RefChanHeadKey k)) = pretty (AsBase58 k)

instance Pretty (AsBase58 (PubKey 'Sign s )) => Pretty (RefChanHeadKey s) where
  pretty (RefChanHeadKey k) = pretty (AsBase58 k)


newtype RefChanLogKey s = RefChanLogKey { fromRefChanLogKey :: PubKey 'Sign s }

instance RefMetaData (RefChanLogKey s)

deriving stock instance IsRefPubKey s => Eq (RefChanLogKey s)

instance IsRefPubKey s => Hashable (RefChanLogKey s) where
  hashWithSalt s k = hashWithSalt s (hashObject @HbSync k)

instance IsRefPubKey s => Hashed HbSync (RefChanLogKey s) where
  hashObject (RefChanLogKey pk) = hashObject ("refchanlog|" <> serialise pk)

instance IsRefPubKey s => FromStringMaybe (RefChanLogKey s) where
  fromStringMay s = RefChanLogKey <$> fromStringMay s

instance IsRefPubKey s =>  IsString (RefChanLogKey s) where
  fromString s = fromMaybe (error "bad public key base58") (fromStringMay s)

instance Pretty (AsBase58 (PubKey 'Sign s )) => Pretty (AsBase58 (RefChanLogKey s)) where
  pretty (AsBase58 (RefChanLogKey k)) = pretty (AsBase58 k)

instance Pretty (AsBase58 (PubKey 'Sign s )) => Pretty (RefChanLogKey s) where
  pretty (RefChanLogKey k) = pretty (AsBase58 k)

instance ForRefChans e => FromStringMaybe (RefChanHeadBlock e) where

  -- NOTE: we-dont-support-old-head-formats-anymore
  fromStringMay str =
    RefChanHeadBlock2 <$> version
                      <*> quorum
                      <*> wait
                      <*> pure (HashMap.fromList peers)
                      <*> pure (HashSet.fromList authors)
                      <*> pure (HashSet.fromList readers)
                      <*> pure (HashSet.fromList notifiers)
                      <*> pure (LBS.toStrict ext)

    where
      parsed = parseTop str & fromRight mempty
      version = lastMay [ n | (ListVal [SymbolVal "version", LitIntVal n] ) <- parsed ]
      quorum  = lastMay [ n | (ListVal [SymbolVal "quorum", LitIntVal n] ) <- parsed ]
      wait    = lastMay [ n | (ListVal [SymbolVal "wait", LitIntVal n] ) <- parsed ]

      peers   = catMaybes [ (,) <$> fromStringMay (Text.unpack s) <*> pure w
                          | (ListVal [SymbolVal "peer", LitStrVal s, LitIntVal w] ) <- parsed
                          ]

      authors = catMaybes [ fromStringMay (Text.unpack s)
                          | (ListVal [SymbolVal "author", LitStrVal s] ) <- parsed
                          ]

      readers = catMaybes [ fromStringMay @(PubKey 'Encrypt  (Encryption e)) (Text.unpack s)
                          | (ListVal [SymbolVal "reader", LitStrVal s] ) <- parsed
                          ]


      notifiers = catMaybes [ fromStringMay @(PubKey 'Sign (Encryption e)) (Text.unpack s)
                            | (ListVal [SymbolVal "notifier", LitStrVal s] ) <- parsed
                            ]


      disclosed = catMaybes [ fromStringMay @HashRef (Text.unpack s)
                            | (ListVal [SymbolVal "disclosed", LitStrVal s] ) <- parsed
                            ]

      ext1 = fmap serialise [ RefChanDisclosedCredentials @L4Proto (coerce c) | c <- disclosed ]
      ext  = RefChanHeadExt ext1 & serialise

instance (ForRefChans e
         , Pretty (AsBase58 (PubKey 'Sign (Encryption e)))
         , Pretty (AsBase58 (PubKey 'Encrypt (Encryption e)))
         ) => Pretty (RefChanHeadBlock e) where
  pretty blk = parens ("version" <+> pretty (view refChanHeadVersion blk)) <> line
               <>
               parens ("quorum" <+> pretty (view refChanHeadQuorum blk)) <> line
               <>
               parens ("wait" <+> pretty (view refChanHeadWaitAccept blk)) <> line
               <>
               lstOf peer   (HashMap.toList $ view refChanHeadPeers blk)
               <>
               lstOf author (HashSet.toList $ view refChanHeadAuthors blk)
               <>
               lstOf reader (HashSet.toList $ view refChanHeadReaders blk)
               <>
               lstOf notifier (HashSet.toList $ view refChanHeadNotifiers blk)
               <>
               lstOf disclosed_ disclosed
               <> semi <+> parens ("head-extensions:"
                                      <+> parens ("count:" <+> pretty (length exs))
                                      <+> parens ("size"   <+> pretty (LBS.length extLbs))
                                  )

    where

      extLbs  = LBS.fromStrict $ view refChanHeadExt blk

      RefChanHeadExt exs = deserialiseOrFail @(RefChanHeadExt L4Proto) extLbs
                                & fromRight mempty

      disclosed = [ deserialiseOrFail @(RefChanDisclosedCredentials L4Proto) s
                  | s <- exs
                  ] & rights

      peer (p,w) = parens ("peer" <+> dquotes (pretty (AsBase58 p)) <+> pretty w)
      author p   = parens ("author" <+> dquotes (pretty (AsBase58 p)))
      reader p   = parens ("reader" <+> dquotes (pretty (AsBase58 p)))
      notifier p = parens ("notifier" <+> dquotes (pretty (AsBase58 p)))
      disclosed_ p = parens ("disclosed" <+> dquotes (pretty (AsBase58 p)))

      -- disclosed p =

      lstOf f e | null e = mempty
                | otherwise = vcat (fmap f e) <> line


-- блок головы может быть довольно большой.
-- поэтому посылаем его, как merkle tree
newtype RefChanHeadBlockTran e =
  RefChanHeadBlockTran { fromRefChanHeadBlockTran :: HashRef }
  deriving stock (Generic)

instance Serialise (RefChanHeadBlockTran e)

data RefChanHead e =
    RefChanHead (RefChanId e) (RefChanHeadBlockTran e)
  | RefChanGetHead (RefChanId e)
  deriving stock (Generic)

instance ForRefChans e => Serialise (RefChanHead e)

-- FIXME: rename
data RefChanAdapter e m =
  RefChanAdapter
  { refChanOnHead     :: RefChanId e -> RefChanHeadBlockTran e -> m ()
  , refChanSubscribed :: RefChanId e -> m Bool
  , refChanWriteTran  :: HashRef -> m ()
  , refChanValidatePropose :: RefChanId e -> HashRef -> m Bool
  , refChanNotifyRely :: RefChanId e -> RefChanNotify e -> m ()
  }

class HasRefChanId e p | p -> e where
  getRefChanId :: p -> RefChanId e

instance HasRefChanId e (RefChanNotify e) where
  getRefChanId = \case
    Notify c _ -> c
    ActionRequest c _ -> c


getActualRefChanHead :: forall e s m . ( MonadIO m
                                       , Sessions e (RefChanHeadBlock e) m
                                       , HasStorage m
                                       , Signatures s
                                       , IsRefPubKey s
                                       -- , Pretty (AsBase58 (PubKey 'Sign s))
                                     -- , Serialise (Signature s)
                                       , ForRefChans e
                                       , HasStorage m
                                       , s ~ Encryption e
                                       )
                   => RefChanHeadKey s
                   -> m (Maybe (RefChanHeadBlock e))

getActualRefChanHead key = do
  sto <- getStorage

  runMaybeT do
    mbHead <- do
      lift $ find @e (RefChanHeadBlockKey key) id

    case mbHead of
      Just hd -> do
        debug "HEAD DISCOVERED"
        pure hd

      Nothing -> do
        headblk <- MaybeT $ getRefChanHead sto key
        debug "HEAD FOUND"
        pure headblk

getRefChanHead :: forall e s m . ( MonadIO m
                                 , s ~ Encryption e
                                 , ForRefChans e
                                 , Signatures s
                                 )
                => AnyStorage
                -> RefChanHeadKey s
                -> m (Maybe (RefChanHeadBlock e))

getRefChanHead sto k = runMaybeT do
    h <- MaybeT $ liftIO $ getRef sto k
    hdblob <- MaybeT $ readBlobFromTree ( getBlock sto ) (HashRef h)
    (_, headblk) <- MaybeT $ pure $ unboxSignedBox @(RefChanHeadBlock e) @s hdblob
    pure headblk


checkACL :: forall e s . (Encryption e ~ s, ForRefChans e)
         => ACLType
         -> RefChanHeadBlock e
         -> Maybe (PubKey 'Sign s)
         -> PubKey 'Sign s
         -> Bool

checkACL acl theHead mbPeerKey authorKey = match
  where
    pips = view refChanHeadPeers theHead
    aus  = view refChanHeadAuthors theHead
    notifiers = view refChanHeadNotifiers theHead
    match = maybe True (`HashMap.member` pips) mbPeerKey
           && ( authorKey `HashSet.member` aus
              || acl == ACLNotify && authorKey `HashSet.member` notifiers
              )

