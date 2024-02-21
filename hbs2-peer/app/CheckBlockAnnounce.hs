{-# Language TypeOperators #-}
module CheckBlockAnnounce where

import HBS2.Prelude.Plated
import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Data.Types.Peer
import HBS2.Hash
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Net.Proto.Types

import PeerTypes
import PeerConfig
import CheckPeer (peerBanned)
import BlockDownload
import DownloadQ

import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.Set qualified as Set
import Data.Set (Set)
import Lens.Micro.Platform
import Data.Text qualified as Text
import Data.Maybe

data PeerAcceptAnnounceKey

data AcceptAnnounce = AcceptAnnounceAll
                    | AcceptAnnounceFrom (Set (PubKey 'Sign (Encryption L4Proto)))

instance Pretty AcceptAnnounce where
  pretty = \case
    AcceptAnnounceAll     -> parens ("accept-announce" <+> "*")

    -- FIXME: better-pretty-for-AcceptAnnounceFrom
    AcceptAnnounceFrom xs -> parens ("accept-announce" <+> pretty (fmap AsBase58 (Set.toList xs)))



instance Monad m => HasCfgKey PeerAcceptAnnounceKey AcceptAnnounce m where
  key = "accept-block-announce"

instance (Monad m, HasConf m) => HasCfgValue PeerAcceptAnnounceKey AcceptAnnounce m where
  cfgValue = do
    syn <- getConf
    pure $ fromMaybe (AcceptAnnounceFrom (lst syn)) (fromAll syn)
    where
      fromAll syn = headMay [ AcceptAnnounceAll | ListVal (Key s [SymbolVal "*"]) <- syn, s == kk ]
      lst syn = Set.fromList $
                catMaybes [ fromStringMay @(PubKey 'Sign (Encryption L4Proto)) (Text.unpack e)
                          | ListVal (Key s [LitStrVal e]) <- syn, s == kk
                          ]
      kk = key @PeerAcceptAnnounceKey @AcceptAnnounce @m


acceptAnnouncesFromPeer :: forall e m . ( MonadIO m
                                        , Sessions e (KnownPeer e) m
                                        , IsPeerAddr e m
                                        , Ord (PubKey 'Sign (Encryption e))
                                        , FromStringMaybe (PubKey 'Sign (Encryption e))
                                        , Ord (PubKey 'Sign (Encryption e))
                                        , e ~ L4Proto
                                        )
                      => PeerConfig
                      -> PeerAddr e
                      -> m Bool
acceptAnnouncesFromPeer conf@(PeerConfig syn) pa = runPlus do

  pip <- lift (fromPeerAddr @e pa)

  pd <- toMPlus =<< lift (find @e (KnownPeerKey pip) id)

  let accptAnn = runReader (cfgValue @PeerAcceptAnnounceKey) syn

  guard . not =<< peerBanned conf pd

  case accptAnn of
    AcceptAnnounceAll -> do
      pure ()

    AcceptAnnounceFrom s -> do
      guard  (view peerSignKey pd `Set.member` s)

  where
    runPlus m = runMaybeT m <&> isJust


checkBlockAnnounce :: forall e m . ( e ~ L4Proto
                                   , m ~ PeerM e IO
                                   )
                   => PeerConfig
                   -> DownloadEnv e
                   -> PeerNonce
                   -> PeerAddr e
                   -> Hash HbSync
                   -> m ()

checkBlockAnnounce conf denv nonce pa h = void $ runMaybeT do

  accept <- lift $ acceptAnnouncesFromPeer conf pa

  myNonce  <- lift $ peerNonce @e

  guard (nonce /= myNonce)

  debug $ "Accept announce from" <+> pretty pa <+> pretty accept

  guard accept

  lift do
    withDownload denv $ do
      processBlock h

