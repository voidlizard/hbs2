{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2.Peer.Proto.Mailbox.Policy.Basic
  ( module HBS2.Peer.Proto.Mailbox.Policy
  , BasicPolicyAction(..)
  , getAsSyntax
  , parseBasicPolicy
  , BasicPolicy(..)
  ) where

import HBS2.Prelude.Plated

import HBS2.Base58
import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Peer.Proto.Mailbox.Policy
import HBS2.Net.Auth.Credentials

import HBS2.System.Dir

import Data.Config.Suckless.Script

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe

data BasicPolicyAction =
  Allow | Deny
  deriving (Eq,Ord,Show,Generic)

data BasicPolicy s =
  BasicPolicy
  { bpDefaulPeerAction    :: BasicPolicyAction
  , bpDefaultSenderAction :: BasicPolicyAction
  , bpPeers               :: HashMap (PubKey 'Sign s) BasicPolicyAction
  , bpSenders             :: HashMap (Sender s) BasicPolicyAction
  }
  deriving stock (Generic)

instance ForMailbox s => Pretty (BasicPolicy s) where
  pretty w = pretty (getAsSyntax @C w)

instance ForMailbox s => IsAcceptPolicy s (BasicPolicy s) where

  policyAcceptPeer BasicPolicy{..} p = do
    pure $ Allow == fromMaybe bpDefaultSenderAction (HM.lookup p bpPeers)

  policyAcceptMessage BasicPolicy{..} s m = do
    pure $ Allow == fromMaybe bpDefaultSenderAction (HM.lookup s bpSenders)

getAsSyntax :: forall c s . (ForMailbox s, IsContext c)
            => BasicPolicy s -> [Syntax c]
getAsSyntax BasicPolicy{..} =
  [ defPeerAction
  , defSenderAction
  ] <> peerActions <> senderActions
  where
    defPeerAction   = mkList [mkSym "peer", action bpDefaulPeerAction, mkSym "all"]
    defSenderAction = mkList [mkSym "sender", action bpDefaulPeerAction, mkSym "all"]

    peerActions = [ mkList [mkSym "peer", action a, mkSym (show $ pretty (AsBase58 who))]
                  | (who, a) <- HM.toList bpPeers ]

    senderActions = [ mkList [mkSym "sender", action a, mkSym (show $ pretty (AsBase58 who))]
                    | (who, a) <- HM.toList bpSenders ]


    action = \case
      Allow -> mkSym "allow"
      Deny  -> mkSym "deny"


parseBasicPolicy :: forall s c m . (IsContext c, s ~ HBS2Basic, ForMailbox s, MonadUnliftIO m)
                 => [Syntax c]
                 -> m (Maybe (BasicPolicy s))

parseBasicPolicy syn = do

  tpAction <- newTVarIO Deny
  tsAction <- newTVarIO Deny
  tpeers   <- newTVarIO mempty
  tsenders <- newTVarIO mempty

  for_ syn $ \case
    ListVal [SymbolVal "peer", SymbolVal "allow", SymbolVal "all"]  -> do
      atomically $ writeTVar tpAction Allow

    ListVal [SymbolVal "peer", SymbolVal "deny", SymbolVal "all"]  -> do
      atomically $ writeTVar tpAction Deny

    ListVal [SymbolVal "peer", SymbolVal "allow", SignPubKeyLike who]  -> do
      atomically $ modifyTVar tpeers (HM.insert who Allow)

    ListVal [SymbolVal "peer", SymbolVal "deny", SignPubKeyLike who]  -> do
      atomically $ modifyTVar tpeers (HM.insert who Deny)

    ListVal [SymbolVal "sender", SymbolVal "allow", SymbolVal "all"]  -> do
      atomically $ writeTVar tsAction Allow

    ListVal [SymbolVal "sender", SymbolVal "deny", SymbolVal "all"]  -> do
      atomically $ writeTVar tsAction Deny

    ListVal [SymbolVal "sender", SymbolVal "allow", SignPubKeyLike who]  -> do
      atomically $ modifyTVar tsenders (HM.insert who Allow)

    ListVal [SymbolVal "sender", SymbolVal "deny", SignPubKeyLike who]  -> do
      atomically $ modifyTVar tsenders (HM.insert who Deny)

    _ -> pure ()

  a <- readTVarIO tpAction
  b <- readTVarIO tsAction
  c <- readTVarIO tpeers
  d <- readTVarIO tsenders

  pure $ Just $ BasicPolicy  @s a b c d


