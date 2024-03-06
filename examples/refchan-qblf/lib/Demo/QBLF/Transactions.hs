{-# Language UndecidableInstances #-}
module Demo.QBLF.Transactions where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Base58
import HBS2.Peer.Proto
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Data.Types.SignedBox
import HBS2.Net.Auth.Credentials
import HBS2.Net.Messaging.Unix (UNIX)

import Data.Hashable(Hashable(..))
import Codec.Serialise
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word64)
import System.Random

newtype Actor e =
  Actor { fromActor :: PubKey 'Sign (Encryption e) }
  deriving stock (Generic)

deriving stock instance Eq (PubKey 'Sign (Encryption e)) => Eq (Actor e)
deriving newtype instance Hashable (PubKey 'Sign (Encryption e)) => Hashable (Actor e)

instance Pretty (AsBase58 (PubKey 'Sign (Encryption e))) => Pretty (Actor e) where
  pretty (Actor a) = pretty (AsBase58 a)

type Account e = PubKey 'Sign (Encryption e)

newtype Amount = Amount Integer
                deriving stock (Eq,Show,Ord,Data,Generic)
                deriving newtype (Read,Enum,Num,Integral,Real,Pretty)

newtype DAppState = DAppState { fromDAppState :: HashRef }
                deriving stock (Eq,Show,Ord,Data,Generic)
                deriving newtype (Hashable,Pretty)

instance Hashed HbSync DAppState where
  hashObject (DAppState (HashRef h)) = h

data EmitTx e = EmitTx (Account e) Amount Word64
                deriving stock (Generic)

data MoveTx e = MoveTx (Account e) (Account e) Amount Word64
                deriving stock (Generic)

data QBLFDemoToken e =
    Emit (SignedBox (EmitTx e) e)  --  proof: owner's  key
  | Move (SignedBox (MoveTx e) e)  --  proof: wallet's key
  deriving stock (Generic)

instance ForRefChans e => Serialise (Actor e)

instance Serialise DAppState

instance Serialise Amount

instance Serialise (PubKey 'Sign (Encryption e)) => Serialise (EmitTx e)

instance Serialise (PubKey 'Sign (Encryption e)) => Serialise (MoveTx e)

instance (Serialise (Account e), ForRefChans e) => Serialise (QBLFDemoToken e)

type ForQBLFDemoToken e = ( Eq (PubKey 'Sign (Encryption e))
                          , Eq (Signature (Encryption e))
                          , Pretty (AsBase58 (PubKey 'Sign (Encryption e)))
                          , ForSignedBox e
                          , FromStringMaybe (PubKey 'Sign (Encryption e))
                          , Serialise (PubKey 'Sign (Encryption e))
                          , Serialise (Signature (Encryption e))
                          , Hashable (PubKey 'Sign (Encryption e))
                          )

deriving stock instance (ForQBLFDemoToken e) => Eq (QBLFDemoToken e)

instance ForQBLFDemoToken e => Hashable (QBLFDemoToken e) where
  hashWithSalt salt = \case
    Emit box  -> hashWithSalt salt box
    Move box  -> hashWithSalt salt box

newtype QBLFDemoTran e =
  QBLFDemoTran (SignedBox (QBLFDemoToken e) e)
  deriving stock Generic

instance ForRefChans e => Serialise (QBLFDemoTran e)

deriving newtype instance
  (Eq (PubKey 'Sign (Encryption e)), Eq (Signature (Encryption e)))
  => Eq (QBLFDemoTran e)

deriving newtype instance
  (Eq (Signature (Encryption e)), ForRefChans e)
    => Hashable (QBLFDemoTran e)

instance Serialise (QBLFDemoTran UNIX) => HasProtocol UNIX (QBLFDemoTran UNIX) where
  type instance ProtocolId (QBLFDemoTran UNIX) = 0xFFFF0001
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

makeEmitTx :: forall e m . ( MonadIO m
                           , ForRefChans e
                           , Signatures (Encryption e)
                           )
           => PubKey 'Sign (Encryption e)
           -> PrivKey 'Sign (Encryption e)
           -> Account e
           -> Amount
           -> m (QBLFDemoToken e)

makeEmitTx pk sk acc amount = do
  nonce <- randomIO
  let box = makeSignedBox @e pk sk (EmitTx @e acc amount nonce)
  pure (Emit @e box)

makeMoveTx :: forall e m . ( MonadIO m
                           , ForRefChans e
                           , Signatures (Encryption e)
                           )
           => PubKey 'Sign (Encryption e)  -- from pk
           -> PrivKey 'Sign (Encryption e) -- from sk
           -> Account e
           -> Amount                       -- amount
           -> m (QBLFDemoToken e)

makeMoveTx pk sk acc amount = do
  nonce <- randomIO
  let box = makeSignedBox @e pk sk (MoveTx @e pk acc amount nonce)
  pure (Move @e box)

