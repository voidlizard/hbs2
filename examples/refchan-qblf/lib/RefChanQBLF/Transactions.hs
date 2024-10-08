{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RefChanQBLF.Transactions where

import Data.String.Conversions (cs)
import HBS2.Base58
import HBS2.Data.Types.Refs (HashRef (..))
import HBS2.Data.Types.SignedBox
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Net.Messaging.Unix (UNIX)
import HBS2.Peer.Proto
import HBS2.Prelude.Plated

import Codec.Serialise
import Control.Arrow hiding ((<+>))
import Data.ByteString.Lazy (ByteString)
import Data.Hashable (Hashable (..))
import Data.Word (Word64)
import System.Random

import RefChanQBLF.Common

newtype Actor s = Actor {fromActor :: PubKey 'Sign s}
    deriving stock (Generic)

deriving stock instance (Eq (PubKey 'Sign s)) => Eq (Actor s)
deriving newtype instance (Hashable (PubKey 'Sign s)) => Hashable (Actor s)

instance (Pretty (AsBase58 (PubKey 'Sign s))) => Pretty (Actor s) where
    pretty (Actor a) = pretty (AsBase58 a)

type Account s = PubKey 'Sign s

newtype Amount = Amount Integer
    deriving stock (Eq, Show, Ord, Data, Generic)
    deriving newtype (Read, Enum, Num, Integral, Real, Pretty)

newtype DAppState = DAppState {fromDAppState :: HashRef}
    deriving stock (Eq, Show, Ord, Data, Generic)
    deriving newtype (Hashable, Pretty)

instance Hashed HbSync DAppState where
    hashObject (DAppState (HashRef h)) = h

data EmitTx s = EmitTx (Account s) Amount Word64
    deriving stock (Generic)

instance (Pretty (AsBase58 (PubKey 'Sign s))) => Pretty (EmitTx s) where
    pretty (EmitTx acc amount n) =
        "Emit"
            <+> "to:"
            <> pretty (AsBase58 acc)
                <+> "amount:"
            <> pretty amount
                <+> "nonce:"
            <> pretty n

data MoveTx s = MoveTx (Account s) (Account s) Amount Word64
    deriving stock (Generic)

instance (Pretty (AsBase58 (PubKey 'Sign s))) => Pretty (MoveTx s) where
    pretty (MoveTx accfrom accto amount n) =
        "Move"
            <+> "from:"
            <> pretty (AsBase58 accfrom)
                <+> "to:"
            <> pretty (AsBase58 accto)
                <+> "amount:"
            <> pretty amount
                <+> "nonce:"
            <> pretty n

data QBLFDemoToken s
    = Emit (SignedBox (EmitTx s) s) --  proof: owner's  key
    | Move (SignedBox (MoveTx s) s) --  proof: wallet's key
    deriving stock (Generic)

instance
    ( Pretty (AsBase58 (PubKey 'Sign s))
    , Signatures s
    , Eq (Signature s)
    , FromStringMaybe (PubKey 'Sign s)
    , Serialise (PubKey 'Sign s)
    , Serialise (Signature s)
    , Hashable (PubKey 'Sign s)
    )
    => Pretty (QBLFDemoToken s)
    where
    pretty = \case
        Emit box -> pretty (WhiteSignedBox @s box)
        Move box -> pretty (WhiteSignedBox @s box)

newtype WhiteSignedBox s a = WhiteSignedBox (SignedBox a s)

instance
    ( Pretty (AsBase58 (PubKey 'Sign s))
    , Pretty a
    , Serialise a
    )
    => Pretty (WhiteSignedBox s a)
    where
    pretty (WhiteSignedBox (SignedBox pk bs _sign)) =
        "SignedBox"
            <+> "Hash:"
            <+> pretty ((hashObject @HbSync . serialise) bs)
            <+> "SignedBy:"
            <+> pretty (AsBase58 pk)
            <+> "("
            <> pretty ((PrettyEither . left show . deserialiseOrFail @a . cs) bs)
            <> ")"

instance (ForQBLFDemoToken s) => Serialise (Actor s)

instance Serialise DAppState

instance Serialise Amount

instance (ForQBLFDemoToken s) => Serialise (EmitTx s)

instance (ForQBLFDemoToken s) => Serialise (MoveTx s)

instance (ForQBLFDemoToken s) => Serialise (QBLFDemoToken s)

type ForQBLFDemoToken s =
    ( Eq (PubKey 'Sign s)
    , Eq (Signature s)
    , Pretty (AsBase58 (PubKey 'Sign s))
    , ForSignedBox s
    , FromStringMaybe (PubKey 'Sign s)
    , Serialise (PubKey 'Sign s)
    , Serialise (Signature s)
    , Hashable (PubKey 'Sign s)
    )

deriving stock instance (ForQBLFDemoToken s) => Eq (QBLFDemoToken s)

instance (ForQBLFDemoToken s) => Hashable (QBLFDemoToken s) where
    hashWithSalt salt = \case
        Emit box -> hashWithSalt salt box
        Move box -> hashWithSalt salt box

newtype QBLFDemoTran e
    = QBLFDemoTran (SignedBox (QBLFDemoToken (Encryption e)) (Encryption e))
    deriving stock (Generic)

instance (ForRefChans e) => Serialise (QBLFDemoTran e)

deriving newtype instance
    (Eq (PubKey 'Sign (Encryption e)), Eq (Signature (Encryption e)))
    => Eq (QBLFDemoTran e)

deriving newtype instance
    (Eq (Signature (Encryption e)), ForRefChans e)
    => Hashable (QBLFDemoTran e)

instance HasProtocol UNIX (QBLFDemoTran UNIX) where
    type ProtocolId (QBLFDemoTran UNIX) = 0xFFFF0001
    type Encoded UNIX = ByteString
    decode = either (const Nothing) Just . deserialiseOrFail
    encode = serialise

makeEmitDemoToken
    :: forall s e m
     . ( MonadIO m
       , ForRefChans e
       , ForQBLFDemoToken s
       , Signatures (Encryption e)
       , s ~ Encryption e
       )
    => PubKey 'Sign s
    -> PrivKey 'Sign s
    -> Account s
    -> Amount
    -> m (QBLFDemoToken s)
makeEmitDemoToken pk sk acc amount = do
    nonce <- randomIO
    let box = makeSignedBox @s pk sk (EmitTx acc amount nonce)
    pure (Emit @s box)

makeMoveDemoToken
    :: forall s e m
     . ( MonadIO m
       , ForQBLFDemoToken s
       , ForRefChans e
       , Signatures s
       , s ~ Encryption e
       )
    => PubKey 'Sign s -- from pk
    -> PrivKey 'Sign s -- from sk
    -> Account s
    -> Amount -- amount
    -> m (QBLFDemoToken s)
makeMoveDemoToken pk sk acc amount = do
    nonce <- randomIO
    let box = makeSignedBox @s pk sk (MoveTx pk acc amount nonce)
    pure (Move @s box)
