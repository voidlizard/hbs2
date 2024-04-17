{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git.Data.Tx.Index where

import HBS2.Git.Client.Prelude
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox

import HBS2.Storage.Operations.Class

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce
import Control.Monad.Identity

import Data.Word

-- |
-- Module      : HBS2.Git.Data.Tx.Index
-- Description : hbs2-git index data structures
--

-- FIXME: fix-all-this-constraint-absurde
type ForGitIndex s = ( ForSignedBox s
                     , IsRefPubKey s
                     )

data RepoForkInfo e =
  RepoForkInfoNone
  deriving stock (Generic)


data GitRepoAnnounce s =
  GitRepoAnnounce
  { repoLwwRef   :: LWWRefKey s
  , repoForkInfo :: Maybe (RepoForkInfo s)
  }
  deriving stock (Generic)


instance ForGitIndex s => Serialise (RepoForkInfo s)
instance ForGitIndex s => Serialise (GitRepoAnnounce s)

instance ForGitIndex s => Pretty (GitRepoAnnounce s) where
  pretty GitRepoAnnounce{..} = parens $ "git-repo-announce" <+> pretty repoLwwRef

newtype NotifyCredentials s = NotifyCredentials (PeerCredentials s)

data GitIndexEntry =
    GitIndexRepoDefine
  | GitIndexRepoTombEntry
  | GitIndexRepoLikes Integer
  deriving stock (Generic)

data GitIndexTx s =
  GitIndexTx
  { gitIndexTxRef      :: LWWRefKey s    -- ^ primary key
  , gitIndexTxSeq      :: Word64         -- ^ sequence ( set tomb / bring from tomb )
  , gitIndexTxPayload  :: GitIndexEntry  -- ^ statement
  }
  deriving stock (Generic)

instance ForGitIndex s => Pretty (GitIndexTx s) where
  pretty GitIndexTx{..} = case gitIndexTxPayload of
    GitIndexRepoDefine    -> "git-repo-define" <+> pretty gitIndexTxRef
    GitIndexRepoTombEntry -> "git-repo-tomb"   <+> pretty gitIndexTxRef
    GitIndexRepoLikes n   -> "git-repo-likes"  <+> pretty gitIndexTxRef <+> pretty n

-- | makes notification tx
-- | it is signed by lwwref private key in order to proove authorship
-- | and signed with published notification private key in order
-- | to publish tx via rpc
makeNotificationTx :: forall  s . (ForGitIndex s)
                   => NotifyCredentials s
                   -> LWWRefKey s
                   -> PrivKey 'Sign s
                   -> Maybe (RepoForkInfo s)
                   -> SignedBox ByteString s
makeNotificationTx ncred lww lwsk forkInfo = do
  let creds = coerce ncred :: PeerCredentials s
  let annData = GitRepoAnnounce @s lww forkInfo
  let lwpk = coerce lww :: PubKey 'Sign s
  let repoAnn = makeSignedBox @s lwpk lwsk (LBS.toStrict $ serialise annData)
  makeSignedBox @s (view peerSignPk creds) (view peerSignSk creds) (LBS.toStrict $ serialise repoAnn)


unpackNotificationTx :: forall s m . (ForGitIndex s, MonadError OperationError m)
                     => SignedBox ByteString s
                     -> m (GitRepoAnnounce s)
unpackNotificationTx box = do
  (_, bs1) <- unboxSignedBox0 @_ @s box
                & orThrowError SignCheckError

  bs2 <- deserialiseOrFail @(SignedBox ByteString s) (LBS.fromStrict bs1)
            & orThrowError UnsupportedFormat

  (_, bs3) <- unboxSignedBox0 bs2
                & orThrowError SignCheckError

  deserialiseOrFail @(GitRepoAnnounce s) (LBS.fromStrict bs3)
    & orThrowError UnsupportedFormat



