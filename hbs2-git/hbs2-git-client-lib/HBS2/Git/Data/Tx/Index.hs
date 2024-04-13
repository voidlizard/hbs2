{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git.Data.Tx.Index where

import HBS2.Git.Client.Prelude
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce

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


data GitRepoAnnounceData s =
  GitRepoAnnounceData
  { repoLwwRef   :: LWWRefKey s
  , repoForkInfo :: Maybe (RepoForkInfo s)
  }
  deriving stock (Generic)

data GitRepoAnnounce s =
  GitRepoAnnounce
  { gitRepoAnnounce :: SignedBox (GitRepoAnnounceData s) s
  }
  deriving stock (Generic)


instance ForGitIndex s => Serialise (RepoForkInfo s)
instance ForGitIndex s => Serialise (GitRepoAnnounceData s)
instance ForGitIndex s => Serialise (GitRepoAnnounce s)


data NotifyCredentials s =
  NotifyCredentials
  { notifyPk :: PubKey 'Sign s
  , notifySk :: PrivKey 'Sign s
  }

-- | makes notification tx
-- | it is signed by lwwref private key in order to proove authorship
-- | and signed with published notification private key in order
-- | to publish tx via rpc
makeNotificationTx :: forall  s m . (Monad m, ForGitIndex s)
                   => NotifyCredentials s
                   -> LWWRefKey s
                   -> ( PubKey 'Sign s -> m (Maybe (PrivKey 'Sign s) ) )
                   -> Maybe (RepoForkInfo s)
                   -> m (Maybe (SignedBox ByteString s))
makeNotificationTx ncred lww klook forkInfo = runMaybeT do
  let annData = GitRepoAnnounceData @s lww forkInfo
  let lwpk = coerce lww :: PubKey 'Sign s
  lwsk <- MaybeT $ klook lwpk
  let repoAnn = makeSignedBox @s lwpk lwsk (LBS.toStrict $ serialise annData)
  pure $ makeSignedBox @s (notifyPk ncred) (notifySk ncred) (LBS.toStrict $ serialise repoAnn)



