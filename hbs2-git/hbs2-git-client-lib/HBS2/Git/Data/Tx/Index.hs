{-# Language UndecidableInstances #-}
module HBS2.Git.Data.Tx.Index where

import HBS2.Git.Client.Prelude
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox

import Data.ByteString (ByteString)


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
  NotifyCredentials (PubKey 'Sign s) (PrivKey 'Sign s)

data RepoCredentials s =
  RepoCredentials
  { rcPubKey :: PubKey 'Sign s
  , rcSeckey :: PrivKey 'Sign s
  }

makeNotificationTx :: forall  s m . (Monad m, ForGitIndex s)
                   => NotifyCredentials s
                   -> RepoCredentials s
                   -> Maybe (RepoForkInfo s)
                   -> m ByteString
makeNotificationTx ncred repocred forkInfo = do
  -- makeSignedBox @e  (LBS.toStrict $ serialise tx)
  let annData = GitRepoAnnounceData @s (LWWRefKey $ rcPubKey repocred)
  undefined

