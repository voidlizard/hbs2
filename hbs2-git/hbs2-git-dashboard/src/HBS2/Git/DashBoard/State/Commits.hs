{-# LANGUAGE TemplateHaskell #-}
module HBS2.Git.DashBoard.State.Commits where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types

import HBS2.Git.Local
import HBS2.Git.Local.CLI

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text.Encoding qualified as Text
import Data.Text qualified as Text
import Data.Time (UTCTime,LocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Either

import Streaming.Prelude qualified as S

{- HLINT ignore "Functor law" -}

data CommitListStyle = CommitListBrief

data SelectCommitsPred =
  SelectCommitsPred
  { _commitListStyle  :: CommitListStyle
  , _commitPredOffset :: Int
  , _commitPredLimit  :: Int
  }

makeLenses ''SelectCommitsPred

instance Semigroup SelectCommitsPred where
  (<>) _ _ = mempty

instance Monoid SelectCommitsPred where
  mempty = SelectCommitsPred CommitListBrief 0 100

briefCommits :: SelectCommitsPred
briefCommits = mempty

newtype Author = Author Text
                 deriving stock (Generic,Data)
                 deriving newtype (Show)


newtype CommitListItemHash = CommitListItemHash GitHash
                              deriving stock (Generic,Data)
                              deriving newtype (Show,Pretty)

newtype CommitListItemTime  = CommitListItemTime Integer
                              deriving stock (Generic,Data)
                              deriving newtype (Show)

newtype CommitListItemTitle = CommitListItemTitle Text
                              deriving stock (Generic,Data)
                              deriving newtype (Show)

newtype CommitListItemAuthor = CommitListItemAuthor Author
                               deriving stock (Generic,Data)
                               deriving newtype (Show)

data CommitListItem =
  CommitListItemBrief
  { commitListHash   :: CommitListItemHash
  , commitListTime   :: CommitListItemTime
  , commitListTitle  :: CommitListItemTitle
  , commitListAuthor :: CommitListItemAuthor
  }
  deriving stock (Generic,Data)

selectCommits :: (DashBoardPerks m, MonadReader DashBoardEnv m)
              => LWWRefKey 'HBS2Basic
              -> SelectCommitsPred
              -> m [CommitListItem]

selectCommits lww SelectCommitsPred{..} = do
  let lim = _commitPredLimit
  let off = _commitPredOffset
  let delim = "|||" :: Text
  dir <- repoDataPath lww

  let cmd = case _commitListStyle of
              CommitListBrief -> do
                let fmt = [qc|--pretty=format:"%H{delim}%at{delim}%an{delim}%s"|] :: String
                [qc|git --git-dir={dir} log --all --max-count {lim} --skip {off} {fmt}|]

  debug $ red "selectCommits" <+> pretty cmd

  ls <- gitRunCommand cmd
          <&> fromRight mempty
          <&> LBS8.lines
          <&> fmap (Text.decodeUtf8 . LBS8.toStrict)

  S.toList_ do
    for_ ls $ \l -> do
      case  Text.splitOn "|||" l of
        z@[cohash,ts,au,msg] -> do

          let utc = readMay @Integer (Text.unpack ts)
                     <&> CommitListItemTime

          let hash = fromStringMay @GitHash (Text.unpack cohash)
                      <&> CommitListItemHash

          let co = CommitListItemBrief
                    <$> hash
                    <*> utc
                    <*> pure (CommitListItemTitle msg)
                    <*> pure (CommitListItemAuthor (Author au))

          maybe1 co none S.yield

        _ -> none



