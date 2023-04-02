{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language TemplateHaskell #-}
module Brains where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Hash

import HBS2.System.Logger.Simple

import Data.Maybe
import Control.Monad
import Control.Exception
import Control.Concurrent.STM
import Control.Concurrent.Async
import Lens.Micro.Platform
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict (HashMap)
import Data.List qualified as List
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Text.InterpolatedString.Perl6 (qc)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import System.Random (randomRIO)
import Data.Word

class HasBrains e a where

  onKnownPeers :: MonadIO m => a -> [Peer e] -> m ()

  onBlockDownloadAttempt :: ( MonadIO m
                            , IsPeerAddr e m
                            )
                         => a
                         -> Peer e
                         -> Hash HbSync
                         -> m ()

  onBlockDownloaded :: MonadIO m
                    => a
                    -> Peer e
                    -> Hash HbSync
                    -> m ()

  onBlockPostponed :: MonadIO m
                   => a
                   -> Hash HbSync
                   -> m ()

  claimBlockCameFrom :: MonadIO m
                     => a
                     -> Hash HbSync
                     -> Hash HbSync
                     -> m ()

  shouldPostponeBlock :: MonadIO m
                     => a
                     -> Hash HbSync
                     -> m Bool


  shouldDownloadBlock :: MonadIO m
                      => a
                      -> Peer e
                      -> Hash HbSync
                      -> m Bool

  advisePeersForBlock :: (MonadIO m, FromStringMaybe (PeerAddr e))
                       => a
                       -> Hash HbSync
                       -> m [PeerAddr e]

type NoBrains = ()

instance Pretty (Peer e) => HasBrains e NoBrains  where

  onKnownPeers _ ps = pure ()

  onBlockDownloadAttempt _ p h = do
    pure ()

  onBlockDownloaded _ p h = do
    pure ()

  onBlockPostponed _ h = do
    pure ()

  claimBlockCameFrom _ _ _ = do pure ()

  shouldPostponeBlock _ _ = pure False

  shouldDownloadBlock _ _ _ = pure True

  advisePeersForBlock _ _ = pure mempty

data SomeBrains e = forall a . HasBrains e a => SomeBrains a

instance HasBrains e (SomeBrains e) where
  onKnownPeers (SomeBrains a) = onKnownPeers a
  onBlockDownloadAttempt (SomeBrains a) = onBlockDownloadAttempt a
  onBlockDownloaded (SomeBrains a) = onBlockDownloaded a
  onBlockPostponed (SomeBrains a) = onBlockPostponed @e a
  claimBlockCameFrom (SomeBrains a) = claimBlockCameFrom @e a
  shouldPostponeBlock (SomeBrains a) = shouldPostponeBlock @e a
  shouldDownloadBlock (SomeBrains a) = shouldDownloadBlock @e a
  advisePeersForBlock (SomeBrains a) = advisePeersForBlock @e a

data BasicBrains e =
  BasicBrains
  { _brainsPeers        :: TVar [Peer e]
  , _brainsPostponeDown :: TVar (HashMap (Peer e, Hash HbSync) Int )
  , _brainsExpire       :: Cache (Hash HbSync) ()
  , _brainsDb           :: Connection
  , _brainsPipeline     :: TQueue (IO ())
  }

makeLenses 'BasicBrains


cleanupPostponed :: MonadIO m => BasicBrains e -> Hash HbSync -> m ()
cleanupPostponed b h =  do
    let po = view brainsPostponeDown b
    let flt (_,h1) _ = h1 /= h
    liftIO $ atomically $ modifyTVar po $ HashMap.filterWithKey flt

instance (Hashable (Peer e), Pretty (Peer e)) => HasBrains e (BasicBrains e) where

  onKnownPeers br ps = do
    -- trace "BRAINS: onKnownPeers"
    let tv = view brainsPeers br
    liftIO $ atomically $ writeTVar tv ps

  onBlockDownloadAttempt b peer h = do
    -- trace "BRAINS: onBlockDownloadAttempt"
    noPeers <- liftIO $ readTVarIO (view brainsPeers b) <&> List.null
    unless noPeers do
      let cache = view brainsExpire b
      liftIO $ Cache.insert cache h ()
      let doAlter = HashMap.alter (maybe (Just 0) (Just . succ)) (peer,h)
      liftIO $ atomically $ modifyTVar (view brainsPostponeDown b) doAlter

  onBlockDownloaded b p h = do
    -- trace $ "BRAINS: onBlockDownloaded" <+> pretty p <+> pretty h
    cleanupPostponed b h
    updateOP b $ insertPeer b h p

  onBlockPostponed b h  = do
    trace $ "BRAINS: onBlockPostponed" <+> pretty h
    cleanupPostponed b h

  claimBlockCameFrom b f t = do
    -- trace $ "BRAINS: claimBlockCameFrom" <+> pretty f <+> pretty t
    updateOP b $ insertAncestor b f t

  shouldPostponeBlock b h = do
    peers <- liftIO $ readTVarIO (view brainsPeers b)
    downs <- liftIO $ readTVarIO (view brainsPostponeDown b)

    r <- forM peers $ \p -> do
           let v = HashMap.lookup (p,h) downs & fromMaybe 0 & (<2)
           pure [v]

    let postpone = not (List.null r || or (mconcat r) )

    pure postpone

  shouldDownloadBlock b p h = do
    noPeers <- liftIO $ readTVarIO (view brainsPeers b) <&> List.null
    downs <- liftIO $ readTVarIO (view brainsPostponeDown b)
    pure $ noPeers || (HashMap.lookup (p,h) downs & fromMaybe 0 & (<2))

  advisePeersForBlock b h = do
    r <- liftIO $ findPeers b h
    pure $ mapMaybe fromStringMay r

updateOP :: forall e m . MonadIO m => BasicBrains e -> IO () -> m ()
updateOP br op = do
  let pip = view brainsPipeline br
  liftIO $ atomically $ writeTQueue pip (liftIO op)

insertAncestor :: BasicBrains e
               -> Hash HbSync -- ^ parent
               -> Hash HbSync -- ^ child
               -> IO ()

insertAncestor br parent child = do

  -- trace $ "INSERT ANCESTOR" <+> pretty parent <+> pretty child

  let conn = view brainsDb br

  void $ liftIO $ execute conn [qc|
    insert into ancestors (child, parent) values (?,?)
    on conflict (child,parent) do nothing
  |] (show $ pretty child, show $ pretty parent)


insertPeer :: forall e . Pretty (Peer e)
           => BasicBrains e
           -> Hash HbSync -- ^ block
           -> Peer e -- ^ peer
           -> IO ()

insertPeer br blk peer = do

  -- trace $ "INSERT PEER" <+> pretty peer <+> pretty blk

  let conn = view brainsDb br

  void $ liftIO $ execute conn [qc|
    insert into seenby (block, peer) values (?,?)
    on conflict (block,peer) do nothing
  |] (show $ pretty blk, show $ pretty peer)


newtype DBData a = DBData { fromDBData :: a }

instance FromField (DBData (Hash HbSync)) where
  fromField = fmap (DBData . fromString) . fromField @String

getAncestors :: forall e m . (MonadIO m)
               => BasicBrains e
               -> Hash HbSync
               -> m [Hash HbSync]

getAncestors br child = do

  let conn = view brainsDb br

  let sql = [qc|
WITH RECURSIVE ancestor_list(parent) AS (
  SELECT parent
  FROM ancestors
  WHERE child = ?
  UNION
  SELECT a.parent
  FROM ancestors a
  JOIN ancestor_list al ON a.child = al.parent
)
SELECT parent FROM ancestor_list;
|]

  liftIO $ query conn sql (Only (show $ pretty child) )
    <&> fmap (fromDBData . fromOnly)


findPeers :: BasicBrains e
          -> Hash HbSync
          -> IO [String]

findPeers br child = do

  let conn = view brainsDb br

  let sql = [qc|
WITH RECURSIVE ancestor_list(parent) AS (
  SELECT parent
  FROM ancestors
  WHERE child = ?
  UNION
  SELECT a.parent
  FROM ancestors a
  JOIN ancestor_list al ON a.child = al.parent
)

SELECT s.peer
  FROM ancestor_list a
    JOIN seenby s on s.block = a.parent;
|]

  liftIO $ query conn sql (Only (show $ pretty child) ) <&> fmap fromOnly


cleanupHashes :: BasicBrains e
              -> IO ()

cleanupHashes br  = do

  debug "BRAINS: cleanup caches"

  let conn = view brainsDb br

  let sql = [qc|
SAVEPOINT zzz1;

DELETE FROM ancestors WHERE strftime('%s','now') - strftime('%s', ts) > 600;
DELETE FROM seenby WHERE strftime('%s','now') - strftime('%s', ts) > 600;

RELEASE SAVEPOINT zzz1;

  |]

  r <- try $ liftIO $ execute_ conn sql

  case r of
    Right{} -> pure ()
    Left (e :: SomeException) -> err $ "BRAINS: " <+> viaShow e

transactional :: BasicBrains e -> IO () -> IO ()
transactional brains action = do
  n <- randomRIO @Word16 (1, maxBound)
  let sp = [qc|sp{n}|] :: String
  let conn = view brainsDb brains
  execute_ conn [qc|SAVEPOINT {sp}|]
  try action >>= \case
    Right{} -> do
      execute_ conn [qc|RELEASE SAVEPOINT {sp}|]

    Left ( _ :: SomeException ) -> do
      execute_ conn [qc|ROLLBACK TO SAVEPOINT {sp}|]

-- FIXME: eventually-close-db
newBasicBrains :: forall e m . (Hashable (Peer e), MonadIO m) => m (BasicBrains e)
newBasicBrains = liftIO do

  conn <- open ":memory:"

  execute_ conn [qc|
    create table if not exists ancestors
    ( child text not null
    , parent text not null
    , ts DATE DEFAULT (datetime('now','localtime'))
    , primary key (child,parent))
  |]

  execute_ conn [qc|
    create table if not exists seenby
    ( block text not null
    , peer text not null
    , ts DATE DEFAULT (datetime('now','localtime'))
    , primary key (block,peer))
  |]

  BasicBrains <$> newTVarIO mempty
              <*> newTVarIO mempty
              <*> Cache.newCache (Just (toTimeSpec (30 :: Timeout 'Seconds)))
              <*> pure conn
              <*> newTQueueIO

runBasicBrains :: MonadIO m => BasicBrains e -> m ()
runBasicBrains brains = do

  let pip = view brainsPipeline brains
  let expire = view brainsExpire brains

  -- FIXME: async-error-handling
  void $ liftIO $ async $ forever do
    pause @'Seconds 5
    -- transactional brains do
    w  <- atomically $ readTQueue pip
    ws <- atomically $ flushTQueue pip
    transactional brains (sequence_ (w:ws))

  void $ liftIO $ async $ forever do
    pause @'Seconds 60
    updateOP brains (cleanupHashes brains)

  void $ forever do
    pause @'Seconds 15
    ee <- liftIO $ Cache.toList expire
    let eee = [ h | (h,_,Just{}) <- ee ]
    forM_ eee $ \h -> do
      cleanupPostponed brains h
    liftIO $ Cache.purgeExpired expire

