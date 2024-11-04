{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language TemplateHaskell #-}
{-# Language TypeOperators #-}
{-# Language RecordWildCards #-}
module Brains
  ( module Brains
  , module HBS2.Peer.Brains
  ) where

import HBS2.Prelude.Plated
import HBS2.Peer.Proto.RefChan(ForRefChans)
import HBS2.Data.Types.Refs
import HBS2.Net.Proto
import HBS2.Hash
import HBS2.Base58
import HBS2.Net.IP.Addr

import HBS2.Peer.Brains

import HBS2.Misc.PrettyStuff

import PeerLogger
import PeerConfig

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Crypto.Saltine.Core.Box qualified as Encrypt
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Either
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Maybe
import Data.Text qualified as Text
import Data.Time.Clock (addUTCTime,getCurrentTime)
import Data.Word
import Lens.Micro.Platform
import System.Directory
import System.FilePath
import System.Random (randomRIO)
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO (MonadUnliftIO(..),race,withAsync)

import Streaming.Prelude qualified as S

data PeerBrainsDb


-- TODO: refactor-brains
--   переключить на db-pipe
--   асинки - в runContT

-- FIXME: move-that-orphans-somewhere

instance ToField HashRef where
  toField h = toField (show $ pretty h)

instance FromField HashRef where
  fromField = fmap fromString . fromField @String


instance HasCfgKey PeerBrainsDb (Maybe String) where
  key = "brains"

newtype CommitCmd = CommitCmd { onCommited :: IO () }

data BasicBrains e =
  BasicBrains
  { _brainsPeers        :: TVar (HashSet (Peer e))
  , _brainsPostponeDown :: TVar (HashMap (Peer e, Hash HbSync) Int )
  , _brainsExpire       :: Cache (Hash HbSync) ()
  , _brainsRemoved      :: Cache HashRef ()
  , _brainsDb           :: Connection
  , _brainsPipeline     :: TQueue (IO ())
  , _brainsCommit       :: TQueue CommitCmd
  , _brainsDelDownload  :: TQueue (Hash HbSync)
  , _brainsSizeCache    :: Cache (Peer e, Hash HbSync) Integer
  , _brainsPolled       :: TVar (HashSet (PubKey 'Sign (Encryption e), String))
  , _brainsProbe        :: TVar AnyProbe
  }

makeLenses 'BasicBrains


cleanupPostponed :: MonadIO m => BasicBrains e -> Hash HbSync -> m ()
cleanupPostponed b h =  do
    let po = view brainsPostponeDown b
    let flt (_,h1) _ = h1 /= h
    liftIO $ atomically $ modifyTVar po $ HashMap.filterWithKey flt

instance ( Hashable (Peer e)
         , Pretty (Peer e), Pretty (PeerAddr e)
         , Pretty (AsBase58 (PubKey 'Sign (Encryption e)))
         , Hashable (PubKey 'Sign (Encryption e))
         , e ~ L4Proto
         , ForRefChans e
         ) => HasBrains e (BasicBrains e) where

  onClientTCPConnected br pa@(L4Address proto _) ssid = do
    debug $ "BRAINS: onClientTCPConnected" <+> pretty proto <+> pretty pa <+> pretty ssid
    updateOP br $ insertClientTCP br pa ssid
    commitNow br False

  getClientTCP br = liftIO (selectClientTCP br)

  setActiveTCPSessions br ssids = do
    trace $ "BRAINS: setActiveTCPSessions" <+> pretty ssids
    updateOP br $ updateTCPSessions br ssids
    commitNow br False

  listTCPPexCandidates = liftIO . selectTCPPexCandidates

  listDownloads = liftIO . selectDownloads

  listPexInfo = liftIO . selectPexInfo

  updatePexInfo b pex = updateOP b $ insertPexInfo b pex

  delDownload br what  = do
    liftIO $ Cache.insert (view brainsRemoved br) what ()
    updateOP br (deleteDownload br what)

  onKnownPeers br ps = do
    trace $ "BRAINS: onKnownPeers" <+> pretty ps
    let tv = view brainsPeers br
    liftIO $ atomically $ writeTVar tv (HashSet.fromList ps)
    updateOP br $ do
      transactional br $ do
        deleteKnownPeers br
        forM_ ps $ \pip -> do
          pa <- toPeerAddr pip
          insertKnownPeer br pa
    commitNow br False

  onBlockSize b p h size = do
    liftIO $ Cache.insert (_brainsSizeCache b) (p,h) size
    updateOP b $ insertSize b p h size

  onBlockDownloadAttempt b peer h = do
    -- trace $ "BRAINS: onBlockDownloadAttempt" <+> pretty peer <+> pretty h
    noPeers <- liftIO $ readTVarIO (view brainsPeers b) <&> List.null
    unless noPeers do
      let cache = view brainsExpire b
      liftIO $ Cache.insert cache h ()
      let doAlter = HashMap.alter (maybe (Just 0) (Just . succ)) (peer,h)
      liftIO $ atomically $ modifyTVar (view brainsPostponeDown b) doAlter
      -- updateOP b $ insertDownload b

  onBlockDownloaded b p h = do
    -- trace $ "BRAINS: onBlockDownloaded" <+> pretty p <+> pretty h
    -- cleanupPostponed b h
    updateOP b do
      insertPeer b h p
      atomically $ writeTQueue (_brainsDelDownload b) h
      -- deleteDownload b (HashRef h)

  onBlockPostponed b h  = do
    -- trace $ "BRAINS: onBlockPostponed" <+> pretty h
    cleanupPostponed b h

  claimBlockCameFrom b (Just f) t = do
    -- trace $ "BRAINS: claimBlockCameFrom" <+> pretty f <+> pretty t
    updateOP b do
      insertAncestor b f t
      insertDownload b (Just $ HashRef f) (HashRef t)

  claimBlockCameFrom b p h = do
    updateOP b do
      insertDownload b (HashRef <$> p) (HashRef h)

  shouldPostponeBlock b h = do
    peers <- liftIO $ readTVarIO (view brainsPeers b) <&> HashSet.toList
    downs <- liftIO $ readTVarIO (view brainsPostponeDown b)
    kicked <- liftIO $ Cache.lookup (view brainsRemoved b) (HashRef h) <&> isJust

    r <- forM peers $ \p -> do
           let v = HashMap.lookup (p,h) downs & fromMaybe 0 & (<simK)
           pure [v]

    let postpone = not (List.null r || or (mconcat r) )

    pure (not kicked && postpone)
    where
      simK = 2

  -- FIXME: investigate-simK-affects-anything
  --   проверить
  shouldDownloadBlock b p h = do
    kicked <- liftIO $ Cache.lookup (view brainsRemoved b) (HashRef h) <&> isJust
    noPeers <- liftIO $ readTVarIO (view brainsPeers b) <&> List.null
    downs <- liftIO $ readTVarIO (view brainsPostponeDown b)
    -- trace $ "shouldDownloadBlock" <+> pretty noPeers <+> pretty doo
    pure $ not kicked && noPeers || (HashMap.lookup (p,h) downs & fromMaybe 0 & (<simK))
    where
      -- TODO: simK-to-settings
      --   в настройки, если на что-либо влияет
      simK = 2

  advisePeersForBlock b h = do
    r <- liftIO $ findPeers b h
    pure $ mapMaybe fromStringMay r

  blockSize b p h = do
    let cs = view brainsSizeCache b
    found <- liftIO $ Cache.lookup cs (p,h)
    maybe (liftIO $ selectBlockSize b p h) (pure . Just) found

  isReflogProcessed b h = do
    liftIO $ selectReflogProcessed b h

  setReflogProcessed b h = do
    updateOP b $ insertReflogProcessed b h

  addPolledRef brains r s i = do

    liftIO $ atomically $ modifyTVar (_brainsPolled brains) (HashSet.insert (r,s))

    updateOP brains $ do
      let conn = view brainsDb brains
      liftIO $ execute conn sql (show $ pretty (AsBase58 r), s, i)

    where
      sql = [qc|
      insert into {poll_table} (ref,type,interval)
      values (?,?,?)
      on conflict do update set interval = excluded.interval
      |]

  delPolledRef brains r = do
    updateOP brains $ do
      let conn = view brainsDb brains
      liftIO $ execute conn sql (Only (show $ pretty (AsBase58 r)))
    where
      sql = [qc|
      delete from {poll_table}
      where ref = ?
      |]

  listPolledRefs brains mtp = do
    liftIO $ do
      let conn = view brainsDb brains
      case mtp of
        Nothing -> postprocess <$>
          query_ conn [qc|select ref, type, interval from {poll_table}|]

        Just tp -> postprocess <$>
          query conn [qc|select ref, type, interval from {poll_table} where type = ?|] (Only tp)
    where
      postprocess = mapMaybe (\(r,t,i) -> (,t,i) <$> fromStringMay r )

  listPolledRefsFiltered brains (t, p)  = liftIO do
    debug $ red "brains: listPolledRefsFiltered" <+> pretty (t,p)
    let conn = view brainsDb brains
    let sql = [qc|
      select ref, type, interval
      from {poll_table}
      where coalesce(type = ?, true)
      limit ?
      offset ?
    |]
    query conn sql (t, lim, off ) <&> postprocess
    where
      postprocess = mapMaybe (\(r,t1,i) -> (,t1,i) <$> fromStringMay r )
      off = maybe 0 fst p
      lim = maybe 1000 snd p

  isPolledRef brains tp ref = do

    cached <- liftIO $ readTVarIO (_brainsPolled brains) <&> HashSet.member (ref,tp)

    if cached then
      pure True
    else do

      r <- liftIO do
              let conn = view brainsDb brains
              query @_ @(Only Int) conn [qc|
                select 1 from {poll_table}
                where ref = ? and type = ?
                limit 1
              |] ( show $ pretty (AsBase58 ref), tp )
                <&> isJust . listToMaybe

      when r do
        liftIO $ atomically $ modifyTVar (_brainsPolled brains) (HashSet.insert (ref,tp))

      pure r

  setSeen brains w ts = do
    utc <- liftIO getCurrentTime <&> addUTCTime ts
    let h = show $ pretty $ hashObject @HbSync w
    liftIO do
      let conn = view brainsDb brains
      void $ execute conn [qc|
        insert into seen (hash,ts)
        values (?,?)
        on conflict (hash) do update set ts = excluded.ts
      |] (h,utc)
    commitNow brains False

  isSeen brains w = do
    let h = show $ pretty $ hashObject @HbSync w
    liftIO do
      let conn = view brainsDb brains
      r <- query @_ @(Only Int) conn [qc|select 1 from seen where hash = ? limit 1|] (Only h)
      pure $ not $ List.null r

  brainsCacheBlockSize b pk ha s  = do
    updateOP b $ do
      let conn = view brainsDb b

      let sql = [qc|
         insert into blocksizecache (block,peer,size)
         values(?,?,?)
         on conflict (block,peer) do update set size = excluded.size
      |]

      void $ execute conn sql (hash,peer,s)

    where
      peer = show $ pretty (AsBase58 pk)
      hash = show $ pretty ha

  brainsFindBlockSize brains pk ha = do
     let conn = view brainsDb brains
     let peer = show $ pretty (AsBase58 pk)
     let hash = show $ pretty ha
     liftIO do
       result <- query @_ @(Only Integer) conn [qc|
         select size
         from blocksizecache
         where block = ? and peer = ?
         limit 1
       |] (hash, peer)
       pure $ fromOnly <$> listToMaybe result

commitNow :: forall e m . MonadIO m
          => BasicBrains e
          -> Bool
          -> m ()

commitNow br doWait = do
  w <- liftIO newTQueueIO

  let answer | doWait = do
        atomically $ writeTQueue w ()
             | otherwise = pure ()

  liftIO $ atomically $ writeTQueue (view brainsCommit br) (CommitCmd answer)

  when doWait $ liftIO do
    void $ atomically $ do
      readTQueue w >> flushTQueue w

updateOP :: forall e m . MonadIO m => BasicBrains e -> IO () -> m ()
updateOP br op = do
  let pip = view brainsPipeline br
  liftIO $ atomically $ writeTQueue pip (liftIO op)

insertSize :: forall e . Pretty (Peer e)
           => BasicBrains e
           -> Peer e
           -> Hash HbSync
           -> Integer
           -> IO ()

insertSize br p h s = do


  let conn = view brainsDb br

  void $ liftIO $ execute conn [qc|
    insert into blocksize (block, peer, size) values (?,?,?)
    on conflict (block,peer) do update set size = ?
  |] (show $ pretty h, show $ pretty p, s, s)


insertClientTCP :: forall e . (Pretty (Peer e), e ~ L4Proto)
                => BasicBrains e
                -> PeerAddr e
                -> Word64
                -> IO ()

-- | only stores TCP address
insertClientTCP br pa@(L4Address TCP (IPAddrPort (h,p))) ssid  = do
  let conn = view brainsDb br
  void $ liftIO $ execute conn [qc|
    insert into tcpclient (peer,ssid,ip,port) values (?,?,?,?)
    on conflict (peer) do update set ssid = excluded.ssid
  |] (show $ pretty pa, ssid, show (pretty h), p)

insertClientTCP _ _ _ = pure ()

selectClientTCP :: BasicBrains L4Proto -> IO [(PeerAddr L4Proto, Word64)]
selectClientTCP br = do
  let conn = view brainsDb br
  rows <- liftIO $ query_ @(String, Word64) conn [qc|
    select peer,ssid from tcpclient limit 200
    |]

  pas <- forM rows $ \(speer,ssid) -> do
           pure $ (,) <$> fromStringMay speer
                      <*> pure ssid

  pure $ catMaybes pas

insertReflogProcessed :: BasicBrains e
                      -> Hash HbSync
                      -> IO ()

insertReflogProcessed br h = do


  let conn = view brainsDb br

  void $ liftIO $ execute conn [qc|
    insert into statedb.processed (hash) values (?)
    on conflict (hash) do nothing
  |] (Only (show $ pretty h))

selectReflogProcessed :: forall e . Pretty (Peer e)
           => BasicBrains e
           -> Hash HbSync
           -> IO Bool
selectReflogProcessed br h  = do

  let conn = view brainsDb br

  liftIO $ query conn [qc|
    select 1
    from statedb.processed
    where hash = ?
    limit 1
  |] (Only (show $ pretty h)) <&> fmap (fromOnly @Int)
                              <&> listToMaybe
                              <&> isJust


selectBlockSize :: forall e . Pretty (Peer e)
           => BasicBrains e
           -> Peer e
           -> Hash HbSync
           -> IO (Maybe Integer)
selectBlockSize br p h  = do

  let conn = view brainsDb br

  liftIO $ query conn [qc|
    select size
    from blocksize
    where
      block = ? and peer = ?
    limit 1
  |] (show $ pretty h, show $ pretty p) <&> fmap fromOnly <&> listToMaybe

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


insertKnownPeer :: forall e . e ~ L4Proto
                => BasicBrains e
                -> PeerAddr e
                -> IO ()

insertKnownPeer br peer@(L4Address _ (IPAddrPort (i,a))) = do
  let conn = view brainsDb br
  void $ liftIO $ execute conn [qc|
    INSERT INTO knownpeer (peer,ip,port)
    VALUES (?,?,?)
    ON CONFLICT (peer)
    DO NOTHING
  |] (show $ pretty peer, show (pretty i), a)


deleteKnownPeers :: forall e . e ~ L4Proto
                 => BasicBrains e
                 -> IO ()

deleteKnownPeers br = do
  let conn = view brainsDb br
  void $ liftIO $ execute_ conn [qc|
    DELETE FROM knownpeer;
  |]

selectKnownPeers :: forall e . e ~ L4Proto
                 => BasicBrains e
                 -> IO [PeerAddr e]  -- ^ list of peers

selectKnownPeers br = do
  let conn = view brainsDb br
  liftIO $ query_ conn [qc|SELECT peer FROM knownpeer|]
   <&> fmap (fromStringMay . fromOnly)
   <&> catMaybes


selectTCPPexCandidates :: forall e . e ~ L4Proto
                       => BasicBrains e
                       -> IO [PeerAddr e]  -- ^ list of peers

selectTCPPexCandidates br = do
  let conn = view brainsDb br
  liftIO $ query_ conn
    [qc| SELECT distinct(cl.peer)
         FROM tcpclient cl JOIN knownpeer p on p.ip = cl.ip
    |] <&> fmap (fromStringMay . fromOnly)
       <&> catMaybes

updateTCPSessions :: forall e . e ~ L4Proto
                 => BasicBrains e
                 -> [(PeerAddr e, Word64)]
                 -> IO ()

updateTCPSessions br ssids = do
  let conn = view brainsDb br
  let sss = fmap (over _1 (show . pretty) . ip) ssids
  transactional br $ do
    void $ liftIO $ execute_ conn [qc|DELETE FROM tcpsession|]
    void $ liftIO $ executeMany conn [qc|
      INSERT INTO tcpsession (peer, ssid, ip, port)
      VALUES (?, ?, ?, ?)
      ON CONFLICT (ssid)
      DO UPDATE SET
          peer = excluded.peer,
          ip = excluded.ip,
          port = excluded.port
    |] sss

  where
    ip (a@(L4Address _ (IPAddrPort (i,p))), s) = (a,s,show $ pretty i,p)

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


delAllDownloads :: BasicBrains e  -> IO ()
delAllDownloads brains = do

  let conn = view brainsDb brains

  let sql = [qc|
DELETE FROM statedb.download;
  |]

  void $ try @SomeException $ liftIO $ execute_ conn sql

cleanupHashes :: BasicBrains e
              -> IO ()

cleanupHashes br  = do

  debug "BRAINS: cleanup caches"

  let conn = view brainsDb br

  let sql = [qc|
SAVEPOINT zzz1;

DELETE FROM ancestors WHERE strftime('%s','now') - strftime('%s', ts) > 600;
DELETE FROM seenby WHERE strftime('%s','now') - strftime('%s', ts) > 600;
DELETE FROM blocksize WHERE strftime('%s','now') - strftime('%s', ts) > (86400*7);
DELETE FROM blocksizecache WHERE strftime('%s','now') - strftime('%s', ts) > (86400*7);
DELETE FROM statedb.pexinfo where seen <  datetime('now', '-7 days');
DELETE FROM seen where ts < datetime('now');

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

    Left ( e :: SomeException ) -> do
      err $ "BRAINS: " <+> viaShow e
      execute_ conn [qc|ROLLBACK TO SAVEPOINT {sp}|]

---

insertPeerAsymmKey :: forall e m . (e ~ L4Proto, MonadIO m)
    => BasicBrains e
    -> Peer e
    -> Encrypt.PublicKey
    -> Encrypt.CombinedKey
    -> m ()

insertPeerAsymmKey br peer hAsymmKey hSymmKey = do
    insertPeerAsymmKey br peer hAsymmKey hSymmKey
    insertPeerAsymmKey' br (show $ pretty peer) hAsymmKey hSymmKey

insertPeerAsymmKey' :: forall e m . (e ~ L4Proto, MonadIO m)
    => BasicBrains e
    -> String
    -> Encrypt.PublicKey
    -> Encrypt.CombinedKey
    -> m ()

insertPeerAsymmKey' br key hAsymmKey hSymmKey = do
  let conn = view brainsDb br
  void $ liftIO $ execute conn [qc|
        INSERT INTO peer_asymmkey (peer,asymmkey,symmkey)
        VALUES (?,?,?)
        ON CONFLICT (peer)
        DO UPDATE SET
          asymmkey = excluded.asymmkey
        , symmkey = excluded.symmkey
    |] (key, show hAsymmKey, show hSymmKey)

---

deletePeerAsymmKey :: forall e m . (e ~ L4Proto, MonadIO m)
    => BasicBrains e -> Peer e -> m ()

deletePeerAsymmKey br peer =
    deletePeerAsymmKey' br (show $ pretty peer)

deletePeerAsymmKey' :: forall e m . (e ~ L4Proto, MonadIO m)
    => BasicBrains e -> String -> m ()

deletePeerAsymmKey' br key =
    void $ liftIO $ execute (view brainsDb br) [qc|
        DELETE FROM peer_asymmkey
        WHERE peer = ?
    |] (Only key)


insertDownload :: forall e  . ( e ~ L4Proto)
               => BasicBrains e
               -> Maybe HashRef
               -> HashRef
               -> IO ()
insertDownload br parent hash = do
  let conn = view brainsDb br
  liftIO $ execute conn [qc|
    insert into statedb.download (hash, parent)
    values (?, ?)
    on conflict (hash) do update set parent = excluded.parent
  |] (hash, parent)


deleteDownload :: forall e . (e ~ L4Proto)
               => BasicBrains e
               -> HashRef
               -> IO ()
deleteDownload br hash = do
  let conn = view brainsDb br
  liftIO $ execute conn [qc|
    delete from statedb.download where hash = ?
  |] (Only hash)



selectDownloads  :: forall e . (e ~ L4Proto)
                 => BasicBrains e
                 -> IO [(HashRef, Integer)]
selectDownloads br = do
  let conn = view brainsDb br
  liftIO $ query_ conn [qc|
    select hash, ts from statedb.download
  |]

---

insertPexInfo :: forall e  . ( e ~ L4Proto)
               => BasicBrains e
               -> [PeerAddr e]
               -> IO ()
insertPexInfo br peers = liftIO do
  let conn = view brainsDb br
  forM_ peers $ \p -> do
    execute conn [qc|
    insert into statedb.pexinfo (peer)
    values(?)
    on conflict (peer)
      do update set seen =  datetime('now','localtime')
    |] (Only (show $ pretty p))


{- HLINT ignore "Functor law" -}

selectPexInfo :: forall e . (e ~ L4Proto)
              => BasicBrains e
              -> IO [PeerAddr e]
selectPexInfo br = liftIO do
  let conn = view brainsDb br
  query_ conn [qc|
    select peer from statedb.pexinfo where seen >= datetime('now', '-7 days')
    order by seen desc
    limit 100
  |] <&> fmap (fromStringMay . fromOnly)
     <&> catMaybes

tableExists :: Connection -> Maybe String -> String -> IO Bool
tableExists conn prefix' tableName = do
  let sql = [qc|
     SELECT name FROM {prefix}.sqlite_master WHERE type='table' AND name=?
  |]
  r <- query conn sql (Only tableName) :: IO [Only String]
  pure $ not $ null r

  where
    prefix = fromMaybe "main" prefix'


type ForBasicBrains e = (Hashable (Peer e), Hashable (PubKey 'Sign (Encryption e)))

basicBrainsSetProbe :: forall e m . (MonadIO m, ForBasicBrains e)
                    => BasicBrains e
                    -> AnyProbe
                    -> m ()
basicBrainsSetProbe BasicBrains{..} p = do
  liftIO $ atomically $ writeTVar _brainsProbe p

-- FIXME: eventually-close-db
newBasicBrains :: forall e m . ( ForBasicBrains e
                               , MonadIO m
                               )
               => PeerConfig
               -> m (BasicBrains e)

newBasicBrains cfg = liftIO do

  stateDbFile <- runReaderT (cfgValue @PeerBrainsDBPath @(Maybe String)) cfg

  stateDb <- maybe (peerStateDirDefault <&> (</> "brains.db")) pure stateDbFile

  liftIO $ createDirectoryIfMissing True (takeDirectory stateDb)

  brains <- runReaderT (cfgValue @PeerBrainsDb @(Maybe String)) cfg
              <&> fromMaybe ":memory:"

  -- unless ( brains == ":memory:" ) do
  --   here <- doesFileExist brains
    -- when here $ do removeFile brains

  conn <- open brains

  debug $ "BRAINS:" <+> "state" <+> pretty stateDb

  execute_ conn [qc|ATTACH DATABASE '{stateDb}' as statedb|]

  execute_ conn [qc|
  create table if not exists statedb.processed ( hash text not null primary key );
  |]

  execute_ conn [qc|
  create table if not exists statedb.download
    ( hash    text not null primary key
    , parent  text null
    , ts INTEGER DEFAULT (strftime('%s','now'))
    );
  |]

  execute_ conn [qc|
  create table if not exists statedb.pexinfo
    ( peer    text not null primary key
    , seen    DATE DEFAULT (datetime('now','localtime'))
    );
  |]

  execute_ conn [qc|
    create table if not exists ancestors
    ( child text not null
    , parent text not null
    , ts DATE DEFAULT (datetime('now','localtime'))
    , primary key (child,parent))
  |]

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

  execute_ conn [qc|
    create table if not exists blocksize
    ( block text not null
    , peer text not null
    , size int
    , ts DATE DEFAULT (datetime('now','localtime'))
    , primary key (block,peer))
  |]

  execute_ conn [qc|
    create table if not exists blocksizecache
    ( block text not null
    , peer text not null
    , size int
    , ts DATE DEFAULT (datetime('now','localtime'))
    , primary key (block,peer))
  |]

  execute_ conn [qc|
    create table if not exists tcpclient
    ( peer text not null
    , ssid unsigned big int not null
    , ip text not null
    , port int not null
    , primary key (peer) )
  |]

  execute_ conn [qc|
    create table if not exists knownpeer
    ( peer text not null
    , ip text not null
    , port int not null
    , primary key (peer)
    )
  |]

  execute_ conn [qc|
    create table if not exists tcpsession
    ( ssid unsigned bin int not null
    , peer text not null
    , ip text not null
    , port int not null
    , primary key (ssid)
    )
  |]

  poll_1 <- tableExists conn (Just "statedb") "poll_1"
  poll_0 <- tableExists conn (Just "statedb") "poll"

  unless poll_1 do
    debug $ red "BRAINS: CREATE poll_1"
    execute_ conn [qc|
      create table if not exists statedb.poll_1
      ( ref      text not null
      , type     text not null
      , interval int not null
      , primary key (ref,type)
      )
    |]

    when poll_0 do
      debug $ red "BRAINS: FILL poll_1"
      execute_ conn [qc|
        insert into statedb.poll_1 (ref,type,interval)
        select ref,type,interval from statedb.poll;
      |]

  execute_ conn [qc|
    create table if not exists peer_asymmkey
    ( peer text not null
    , asymmkey text not null
    , symmkey text not null
    , ts DATE DEFAULT (datetime('now','localtime'))
    , primary key (peer)
    )
  |]

  execute_ conn [qc|
    create table if not exists seen
    ( hash text not null
    , ts not null
    , primary key (hash)
    )
  |]

  BasicBrains <$> newTVarIO mempty
              <*> newTVarIO mempty
              <*> Cache.newCache (Just (toTimeSpec (30 :: Timeout 'Seconds)))
              <*> Cache.newCache (Just (toTimeSpec (60 :: Timeout 'Seconds)))
              <*> pure conn
              <*> newTQueueIO
              <*> newTQueueIO
              <*> newTQueueIO
              <*> Cache.newCache (Just (toTimeSpec (600:: Timeout 'Seconds)))
              <*> newTVarIO mempty
              <*> newTVarIO (AnyProbe ())

data PeerDownloadsDelOnStart

instance HasCfgKey PeerDownloadsDelOnStart b where
  key = "downloads-del-on-start"

{- HLINT ignore "Use camelCase" -}
poll_table :: String
poll_table = "statedb.poll_1"

runBasicBrains :: forall e m . ( e ~ L4Proto
                               , MonadUnliftIO m
                               , ForRefChans e
                               , Pretty (AsBase58 (PubKey 'Sign (Encryption L4Proto)))
                               )
               =>  PeerConfig
               -> BasicBrains e
               -> m ()

runBasicBrains cfg brains@BasicBrains{..} = do

  let pip = view brainsPipeline brains
  let expire = view brainsExpire brains
  let sizes = view brainsSizeCache brains
  let commit = view brainsCommit brains

  void $ flip runContT pure do

    -- FIXME: async-error-handling
    void $ ContT $ withAsync $ forever $ liftIO do

      ewaiters <- race (pause @'Seconds 10) $ do
        atomically $ do
          c  <- readTQueue commit
          cs <- flushTQueue commit
          pure (c:cs)

      let waiters = fromRight mempty ewaiters & fmap onCommited

      w  <- atomically $ readTQueue pip
      ws <- atomically $ flushTQueue pip

      transactional brains (sequence_ (w:ws))
      sequence_ waiters

    void $ ContT $ withAsync $ forever do
      pause @'Seconds 10
      p <- liftIO $ readTVarIO _brainsProbe
      acceptReport p =<< S.toList_ do
        S.yield =<< liftIO (readTVarIO _brainsPeers <&> ("brainsPeers",) . fromIntegral . length)
        S.yield =<< liftIO (readTVarIO _brainsPostponeDown <&> ("brainsPostponeDown",) . fromIntegral . HashMap.size)
        S.yield =<< liftIO (Cache.size _brainsExpire <&> ("brainsExpire",) . fromIntegral)
        S.yield =<< liftIO (Cache.size _brainsRemoved <&> ("brainsRemoved",) . fromIntegral)
        S.yield =<< liftIO (Cache.size _brainsSizeCache <&> ("brainsSizeCache",) . fromIntegral)
        S.yield =<< liftIO (readTVarIO _brainsPolled <&> ("brainsPolled",) . fromIntegral . HashSet.size)

    void $ ContT $ withAsync do
      forever do
        pause @'Seconds 60
        del <- liftIO $ atomically $ flushTQueue _brainsDelDownload

        updateOP brains (cleanupHashes brains)

        for_ del $ \d -> do
          delDownload @e brains (HashRef d)

    trace "runBasicBrains init"

    let (PeerConfig syn) = cfg

    let delDowns = runReader (cfgValue @PeerDownloadsDelOnStart) cfg :: FeatureSwitch

    when (delDowns == FeatureOn ) do
      debug $ yellow "CLEAN ALL DOWNLOADS"
      updateOP brains (delAllDownloads brains)
      commitNow brains False

    let polls = catMaybes (
         [ (tp,n,) <$> fromStringMay @(PubKey 'Sign (Encryption e)) (Text.unpack ref)
         | ListVal (Key "poll" [SymbolVal tp, LitIntVal n, LitStrVal ref]) <- syn
         ] )

    void $ ContT $ withAsync $ do
      pause @'Seconds 10
      forM_ polls $ \(t,mi,x) -> do
        trace $ "BRAINS: poll" <+> pretty t <+> pretty (AsBase58 x) <+> pretty mi
        updateOP brains $ do
          let conn = view brainsDb brains
          liftIO $ execute conn [qc|
            insert into {poll_table} (ref,type,interval)
            values (?,?,?)
            on conflict do update set interval = excluded.interval
            |] (show $ pretty (AsBase58 x), show $ pretty t, mi)
        -- commitNow brains True

    void $ forever do
      pause @'Seconds 30
      ee <- liftIO $ Cache.toList expire
      let eee = [ h | (h,_,Just{}) <- ee ]
      forM_ eee $ \h -> do
        cleanupPostponed brains h

      liftIO do
        Cache.purgeExpired expire
        Cache.purgeExpired sizes
        Cache.purgeExpired _brainsRemoved

      del <- liftIO $ atomically $ flushTQueue _brainsDelDownload
      for_ del $ \d -> do
        delDownload @e brains (HashRef d)






