{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language TemplateHaskell #-}
{-# Language TypeOperators #-}
module Brains
  ( module Brains
  , module HBS2.Peer.Brains
  ) where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Net.Proto.RefChan(ForRefChans)
import HBS2.Net.Proto
import HBS2.Hash
import HBS2.Base58
import HBS2.Net.IP.Addr

import HBS2.Peer.Brains
import HBS2.System.Logger.Simple

import PeerConfig

import Crypto.Saltine.Core.Box qualified as Encrypt
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import Control.Concurrent.STM
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Either
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Maybe
import Data.Text qualified as Text
import Data.Word
import Lens.Micro.Platform
import System.Directory
import System.FilePath
import System.Random (randomRIO)
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO (MonadUnliftIO(..),async,race)

data PeerBrainsDb

instance Monad m => HasCfgKey PeerBrainsDb (Maybe String) m where
  key = "brains"

newtype CommitCmd = CommitCmd { onCommited :: IO () }

data BasicBrains e =
  BasicBrains
  { _brainsPeers        :: TVar [Peer e]
  , _brainsPostponeDown :: TVar (HashMap (Peer e, Hash HbSync) Int )
  , _brainsExpire       :: Cache (Hash HbSync) ()
  , _brainsDb           :: Connection
  , _brainsPipeline     :: TQueue (IO ())
  , _brainsCommit       :: TQueue CommitCmd
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
         , e ~ L4Proto
         , ForRefChans e
         ) => HasBrains e (BasicBrains e) where

  onClientTCPConnected br pa@(L4Address proto _) ssid = do
    debug $ "BRAINS: onClientTCPConnected" <+> pretty proto <+> pretty pa <+> pretty ssid
    updateOP br $ insertClientTCP br pa ssid
    commitNow br True

  getClientTCP br = liftIO (selectClientTCP br)

  setActiveTCPSessions br ssids = do
    trace $ "BRAINS: setActiveTCPSessions" <+> pretty ssids
    updateOP br $ updateTCPSessions br ssids
    commitNow br True

  listTCPPexCandidates = liftIO . selectTCPPexCandidates

  onKnownPeers br ps = do
    trace $ "BRAINS: onKnownPeers" <+> pretty ps
    let tv = view brainsPeers br
    liftIO $ atomically $ writeTVar tv ps
    updateOP br $ do
      transactional br $ do
        deleteKnownPeers br
        forM_ ps $ \pip -> do
          pa <- toPeerAddr pip
          insertKnownPeer br pa
    commitNow br True

  onBlockSize b p h size = do
    updateOP b $ insertSize b p h size
    commitNow b True
    -- FIXME: wait-till-really-commited
    sz <- liftIO $ selectBlockSize b p h
    -- trace $ "BRAINS: onBlockSize" <+> pretty p <+> pretty h <+> pretty sz
    pure ()

  onBlockDownloadAttempt b peer h = do
    -- trace $ "BRAINS: onBlockDownloadAttempt" <+> pretty peer <+> pretty h
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
    -- trace $ "BRAINS: onBlockPostponed" <+> pretty h
    cleanupPostponed b h

  claimBlockCameFrom b f t = do
    -- trace $ "BRAINS: claimBlockCameFrom" <+> pretty f <+> pretty t
    updateOP b $ insertAncestor b f t

  shouldPostponeBlock b h = do
    peers <- liftIO $ readTVarIO (view brainsPeers b)
    downs <- liftIO $ readTVarIO (view brainsPostponeDown b)

    r <- forM peers $ \p -> do
           let v = HashMap.lookup (p,h) downs & fromMaybe 0 & (<4)
           pure [v]

    let postpone = not (List.null r || or (mconcat r) )

    pure postpone

  shouldDownloadBlock b p h = do
    noPeers <- liftIO $ readTVarIO (view brainsPeers b) <&> List.null
    downs <- liftIO $ readTVarIO (view brainsPostponeDown b)
    let doo = HashMap.lookup (p,h) downs & fromMaybe 0 & (<4)
    -- trace $ "shouldDownloadBlock" <+> pretty noPeers <+> pretty doo
    pure $ noPeers || (HashMap.lookup (p,h) downs & fromMaybe 0 & (<4))

  advisePeersForBlock b h = do
    r <- liftIO $ findPeers b h
    pure $ mapMaybe fromStringMay r

  blockSize b p h = do
    liftIO $ selectBlockSize b p h

  isReflogProcessed b h = do
    liftIO $ selectReflogProcessed b h

  setReflogProcessed b h = do
    updateOP b $ insertReflogProcessed b h

  addPolledRef brains r s i = do

    updateOP brains $ do
      let conn = view brainsDb brains
      liftIO $ execute conn sql (show $ pretty (AsBase58 r), s, i)
    where
      sql = [qc|
      insert into statedb.poll (ref,type,interval)
      values (?,?,?)
      on conflict do update set interval = excluded.interval
      |]

  delPolledRef brains r = do
    updateOP brains $ do
      let conn = view brainsDb brains
      liftIO $ execute conn sql (Only (show $ pretty (AsBase58 r)))
    where
      sql = [qc|
      delete from statedb.poll
      where ref = ?
      |]

  listPolledRefs brains mtp = do
    liftIO $ do
      let conn = view brainsDb brains
      case mtp of
        Nothing -> postprocess <$>
          query_ conn [qc|select ref, type, interval from statedb.poll|]

        Just tp -> postprocess <$>
          query conn [qc|select ref, type, interval from statedb.poll where type = ?|] (Only tp)
    where
      postprocess = mapMaybe (\(r,t,i) -> (,t,i) <$> fromStringMay r )

  isPolledRef brains ref = do
    liftIO do
      let conn = view brainsDb brains
      query @_ @(Only Int) conn [qc|
        select 1 from statedb.poll
        where ref = ?
        limit 1
      |] ( Only ( show $ pretty (AsBase58 ref) ) )
        <&> isJust . listToMaybe

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


cleanupHashes :: BasicBrains e
              -> IO ()

cleanupHashes br  = do

  debug "BRAINS: cleanup caches"

  let conn = view brainsDb br

  let sql = [qc|
SAVEPOINT zzz1;

DELETE FROM ancestors WHERE strftime('%s','now') - strftime('%s', ts) > 600;
DELETE FROM seenby WHERE strftime('%s','now') - strftime('%s', ts) > 600;
DELETE FROM blocksize WHERE strftime('%s','now') - strftime('%s', ts) > 300;

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

---

-- FIXME: eventually-close-db
newBasicBrains :: forall e m . (Hashable (Peer e), MonadIO m)
               => PeerConfig
               -> m (BasicBrains e)

newBasicBrains cfg = liftIO do

  sdir <- peerStateDirDefault

  liftIO $ createDirectoryIfMissing True sdir

  let stateDb = sdir </> "brains.db"

  brains <- runReaderT (cfgValue @PeerBrainsDb @(Maybe String)) cfg
              <&> fromMaybe ":memory:"

  unless ( brains == ":memory:" ) do
    here <- doesFileExist brains
    when here $ do removeFile brains

  conn <- open brains

  debug $ "BRAINS:" <+> "state" <+> pretty stateDb

  execute_ conn [qc|ATTACH DATABASE '{stateDb}' as statedb|]

  execute_ conn [qc|
  create table if not exists statedb.processed ( hash text not null primary key );
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

  execute_ conn [qc|
    create table if not exists statedb.poll
    ( ref      text not null
    , type     text not null
    , interval int not null
    , primary key (ref)
    )
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

  BasicBrains <$> newTVarIO mempty
              <*> newTVarIO mempty
              <*> Cache.newCache (Just (toTimeSpec (30 :: Timeout 'Seconds)))
              <*> pure conn
              <*> newTQueueIO
              <*> newTQueueIO

runBasicBrains :: forall e m . ( e ~ L4Proto
                               , MonadUnliftIO m
                               , ForRefChans e
                               , Pretty (AsBase58 (PubKey 'Sign (Encryption L4Proto)))
                               )
               =>  PeerConfig
               -> BasicBrains e
               -> m ()

runBasicBrains cfg brains = do

  let pip = view brainsPipeline brains
  let expire = view brainsExpire brains
  let commit = view brainsCommit brains

  -- FIXME: async-error-handling
  void $ liftIO $ async $ forever do

    ewaiters <- race (pause @'Seconds 5) $ do
      atomically $ do
        c  <- readTQueue commit
        cs <- flushTQueue commit
        pure (c:cs)

    let waiters = fromRight mempty ewaiters & fmap onCommited

    w  <- atomically $ readTQueue pip
    ws <- atomically $ flushTQueue pip

    transactional brains (sequence_ (w:ws))
    sequence_ waiters

  void $ liftIO $ async $ forever do
    pause @'Seconds 60
    updateOP brains (cleanupHashes brains)

  trace "runBasicBrains init"

  let (PeerConfig syn) = cfg
  let polls = catMaybes (
       [ (tp,n,) <$> fromStringMay @(PubKey 'Sign (Encryption e)) (Text.unpack ref)
       | ListVal (Key "poll" [SymbolVal tp, LitIntVal n, LitStrVal ref]) <- syn
       ] )

  void $ async $ do
    -- pause @'Seconds 5
    forM_ polls $ \(t,mi,x) -> do
      trace $ "BRAINS: poll" <+> pretty t <+> pretty (AsBase58 x) <+> pretty mi
      updateOP brains $ do
        let conn = view brainsDb brains
        liftIO $ execute conn [qc|
          insert into statedb.poll (ref,type,interval)
          values (?,?,?)
          on conflict do update set interval = excluded.interval
          |] (show $ pretty (AsBase58 x), show $ pretty t, mi)
      commitNow brains True

  void $ forever do
    pause @'Seconds 15
    ee <- liftIO $ Cache.toList expire
    let eee = [ h | (h,_,Just{}) <- ee ]
    forM_ eee $ \h -> do
      cleanupPostponed brains h
    liftIO $ Cache.purgeExpired expire


