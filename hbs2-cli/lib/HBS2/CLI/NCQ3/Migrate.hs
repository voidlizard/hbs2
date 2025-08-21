{-# Language AllowAmbiguousTypes #-}
module HBS2.CLI.NCQ3.Migrate where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58
import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Net.Auth.Schema()
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix
import HBS2.Storage
import HBS2.Storage.NCQ3
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.Fossil
import HBS2.Storage.NCQ3.Internal.Index
import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Peer.Proto.RefLog
import HBS2.Peer.Proto.RefChan
import HBS2.Peer.Proto.LWWRef

import Data.Config.Suckless.System

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict (HashMap)
import Data.Either
import Data.List qualified as L
import System.Directory (getModificationTime)
import UnliftIO.IO

import Streaming.Prelude qualified as S

{- HLINT ignore "Functor law"-}

data SomeRef = forall a . ( Eq a
                          , Hashable a
                          , Hashed HbSync a
                          , Pretty a
                          , RefMetaData a
                          )
             => SomeRef a

instance RefMetaData SomeRef where
  refMetaData (SomeRef a) = refMetaData a

instance Pretty SomeRef where
  pretty (SomeRef wtf) = pretty wtf

instance Hashed HbSync SomeRef where
  hashObject (SomeRef a) = hashObject a

legacyHashRefNCQ1 :: HashRef -> HashRef -> HashRef
legacyHashRefNCQ1 salt h = HashRef (hashObject (coerce @_ @ByteString h <> coerce salt))

legacyHashRefFor :: Hashed HbSync p => HashRef -> p -> HashRef
legacyHashRefFor salt x = legacyHashRefNCQ1 salt (HashRef $ hashObject @HbSync $ x)

migrateEntries :: forall c m . ( MonadUnliftIO m
                               , IsContext c
                               , Exception (BadFormException c)
                               , HasClientAPI PeerAPI UNIX m
                               , HasStorage m
                               ) => MakeDictM c m ()
migrateEntries = do
  brief "migrate NCQv1 => NCQ3"
   $ args [ arg "path" "src"
          , arg "path" "dst"
          ]

   $ entry $ bindMatch "ncq3:migrate:ncq" $ nil_ $ \case
       [ StringLike src, StringLike dst] -> do

        sto <- getStorage



        api <- getClientAPI @PeerAPI  @UNIX

        refs <- callRpcWaitMay @RpcPollList2 (1.0 :: Timeout 'Seconds) api (Nothing, Nothing)
                  <&> fromMaybe mempty

        rrefs <- S.toList_ <$> for refs $ \(pk, s, _) -> case s of
                   "reflog"  -> S.yield (SomeRef $ RefLogKey @'HBS2Basic pk)
                   "refchan" -> S.yield (SomeRef $ RefChanLogKey @'HBS2Basic pk)
                   "lwwref"  -> S.yield (SomeRef $ LWWRefKey @'HBS2Basic pk)
                   _         -> none

        lift $ migrateNCQ1 nicelog rrefs src dst

       e -> throwIO $ BadFormException (mkList e)

nicelog :: forall m . MonadIO  m => Doc AnsiStyle -> m ()
nicelog doc = liftIO $ hPutDoc stdout (doc <> line)

migrateNCQ1 :: MonadUnliftIO m
            => ( Doc AnsiStyle -> IO () )
            -> [SomeRef]
            -> FilePath
            -> FilePath
            -> m ()
migrateNCQ1 logger refs src b = flip runContT pure do
  debug "migrate NCQ to NCQ3"

  let a0 = src </> "0"
  a0here <- doesPathExist a0

  let a = if a0here then a0 else src

  ncq <- ContT $ ncqWithStorage b

  let sto = AnyStorage ncq

  let mlog = ncqGetFileName ncq "migrate-ncq1.log"

  info $ "set SOURCE dir" <+> pretty a

  ncqSalt <- liftIO (try @_ @IOException (BS.readFile (src </> ".seed")))
               <&> fromRight mempty
               <&> HashRef . hashObject

  notice $ "ref. salt" <+> pretty ncqSalt

  files <- do
    fs <- dirFiles a <&> L.filter (\x -> takeExtension x == ".data")
    mtimes <- liftIO $ mapM getModificationTime fs
    pure [ f | (f,_) <- L.sortOn snd (zip fs mtimes) ]

  touch mlog

  processed <- HS.fromList . lines <$> liftIO (readFile mlog)

  let refz = HM.fromList [ (legacyHashRefFor ncqSalt r, SomeRef r) | SomeRef r <- refs ]

  rv <- newTVarIO ( mempty :: HashMap HashRef (SomeRef, HashRef) )
  nblk <- newTVarIO 0

  for_ files $ \f -> void $ runMaybeT do

    guard ( not $ HS.member f processed )

    debug $ "processing" <+> pretty f

    ncqStorageScanDataFile0 f $ \offset w key s -> void $ runMaybeT do
      (t, bs) <- toMPlus (ncqEntryUnwrapValue s)

      case t of
        B -> do
          h1 <- putBlock sto (LBS.fromStrict bs)
          atomically $ modifyTVar nblk succ

        R -> do
          case HM.lookup key refz of
            Nothing -> none
            Just r -> do
              atomically $ modifyTVar rv (HM.insert (HashRef $ hashObject r) (r, HashRef (coerce bs)))

        _ -> none


    liftIO $ appendFile mlog (f <> "\n")

  nb <- readTVarIO nblk
  liftIO $ logger $ "moved" <+> pretty nb <+> "blocks"

  foundRefs <- readTVarIO rv <&> HM.toList
  for_ foundRefs $ \(_, (r, v)) -> do
    blk <- hasBlock sto (coerce v)
    when (isJust blk) do
      liftIO $ logger $ green "update ref" <+> pretty r <+> pretty v
      updateRef sto r (coerce v)

  -- for_ refs $ \(SomeRef r) -> void $ runMaybeT do
  --   v  <- getRef origSto r >>= toMPlus
  --   v0 <- hasBlock origSto v
  --   v1 <- hasBlock sto v
  --   updateRef sto r v
  --   v3 <- getRef sto r
  --   let okay = if v3  == Just v then green "ok" else red "fail"
  --   liftIO $ logger $ okay <+> "update ref" <+> pretty r <+> pretty v <+> pretty v1 <+> parens (pretty v0)


