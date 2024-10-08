module RefChanQBLF.App where

import Codec.Serialise
import Control.Monad.Cont
import Control.Monad.Trans.Maybe
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Cache qualified as Cache
import Data.HashSet qualified as HashSet
import HBS2.Actors.Peer
import HBS2.Actors.Peer.Types ()
import HBS2.Clock
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Defaults
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.QBLF
import HBS2.Net.Proto.Service
import HBS2.OrDie
import HBS2.Peer.Proto.RefChan
import HBS2.Prelude
import HBS2.Storage.Simple
import HBS2.System.Logger.Simple
import Lens.Micro.Platform hiding ((.=))
import System.Directory

import RefChanQBLF.Common
import RefChanQBLF.Impl
import RefChanQBLF.RPCServer
import RefChanQBLF.Transactions

data QBLFAppConf = QBLFAppConf
    { qapActorKeyring :: FilePath
    , qapRefchanID :: RefChanId L4Proto
    , qapSocket :: FilePath
    , qapAppSocket :: FilePath
    , qapDefState :: Maybe (Hash HbSync)
    , qapStateRef :: MyRefKey
    }

withSimpleAnyStorage :: FilePath -> (AnyStorage -> IO r) -> IO r
withSimpleAnyStorage storepath go = do
    -- FIXME: fix-default-storage
    xdg <- getXdgDirectory XdgData storepath <&> fromString
    sto' <- simpleStorageInit @HbSync [StoragePrefix xdg]
    flip runContT go do
        replicateM 4 $ contAsync $ simpleStorageWorker sto'
        pure $ AnyStorage sto'

loadCreds :: FilePath -> IO (PeerCredentials 'HBS2Basic)
loadCreds fpath = do
    bs <- BS.readFile fpath
    pure (parseCredentials @'HBS2Basic (AsCredFile bs)) `orDie` "bad keyring file"

runQBLFApp :: (ForConsensus IO) => QBLFAppConf -> IO ()
runQBLFApp QBLFAppConf {..} = withLogging do
    creds <- loadCreds qapActorKeyring

    whenM (doesFileExist qapSocket) $ removeFile qapSocket

    -- FIXME: fix-hardcoded-timeout
    fetches <- Cache.newCache (Just (toTimeSpec (TimeoutSec 30)))

    flip runContT pure do
        sto <- ContT $ withSimpleAnyStorage defStorePath

        server <- newMessagingUnixOpts [MUNoFork] True 1.0 qapSocket
        contAsync $ runMessagingUnix server

        s0 <- lift $ readOrCreateStateRef qapDefState sto qapStateRef
        debug $ "STATE0:" <+> pretty s0

        let myEnv =
                MyEnv
                    { mySelf = fromString qapSocket -- Peer UNIX
                    , myFab = (Fabriq server) -- Fabriq UNIX
                    , myChan = qapRefchanID -- RefChanId UNIX
                    , myRef = qapStateRef -- MyRefKey
                    , mySto = sto -- AnyStorage
                    , myCred = creds -- PeerCredentials 'HBS2Basic
                    -- , myAppSoPath = appso                   -- TODO ?
                    , myFetch = fetches -- Cache HashRef ()
                    }

        lift $ runMyAppT myEnv do
            -- FIXME: timeout-hardcode
            let w = realToFrac 5

            -- получить голову
            -- из головы получить акторов
            headBlk <-
                getRefChanHead @L4Proto sto (RefChanHeadKey qapRefchanID)
                    `orDie` "can't read head block"

            -- FIXME: use-actors-asap
            let self = Actor $ view peerSignPk creds
            let actors = fmap Actor $ HashSet.toList $ view refChanHeadAuthors headBlk
            qblf <- qblfInit @ConsensusQBLF self actors (DAppState (HashRef s0)) w

            flip runContT pure do
                contAsync do
                    pause @'Seconds 0.5
                    qblfRun qblf

                do
                    srv <- liftIO $ newMessagingUnix True 1.0 qapAppSocket
                    contAsync $ runMessagingUnix srv
                    let qenv =
                            QRPCEnv
                                { qrpcenvQConsensus = qblf
                                , qrpcenvRefchanId = qapRefchanID
                                , qrpcenvFabriq = Fabriq srv
                                , qrpcenvOwnPeer = fromString qapAppSocket
                                }

                    contAsync $ liftIO $ runQRPCT qenv do
                        runProto @UNIX
                            [ makeResponse (makeServer @QBLFAppRPC)
                            ]

                lift $ runProto [makeResponse (myProto myEnv qblf qapRefchanID)]
  where
    myProto
        :: forall e m
         . ( MonadIO m
           , Request e (RefChanNotify e) m
           , e ~ UNIX
           )
        => MyEnv
        -> QBLF ConsensusQBLF
        -> RefChanId e
        -> RefChanNotify e
        -> m ()

    myProto _ _qblf _ (ActionRequest {}) = do
        pure ()
    myProto env qblf _chan (Notify _ msg) = do
        void $ runMaybeT do
            (_, wrapped) <- MaybeT $ pure $ unboxSignedBox0 msg
            qbmess <-
                MaybeT $
                    pure $
                        deserialiseOrFail @(QBLFMessage ConsensusQBLF) (LBS.fromStrict wrapped)
                            & either (const Nothing) Just

            states <- case qbmess of
                QBLFMsgAnn _ (QBLFAnnounce s0 s1) -> do
                    pure [s0, s1]
                QBLFMsgHeartBeat _ _ s0 _ -> do
                    pure [s0]
                _ -> do
                    pure mempty

            -- FIXME: full-download-guarantee
            lift $ forM_ states (fetchMissed env)

            qblfAcceptMessage qblf qbmess
    -- debug $ "RefChanQBLFMain(3)" <+> "got message" <+> pretty (AsBase58 chan) <+> pretty coco

    readOrCreateStateRef :: Maybe (Hash HbSync) -> AnyStorage -> MyRefKey -> IO (Hash HbSync)
    readOrCreateStateRef mbDs sto ref = do
        debug $ "MyRef:" <+> pretty (hashObject @HbSync ref)
        fix \spin -> do
            mbref <- readStateHashMay sto ref
            case mbref of
                Nothing -> do
                    debug "STATE is empty"
                    mbDs & maybe none \ds -> do
                        debug $ "UPDATE REF" <+> pretty (hashObject @HbSync ref) <+> pretty (HashRef ds)
                        updateRef sto ref ds

                    pause @'Seconds 0.25

                    spin
                Just val -> do
                    pure val

readStateHashMay :: AnyStorage -> MyRefKey -> IO (Maybe (Hash HbSync))
readStateHashMay sto ref =
    getRef @_ @HbSync sto ref
