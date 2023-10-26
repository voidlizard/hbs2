{-# Language UndecidableInstances #-}
{-# Language RecordWildCards #-}
module HBS2.Net.Messaging.Encrypted.ByPass
  ( ForByPass
  , ByPass
  , ByPassOpts(..)
  , ByPassStat(..)
  , byPassDef
  , newByPassMessaging
  , cleanupByPassMessaging
  , getStat
  ) where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Clock hiding (sec)
import HBS2.Net.Messaging
import HBS2.Data.Types.SignedBox
import HBS2.Net.Proto.Definition()
import HBS2.Net.Auth.Credentials()

import HBS2.Net.Messaging.Encrypted.RandomPrefix

import HBS2.System.Logger.Simple

import Codec.Serialise
import Control.Concurrent.STM (flushTQueue)
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Network.ByteOrder qualified as N
import Crypto.Saltine.Core.Box (Keypair(..),CombinedKey)
import Crypto.Saltine.Class qualified as SA
import Crypto.Saltine.Core.Box qualified as PKE
import Data.Bits
import Data.ByteArray.Hash qualified as BA
import Data.ByteArray.Hash (SipHash(..), SipKey(..))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Maybe
import Data.Time.Clock.POSIX
import Data.Word
import System.Random
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO

heySeed :: Word8
heySeed = 117

newtype NonceA = NonceA { fromNonceA :: Word16 }
                 deriving newtype (Eq,Ord,Show,Pretty,Real,Num,Enum,Integral,Hashable)
                 deriving stock Generic

type FlowKey = Word32

instance Serialise NonceA

mySipHash :: Integral a => BS.ByteString -> a
mySipHash s = BA.sipHash (SipKey a b) s
                & \(SipHash w) -> fromIntegral w
  where
   a = 3857206264
   b = 1307114574


-- NOTE: key-update-on-fly
--  мы можем на ходу менять ключи:
--   меняем nonceA, перегенеряем ключ, больше ничего не трогаем:
--   тогда пакеты посланные для старого nonceA можно будет расшифровать,
--   а шифровать уже для нового.
--
--   Таким образом, хост может иметь много flow с разными
--   нонсами одновременно
--


data ByPassOpts e =
  ByPassOpts
  { byPassEnabled    :: Bool
  , byPassKeyAllowed :: PubKey 'Sign (Encryption e) -> IO Bool
  , byPassTimeRange  :: Maybe (Int, Int)
  }

data ByPassStat =
  ByPassStat
  { statBypassed     :: Int
  , statEncrypted    :: Int
  , statDecrypted    :: Int
  , statDecryptFails :: Int
  , statSent         :: Int
  , statReceived     :: Int
  , statFlowNum      :: Int
  , statPeers        :: Int
  , statAuthFail     :: Int
  }
  deriving stock (Show,Generic)

instance Serialise ByPassStat

data ByPass e them =
  ByPass
  { opts         :: ByPassOpts e
  , self         :: Peer e
  , pks          :: PubKey 'Sign (Encryption e)
  , sks          :: PrivKey 'Sign (Encryption e)
  , pke          :: PubKey 'Encrypt (Encryption e)
  , ske          :: PrivKey 'Encrypt (Encryption e)
  , proxied      :: them
  , nonceA       :: NonceA
  , delayed      :: TQueue (To e, ByteString)
  , heySent      :: TVar (HashMap (Peer e) TimeSpec)
  , noncesByPeer :: TVar (HashMap (Peer e) NonceA)
  , flowKeys     :: TVar (HashMap FlowKey CombinedKey)
  , bypassed     :: TVar Int
  , encrypted    :: TVar Int
  , decrypted    :: TVar Int
  , decryptFails :: TVar Int
  , sentNum      :: TVar Int
  , recvNum      :: TVar Int
  , authFail     :: TVar Int
  }

type ForByPass e   = ( Hashable (Peer e)
                     , Pretty (Peer e)
                     , Eq (PubKey 'Sign (Encryption e))
                     , Serialise (PubKey 'Sign (Encryption e))
                     , PrivKey 'Encrypt (Encryption e) ~ PKE.SecretKey
                     , PubKey 'Encrypt (Encryption e) ~ PKE.PublicKey
                     , ForSignedBox e
                     )


data HEYBox e =
  HEYBox Int (PubKey 'Encrypt (Encryption e))
  deriving stock Generic

instance ForByPass e => Serialise (HEYBox e)

data EncryptHandshake e =
    HEY
    { heyNonceA :: NonceA
    , heyBox    :: SignedBox (HEYBox e) e
    }
  deriving stock (Generic)

instance ForByPass e => Serialise (EncryptHandshake e)

getStat :: forall e w m . ( ForByPass e
                          , MonadIO m
                          )
        => ByPass e w
        -> m ByPassStat
getStat bus = liftIO $
  ByPassStat <$> readTVarIO (bypassed bus)
             <*> readTVarIO (encrypted bus)
             <*> readTVarIO (decrypted bus)
             <*> readTVarIO (decryptFails bus)
             <*> readTVarIO (sentNum bus)
             <*> readTVarIO (recvNum bus)
             <*> (readTVarIO (flowKeys bus) <&> HashMap.size)
             <*> (readTVarIO (noncesByPeer bus) <&> HashMap.size)
             <*> readTVarIO (authFail bus)

cleanupByPassMessaging :: forall e w m . ( ForByPass e
                                    , MonadIO m
                                    )
                   => ByPass e w
                   -> [Peer e]
                   -> m ()

cleanupByPassMessaging  bus pips = do
  debug "cleanupByPassMessaging"

  let alive = HashSet.fromList pips

  atomically do
    sent   <- readTVar (heySent bus)
    nonces <- readTVar (noncesByPeer bus)
    flows  <- readTVar (flowKeys bus)

    let livePeers = [ (k,v)
                    | (k,v) <- HashMap.toList nonces
                    , k `HashSet.member` alive
                    ] & HashMap.fromList

    let liveSent = HashMap.filterWithKey (\k _ -> k `HashMap.member` livePeers) sent

    let liveFk = [ makeKey (nonceA bus) nonce
                 | nonce <- HashMap.elems livePeers
                 ] & HashSet.fromList

    let liveFlows =  HashMap.filterWithKey (\k _ -> k `HashSet.member` liveFk) flows

    writeTVar (heySent bus) liveSent
    writeTVar (noncesByPeer bus) livePeers
    writeTVar (flowKeys bus) liveFlows


byPassDef :: ByPassOpts e
byPassDef =
  ByPassOpts
  { byPassEnabled    = True
  , byPassKeyAllowed = const $ pure True
  , byPassTimeRange  = Nothing
  }

newByPassMessaging :: forall e w m . ( ForByPass e
                                     , MonadIO m
                                     , Messaging w e ByteString
                                     )
                   => ByPassOpts e
                   -> w
                   -> Peer e
                   -> PubKey 'Sign (Encryption e)
                   -> PrivKey 'Sign (Encryption e)
                   -> m (ByPass e w)

newByPassMessaging o w self ps sk  = do
  (Keypair s p) <- liftIO PKE.newKeypair
  let n = mySipHash (LBS.toStrict (serialise s))
  ByPass @e o self ps sk p s w n <$> newTQueueIO
                                 <*> newTVarIO mempty
                                 <*> newTVarIO mempty
                                 <*> newTVarIO mempty
                                 <*> newTVarIO 0
                                 <*> newTVarIO 0
                                 <*> newTVarIO 0
                                 <*> newTVarIO 0
                                 <*> newTVarIO 0
                                 <*> newTVarIO 0
                                 <*> newTVarIO 0

instance (ForByPass e, Messaging w e ByteString)
  => Messaging (ByPass e w) e ByteString where

  sendTo bus t@(To whom) f m = do

    mkey <- lookupEncKey bus whom

    atomically $ modifyTVar (sentNum bus) succ

    case mkey of
      Just fck -> do
        sendTo (proxied bus) t f =<< encryptMessage bus fck m

      Nothing -> do
        -- let ByPassOpts{..} = opts bus

        if False then do
          atomically $ writeTQueue (delayed bus) (t,m)
        else do
          trace $ "bypassed message to"  <+> pretty whom
          atomically $ modifyTVar (bypassed bus) succ
          sendTo (proxied bus) t f m

        -- TODO: stop-sending-hey-after-while
        --   Если адрес кривой и мы его не знаем/не можем
        --   на него послать/ничего с него не получаем ---
        --   надо переставать слать на него HEY с какого-то момента

        -- TODO: fix-timeout-hardcode
        withHeySent bus 30 whom do
          sendHey bus whom

  receive bus f = do
    msgs <- receive  (proxied bus) f

    q <- newTQueueIO

    -- TODO: run-concurrently
    for_ msgs $ \(From who, mess) -> runMaybeT do

      atomically $ modifyTVar (recvNum bus) succ

      hshake <- processHey who mess

      guard (not hshake)

      msg <- tryDecryptMessage bus mess

      case msg of
        Just demsg -> do
          atomically $ writeTQueue q (From who, demsg)

        Nothing    -> do
          withHeySent bus 60 who do
            sendHey bus who

          atomically $ writeTQueue q (From who, mess)

    liftIO $ atomically $ flushTQueue q

    where
      processHey orig bs = isJust <$> runMaybeT do

        let o = opts bus

        let (code, hbs) = runCodeLazy bs

        -- FIXME: check-code
        guard ( code == Just heySeed )

        debug $ "HEY CODE:" <> parens (pretty code) <+> pretty orig

        guard (not (LBS.null hbs))

        hshake <- toMPlus (deserialiseOrFail @(EncryptHandshake e) hbs)

        case hshake of
          HEY{..} -> do-- void $ runMaybeT do
            debug $ "GOT HEY MESSAGE" <+> parens (pretty code) <+> pretty heyNonceA

            let mbx = unboxSignedBox0 heyBox

            when (isNothing mbx) do
              debug $ "HEY: failed to unbox" <+> pretty heyNonceA <+> pretty orig

            n <- toMPlus mbx

            (pks, HEYBox t puk) <- toMPlus mbx

            let dt = byPassTimeRange o

            allowed <- liftIO $ byPassKeyAllowed o pks
            now <- liftIO getPOSIXTime <&> round
            let actual = maybe1 dt True (\(ta, tb) -> t >= now - ta &&  t <= now + tb)

            let authorized = allowed && actual

            unless authorized do
              atomically $ modifyTVar (authFail bus) succ
              warn $ "ByPass:" <+> "NOT AUTHORIZED" <+> pretty orig

            when authorized do
              debug $ "ByPass:" <+> "AUTHORIZED" <+> pretty now <+> pretty orig

            guard authorized

            let fk = makeKey (nonceA bus) heyNonceA

            here <- readTVarIO (flowKeys bus) <&> HashMap.member fk

            updatePeerNonce bus orig heyNonceA

            unless here do

              let ck = PKE.beforeNM (ske bus) puk

              debug $ "HEY: CK" <+> pretty (nonceA bus)
                                <+> pretty fk
                                <+> pretty (hashObject @HbSync (SA.encode ck))

              atomically $ do
                modifyTVar (flowKeys bus) (HashMap.insert fk ck)

              withHeySent bus 30 orig do
                sendHey bus orig

        pure hshake

makeKey :: NonceA -> NonceA -> FlowKey
makeKey a b = runIdentity do
  let aa = fromIntegral a :: FlowKey
  let bb = fromIntegral b :: FlowKey

  let (f0,f1) = if aa < bb then (aa,bb) else (bb,aa)

  pure $ (f0 `shiftL` 16) .|. f1


sendHey :: forall e w m . ( ForByPass e
                          , Messaging w e ByteString
                          , MonadIO m
                          )
        => ByPass e w
        -> Peer e
        -> m ()

sendHey bus whom = do

  pref <- randomPrefix (PrefixMethod1 4 11 heySeed) <&> toLazyByteString

  let (code, _) = runCodeLazy pref

  ts <- liftIO getPOSIXTime <&> round

  let hbox = HEYBox @e ts (pke bus)
  let box = makeSignedBox @e (pks bus) (sks bus) hbox
  let hey = HEY @e (nonceA bus) box
  let msg = pref <> serialise hey

  debug $ "SEND HEY" <+> pretty  (heyNonceA hey)
                     <+> parens ("seed" <+> pretty code)
                     <+> pretty whom
                     <+> pretty (LBS.length  msg)

  sendTo (proxied bus) (To whom) (From (self bus)) msg

withHeySent :: forall e w m . (MonadIO m, ForByPass e)
            => ByPass e w
            -> Timeout 'Seconds
            -> Peer e
            -> m ()
            -> m ()

withHeySent w ts pip m = do
  now <- getTimeCoarse

  t0  <- readTVarIO (heySent w) <&> HashMap.lookup pip
           <&> fromMaybe 0

  let elapsed = toNanoSeconds $ TimeoutTS (now - t0)

  when ( elapsed >= toNanoSeconds ts ) do
    atomically $ modifyTVar (heySent w) (HashMap.insert pip now)
    m


updatePeerNonce :: forall e w m . ( ForByPass e
                                  , MonadIO m
                                  )
                => ByPass e w
                -> Peer e
                -> NonceA
                -> m ()

updatePeerNonce bus pip nonce = do
  atomically $ modifyTVar (noncesByPeer bus) (HashMap.insert pip nonce)

lookupEncKey :: (ForByPass e, MonadIO m) => ByPass e w -> Peer e -> m (Maybe (FlowKey, CombinedKey))
lookupEncKey bus whom = runMaybeT do
  nonce <- MaybeT $ readTVarIO (noncesByPeer bus) <&> HashMap.lookup whom
  let fk = makeKey nonce (nonceA bus)
  ck <- MaybeT $ readTVarIO (flowKeys bus) <&> HashMap.lookup fk
  pure (fk, ck)


typicalNonceLength :: Integral a => a
typicalNonceLength = unsafePerformIO PKE.newNonce & SA.encode & BS.length & fromIntegral
{-# NOINLINE typicalNonceLength #-}

newtype ByPassNonce = ByPassNonce { unByPassNonce :: PKE.Nonce }

instance NonceFrom ByPassNonce Word32 where
  nonceFrom a = ByPassNonce nonce
    where
      n = typicalNonceLength
      nonce = fromJust (SA.decode s)
      s = BS.take n (N.bytestring32 a <> BS.replicate n 0)


tryDecryptMessage :: (MonadIO m, ForByPass e)
                  => ByPass e w
                  -> ByteString
                  -> m (Maybe ByteString)

tryDecryptMessage bus bs = runMaybeT do

  let (hdr, body) = LBS.splitAt 8 bs

  guard (LBS.length hdr == 8)

  (fk, wnonce) <- liftIO $ N.withReadBuffer (LBS.toStrict hdr) $ \buf -> do
                              (,) <$> N.read32 buf <*> N.read32 buf

  let bnonce = nonceFrom @ByPassNonce wnonce

  ck <- MaybeT $ readTVarIO (flowKeys bus) <&> HashMap.lookup fk

  let dmess = PKE.boxOpenAfterNM ck (unByPassNonce bnonce) (LBS.toStrict body) <&> LBS.fromStrict

  atomically do
    maybe1 dmess
      (modifyTVar (decryptFails bus) succ)
      (const $ modifyTVar (decrypted bus) succ)

  toMPlus dmess


encryptMessage :: (MonadIO m, ForByPass e)
               => ByPass e w
               -> (FlowKey, CombinedKey)
               -> ByteString
               -> m ByteString

encryptMessage bus (fk, ck) bs = do

  atomically $ modifyTVar (encrypted bus) succ

  wnonce <- liftIO (randomIO @Word32)
  let bnonce = nonceFrom @ByPassNonce wnonce

  let ebs = PKE.boxAfterNM ck (unByPassNonce bnonce) (LBS.toStrict bs)

  let pkt = mconcat [ word32BE fk
                    , word32BE wnonce
                    , byteString ebs
                    ] & toLazyByteString

  pure pkt

instance Pretty ByPassStat where
  pretty (ByPassStat{..}) =
    vcat [ prettyField "bypassed"     statBypassed
         , prettyField "encrypted"    statEncrypted
         , prettyField "decrypted"    statDecrypted
         , prettyField "decryptFails" statDecryptFails
         , prettyField "sent"         statSent
         , prettyField "received"     statReceived
         , prettyField "flowNum"      statFlowNum
         , prettyField "peers"        statPeers
         , prettyField "authFail"     statAuthFail
         ]
    where
      prettyField x e = fill 15 (x <> colon) <+> pretty e

