module CLI.RefChan where

import HBS2.Prelude.Plated

import HBS2.Hash
import HBS2.Base58
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.Merkle
import HBS2.Peer.Proto.RefChan
import HBS2.Data.Types.Refs
import HBS2.Actors.Peer.Types
import HBS2.Data.Types.SignedBox
import HBS2.Net.Proto.Service
import HBS2.Data.Detect
import HBS2.Storage
import HBS2.Storage.Operations.ByteString
import HBS2.Net.Messaging.Unix
import HBS2.Net.Auth.GroupKeySymm

import HBS2.KeyMan.Keys.Direct

import HBS2.OrDie

import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Peer.RefChanNotifyLog

import CLI.Common
import RPC2()
import PeerLogger hiding (info)

import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.HashSet qualified as HashSet
import Data.Maybe
import Data.Coerce
import Lens.Micro.Platform
import Options.Applicative
import System.Exit
import Control.Monad.Except (runExceptT)
import Codec.Serialise
import UnliftIO


pRefChan :: Parser (IO ())
pRefChan = hsubparser (   command "head"    (info pRefChanHead       (progDesc "head commands" ))
                       <> command "propose" (info pRefChanPropose (progDesc "post propose transaction"))
                       <> command "notify"  (info pRefChanNotify  (progDesc "post notify message"))
                       <> command "fetch"   (info pRefChanFetch   (progDesc "fetch and sync refchan value"))
                       <> command "get"     (info pRefChanGet     (progDesc "get refchan value"))
                       <> command "gk"      (info pRefChanGK      (progDesc "generate a group key"))
                       <> command "cat"     (info pRefChanCat    (progDesc "dump refchan content"))
                      )


pRefChanHead :: Parser (IO ())
pRefChanHead = hsubparser (   command "gen"  (info pRefChanHeadGen (progDesc "generate head blob"))
                           <> command "dump" (info pRefChanHeadDump (progDesc "dump head blob"))
                           <> command "post" (info pRefChanHeadPost (progDesc "post head transaction"))
                           <> command "fetch" (info pRefChanHeadFetch (progDesc "fetch head from neighbours"))
                           <> command "get"  (info pRefChanHeadGet (progDesc "get head value"))
                          )

pRefChanHeadGen :: Parser (IO ())
pRefChanHeadGen = do
  fn <- optional $ strArgument (metavar "head dsl file")
  rchan <- argument pRefChanId (metavar "REFCHAN-KEY")
  pure $ do

    creds <- runKeymanClient $ loadCredentials rchan
               >>= orThrowUser "can't load credentials"

    s <- maybe1 fn getContents readFile
    hd <- pure (fromStringMay @(RefChanHeadBlock L4Proto) s) `orDie` "can't generate head block"
    let qq = makeSignedBox @'HBS2Basic (view peerSignPk creds) (view peerSignSk creds) hd
    LBS.putStr (serialise qq)

data HeadDumpOpts = HeadDumpRef (RefChanId L4Proto)
                  | HeadDumpFile FilePath

pRefChanHeadDump :: Parser (IO ())
pRefChanHeadDump= do
  opts <- pRpcCommon

  what <- optional $ HeadDumpRef <$> argument pRefChanId  (metavar "REFCHAN-KEY")
            <|> HeadDumpFile <$> strOption (short 'f' <> long "file"
                                                      <> metavar "FILE"
                                                      <> help "read from file")

  pure $ flip runContT pure do

    lbs <- case what of
              Nothing -> lift $ LBS.getContents
              Just (HeadDumpFile f) -> lift $ LBS.readFile f
              Just (HeadDumpRef r) -> do

                client <- ContT $ withRPCMessaging opts

                self <- runReaderT (ownPeer @UNIX) client
                refChanAPI  <- makeServiceCaller @RefChanAPI self
                storageAPI  <- makeServiceCaller @StorageAPI self

                let endpoints = [ Endpoint @UNIX refChanAPI
                                , Endpoint @UNIX storageAPI
                                ]

                void $ ContT $ bracket (async $ runReaderT (runServiceClientMulti endpoints) client) cancel

                rv <- lift (callRpcWaitMay @RpcRefChanHeadGet (TimeoutSec 1) refChanAPI r)
                        >>= orThrowUser "rpc error"
                        >>= orThrowUser "refchan head value not found"

                liftIO $ print (pretty rv)

                let sto = AnyStorage (StorageClient storageAPI)
                runExceptT (readFromMerkle sto (SimpleKey (coerce rv)))
                          >>= orThrowUser "can't decode refchan head "


    (pk, hdblk) <- pure (unboxSignedBox @(RefChanHeadBlock L4Proto) @'HBS2Basic lbs) `orDie` "can't unbox signed box"
    liftIO $ print $
      (semi <+> "refchan" <+> pretty (AsBase58 pk)) <> line <> pretty hdblk


pRefChanHeadPost :: Parser (IO ())
pRefChanHeadPost = do
  opts <- pRpcCommon
  ref <- strArgument (metavar "HEAD-BLOCK-TREE-HASH")
  pure $ withMyRPC @RefChanAPI opts $ \caller -> do
    href <- pure (fromStringMay ref) `orDie` "HEAD-BLOCK-TREE-HASH"
    -- FIXME: proper-error-handling
    void $ callService @RpcRefChanHeadPost caller href

pRefChanHeadFetch :: Parser (IO ())
pRefChanHeadFetch = do
  opts <- pRpcCommon
  ref <- strArgument (metavar "REFCHAH-HEAD-KEY")
  pure $ withMyRPC @RefChanAPI opts $ \caller -> do
    href <- pure (fromStringMay ref) `orDie` "invalid REFCHAN-HEAD-REF"
    void $ callService @RpcRefChanHeadFetch caller href

pRefChanHeadGet :: Parser (IO ())
pRefChanHeadGet = do
  rpc <- pRpcCommon
  ref <- strArgument (metavar "REFCHAH-HEAD-KEY")
  pure $ withMyRPC @RefChanAPI rpc $ \caller -> do
    href <- pure (fromStringMay ref) `orDie` "invalid REFCHAN-HEAD-REF"
    callService @RpcRefChanHeadGet caller href >>= \case
      Left{} -> exitFailure
      Right Nothing -> exitFailure
      Right (Just h) -> print (pretty h) >> exitSuccess

pRefChanPropose :: Parser (IO ())
pRefChanPropose = do
  opts <- pRpcCommon
  kra <- option pRefChanId (long "author" <> short 'a' <> help "author key")
  fn  <- optional $ strOption (long "file" <> short 'f' <> help "file")
  dry <- optional (flag' True (long "dry" <> short 'n' <> help "only dump transaction")) <&> fromMaybe False
  puk <- argument pRefChanId (metavar "REFCHAH-KEY")

  pure $ withMyRPC @RefChanAPI opts $ \caller -> do
    creds <- runKeymanClient $ loadCredentials kra
               >>= orThrowUser "can't load credentials"

    lbs <- maybe1 fn LBS.getContents LBS.readFile

    let box = makeSignedBox (view peerSignPk creds) (view peerSignSk creds) (LBS.toStrict lbs)

    if dry then do
      LBS.putStr (serialise box)
    else do
      -- FIXME: proper-error-handling
      void $ callService @RpcRefChanPropose caller (puk, box)


pRefChanNotify :: Parser (IO ())
pRefChanNotify =
  hsubparser (  command  "post"  (info pRefChanNotifyPost    (progDesc "post notify message"))
             <> command  "log"   (info pRefChanNotifyLog     (progDesc  "post notify message"))
             <> command  "tail"  (info pRefChanNotifyLogTail (progDesc  "output last messages"))
             )

data EncMethod = EncryptWithSigil FilePath
               | NoEncryption

pRefChanNotifyPost :: Parser (IO ())
pRefChanNotifyPost = do
  opts <- pRpcCommon

  si  <- strOption (long "sigil" <> short 's' <> help "sigil file")
  fn   <- optional $ strOption (long "file" <> short 'f' <> help "file")

  encrypt  <- encryptOpt
  -- EncMethod <$> optional (switch (long "encrypt" <> help "encrypt transaction"))
  --                       <*> optional (strOption (long "sigil" <> help "using sigil"))

  puk <- argument pRefChanId (metavar "REFCHAH-REF")

  pure $ flip runContT pure do

    client <- ContT $ withRPCMessaging opts

    self <- runReaderT (ownPeer @UNIX) client
    refChanAPI  <- makeServiceCaller @RefChanAPI self
    storageAPI  <- makeServiceCaller @StorageAPI self

    let endpoints = [ Endpoint @UNIX refChanAPI
                    , Endpoint @UNIX storageAPI
                    ]

    void $ ContT $ bracket (async $ runReaderT (runServiceClientMulti endpoints) client) cancel

    -- caller <- ContT $ withMyRPC @RefChanAPI opts

    sigil <- liftIO $ (BS.readFile si <&> parseSerialisableFromBase58)
                        `orDie` "parse sigil failed"

    (auPk, sd) <- pure (unboxSignedBox0 (sigilData sigil))
                   >>= orThrowUser "malformed sigil/bad signature"

    keys <- liftIO $ runKeymanClient do
              creds  <- loadCredentials auPk >>= orThrowUser "can't load credentials"
              encKey <- loadKeyRingEntry (sigilDataEncKey @'HBS2Basic sd)
              pure (creds,encKey)

    let creds = view _1 keys

    lbs <- liftIO $ maybe1 fn LBS.getContents LBS.readFile

    let sto = AnyStorage (StorageClient storageAPI)

    ss <- if not encrypt then do
            pure lbs
          else do
            -- итак нам тут нужно знать:
            --   2.   старый gk или новый
            --   2.1  если старый - то где взять
            --   2.2  стоит обновить gk или нет
            --   2.3  стоит сохранять gk или нет
            --
            --   допустим так: ключ равно: голова + эпоха + enc SigilData
            --
            --   тогда: 1) где его хранить? в кеймане или в стейте?
            --
            --   + в кеймане, допустим?
            --   в кеймане может быть блокировка sqlite, нехорошо

            -- можно сохранять в hbs2:
            --   1. групповой ключ и так там сохраняется
            --   2. ссылок не должно быть много, если
            --      ссылка ~ hash(канал, ключ пользователя)
            --   3. обновление ключа:  если явно сказано!
            --      иначе -- берём существующий
            --   4. если голова поменялась -- можем
            --      удалить ссылку?

            kre@(KeyringKeys pk sk) <- orThrowUser "encryption key not found for given sigil" (view _2 keys)
            let kreHash = hashObject @HbSync (serialise kre)

            hh <- callService @RpcRefChanHeadGet refChanAPI puk
                    >>= orThrowUser "RPC error"
                    >>= orThrowUser "refchan head not available"

            hd <- getRefChanHead @L4Proto sto (RefChanHeadKey puk)
                    >>= orThrowUser "head block not found"

            let rcpts = view refChanHeadReaders hd

            when ( HashSet.null rcpts ) do
              throwIO (userError "empty recipients list -- encryption is not possible")

            notice $ "refchan head" <+> pretty hh

            -- FIXME: key-rotation-factor-hardcode
            --   около раза в месяц. может, нормально и так
            t <- liftIO $ getPOSIXTime <&> round

            let gkkey0 = SomeRefKey (hh,kreHash)

            notice $ "gkkey0" <+> pretty (hashObject @HbSync gkkey0)

            mgk <- runMaybeT do
                    gkv <- toMPlus =<< getRef sto gkkey0

                    notice $ "FOUND REF VAL" <+> pretty gkv

                    gks <- runExceptT (readFromMerkle sto (SimpleKey gkv))
                             >>= toMPlus

                    gk <- deserialiseOrFail @(GroupKey 'Symm 'HBS2Basic) gks
                            & toMPlus

                    notice $ "found GK0" <+> pretty gkv

                    pure gk

            gk <- case mgk of
                    Just x -> pure x
                    Nothing -> do
                      gknew <- generateGroupKey @'HBS2Basic Nothing (HashSet.toList rcpts)

                      gkh <- writeAsMerkle sto (serialise gknew)

                      -- FIXME: key-expiration-hardcode
                      let gkkey1 = ExpiredAfter (t+1209600) gkkey0

                      notice $ "UPDATE REF" <+> pretty (hashObject @HbSync gkkey1) <+> pretty gkh
                      updateRef sto gkkey1  gkh

                      notice $ "generated! GK0" <+> pretty gkh

                      pure gknew

            gks <- orThrowUser "can't decrypt group key" $ lookupGroupKey sk pk gk
            -- FIXME: use-deterministic-nonce
            lift $ encryptBlock sto gks (Right gk) Nothing lbs <&> serialise

    let box = makeSignedBox (view peerSignPk creds) (view peerSignSk creds) (LBS.toStrict ss)
    void $ callService @RpcRefChanNotify refChanAPI (puk, box)

  where
    encryptOpt = do
      optional (switch (long "encrypt" <> help "post encrypted")) <&> fromMaybe False

    -- noEncryption = pure NoEncryption
    -- encryptWithSigil = EncryptWithSigil <$> strOption (long "sigil" <> help "sigil file")

pRefChanId :: ReadM (RefChanId L4Proto)
pRefChanId = maybeReader (fromStringMay @(RefChanId L4Proto))

pRefChanNotifyLog :: Parser (IO ())
pRefChanNotifyLog = do
  opts <- pRpcCommon
  puk <- argument pRefChanId (metavar "REFCHAH-REF")
  pure $ withMyRPC @StorageAPI opts $ \caller -> do
    let sto = AnyStorage (StorageClient caller)
    href <- getRef sto (refAlias (makeRefChanNotifyLogKey @L4Proto puk)) <&> fmap HashRef
    maybe1 href exitFailure $ \r -> do
      print $ pretty r
      exitSuccess

pRefChanNotifyLogTail :: Parser (IO ())
pRefChanNotifyLogTail = do
  opts <- pRpcCommon

  n <- optional (option auto ( short 'n' <> long "lines" <> help "output last NUM lines" ))
          <&> fromMaybe 10

  puk <- argument pRefChanId (metavar "REFCHAH-REF")
  pure $ withMyRPC @StorageAPI opts $ \caller -> void $ runMaybeT do
    let sto = AnyStorage (StorageClient caller)
    href <- getRef sto (refAlias (makeRefChanNotifyLogKey @L4Proto puk)) <&> fmap HashRef
              >>= toMPlus

    rs <- readLog (getBlock sto) href
             <&> reverse . take n . reverse

    liftIO $ print $ vcat (fmap pretty rs)

pRefChanGet :: Parser (IO ())
pRefChanGet = do
  opts <- pRpcCommon
  sref <- strArgument (metavar "REFCHAH-KEY")
  pure $ withMyRPC @RefChanAPI opts $ \caller -> do
    puk <- pure (fromStringMay @(RefChanId L4Proto) sref) `orDie` "can't parse refchan/public key"
    callService @RpcRefChanGet caller puk >>= \case
      Left{} -> exitFailure
      Right Nothing -> exitFailure
      Right (Just h) -> print (pretty h) >> exitSuccess

pRefChanFetch :: Parser (IO ())
pRefChanFetch = do
  opts <- pRpcCommon
  ref <- strArgument (metavar "REFCHAH-KEY")
  pure $ withMyRPC @RefChanAPI opts $ \caller -> do
    href <- pure (fromStringMay ref) `orDie` "invalid REFCHAN-HEAD-REF"
    void $ callService @RpcRefChanFetch caller href


pRefChanGK :: Parser (IO ())
pRefChanGK = do
  opts <- pRpcCommon
  puk <- argument pRefChanId (metavar "REFCHAH-REF")
  pure $ flip runContT pure do

    client <- ContT $ withRPCMessaging opts

    self <- runReaderT (ownPeer @UNIX) client
    refChanAPI  <- makeServiceCaller @RefChanAPI self
    storageAPI  <- makeServiceCaller @StorageAPI self

    let endpoints = [ Endpoint @UNIX refChanAPI
                    , Endpoint @UNIX storageAPI
                    ]

    void $ ContT $ bracket (async $ runReaderT (runServiceClientMulti endpoints) client) cancel

    let sto = AnyStorage (StorageClient storageAPI)

    hd <- getRefChanHead @L4Proto sto (RefChanHeadKey puk)
           >>= orThrowUser "head block not found"

    let readers = view refChanHeadReaders' hd

    gk <- generateGroupKey @'HBS2Basic Nothing (HashSet.toList readers)

    liftIO $ print $ pretty (AsGroupKeyFile gk)


pRefChanCat :: Parser (IO ())
pRefChanCat = do
  opts <- pRpcCommon
  puk <- argument pRefChanId (metavar "REFCHAH-REF")
  pure $ flip runContT pure do

    client <- ContT $ withRPCMessaging opts

    self <- runReaderT (ownPeer @UNIX) client
    refChanAPI  <- makeServiceCaller @RefChanAPI self
    storageAPI  <- makeServiceCaller @StorageAPI self

    let endpoints = [ Endpoint @UNIX refChanAPI
                    , Endpoint @UNIX storageAPI
                    ]

    void $ ContT $ bracket (async $ runReaderT (runServiceClientMulti endpoints) client) cancel

    let sto = AnyStorage (StorageClient storageAPI)

    rv' <- lift (callRpcWaitMay @RpcRefChanGet (TimeoutSec 1) refChanAPI puk)
             >>= orThrowUser "rpc call error/timeout"

    rv <- ContT $ maybe1 rv' none

    walkMerkle (fromHashRef rv) (getBlock sto) $ \case
      Left{} -> pure ()
      Right (hrs :: [HashRef]) -> do
        for_ hrs $ \h -> void $ runMaybeT do

          s <- getBlock sto (fromHashRef h)
                 >>= toMPlus
                 <&> deserialiseOrFail @(RefChanUpdate L4Proto)
                 >>= toMPlus

          case s of
            Accept{} -> pure ()
            Propose _ box -> do
              (_, ProposeTran _ pbox :: ProposeTran L4Proto) <- toMPlus $ unboxSignedBox0 box
              (_, bs2) <- toMPlus $ unboxSignedBox0 pbox
              liftIO $ BS.putStr bs2

