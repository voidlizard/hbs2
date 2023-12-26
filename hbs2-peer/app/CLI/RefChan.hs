module CLI.RefChan where

import HBS2.Prelude.Plated

import HBS2.Hash
import HBS2.Clock
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.Net.Proto.Definition()
import HBS2.Net.Proto.RefChan
import HBS2.Net.Proto.Types
import HBS2.Data.Types.Refs
import HBS2.Merkle
import HBS2.Actors.Peer.Types
import HBS2.Data.Types.SignedBox
import HBS2.Net.Proto.Service
import HBS2.Data.Detect
import HBS2.Storage
import HBS2.Storage.Operations.ByteString
import HBS2.Data.Types.Refs
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

import HBS2.System.Logger.Simple hiding (info)
import HBS2.System.Logger.Simple qualified as Log

import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.HashSet qualified as HashSet
import Data.Maybe
import Lens.Micro.Platform
import Options.Applicative
import System.Exit
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad.Except (runExceptT)
import Data.Word
import Codec.Serialise
import UnliftIO
import Streaming.Prelude qualified as S


pRefChan :: Parser (IO ())
pRefChan = hsubparser (   command "head" (info pRefChanHead (progDesc "head commands" ))
                       <> command "propose" (info pRefChanPropose (progDesc "post propose transaction"))
                       <> command "notify"  (info pRefChanNotify (progDesc "post notify message"))
                       <> command "fetch"   (info pRefChanFetch (progDesc "fetch and sync refchan value"))
                       <> command "get"     (info pRefChanGet (progDesc "get refchan value"))
                       <> command "gk"      (info pRefChanGK  (progDesc "generate a group key"))
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
  kr <- strOption (long "keyring" <> short 'k' <> help "owner credentials")
  fn <- optional $ strArgument (metavar "head dsl file")
  pure $ do
    sc <- BS.readFile kr
    creds <- pure (parseCredentials @(Encryption L4Proto) (AsCredFile sc)) `orDie` "bad keyring file"
    s <- maybe1 fn getContents readFile
    hd <- pure (fromStringMay @(RefChanHeadBlock L4Proto) s) `orDie` "can't generate head block"
    let qq = makeSignedBox @L4Proto @(RefChanHeadBlock L4Proto) (view peerSignPk creds) (view peerSignSk creds) hd
    LBS.putStr (serialise qq)

pRefChanHeadDump :: Parser (IO ())
pRefChanHeadDump= do
  fn <- optional $ strArgument (metavar "refchan head blob")
  pure $ do
    lbs <- maybe1 fn LBS.getContents LBS.readFile
    (_, hdblk) <- pure (unboxSignedBox @(RefChanHeadBlock L4Proto) @L4Proto  lbs) `orDie` "can't unbox signed box"
    print $ pretty hdblk


-- FIXME: options-duped-with-peer-main
confOpt :: Parser FilePath
confOpt = strOption ( long "config"  <> short 'c' <> help "config" )

rpcOpt :: Parser String
rpcOpt = strOption ( short 'r' <> long "rpc"
                               <> help "addr:port" )

pRpcCommon :: Parser RPCOpt
pRpcCommon = do
  RPCOpt <$> optional confOpt
         <*> optional rpcOpt

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
  kra <- strOption (long "author" <> short 'a' <> help "author credentials")
  fn  <- optional $ strOption (long "file" <> short 'f' <> help "file")
  dry <- optional (flag' True (long "dry" <> short 'n' <> help "only dump transaction")) <&> fromMaybe False
  puk <- argument pRefChanId (metavar "REFCHAH-KEY")
  pure $ withMyRPC @RefChanAPI opts $ \caller -> do
    sc <- BS.readFile kra
    creds <- pure (parseCredentials @(Encryption L4Proto) (AsCredFile sc)) `orDie` "bad keyring file"

    lbs <- maybe1 fn LBS.getContents LBS.readFile

    let box = makeSignedBox @L4Proto @BS.ByteString (view peerSignPk creds) (view peerSignSk creds) (LBS.toStrict lbs)

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

    sigil <- liftIO $ (BS.readFile si <&> parseSerialisableFromBase58 @(Sigil L4Proto))
                        `orDie` "parse sigil failed"

    (auPk, sd) <- pure (unboxSignedBox0 @(SigilData L4Proto) (sigilData sigil))
                   >>= orThrowUser "malformed sigil/bad signature"

    keys <- liftIO $ runKeymanClient do
              creds  <- loadCredentials auPk >>= orThrowUser "can't load credentials"
              encKey <- loadKeyRingEntry (sigilDataEncKey sd)
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

                    gk <- deserialiseOrFail @(GroupKey 'Symm HBS2Basic) gks
                            & toMPlus

                    notice $ "found GK0" <+> pretty gkv

                    pure gk

            gk <- case mgk of
                    Just x -> pure x
                    Nothing -> do
                      gknew <- generateGroupKey @HBS2Basic Nothing (HashSet.toList rcpts)

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

    let box = makeSignedBox @L4Proto @BS.ByteString (view peerSignPk creds) (view peerSignSk creds) (LBS.toStrict ss)
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

    gk <- generateGroupKey @HBS2Basic Nothing (HashSet.toList readers)

    liftIO $ print $ pretty (AsGroupKeyFile gk)

