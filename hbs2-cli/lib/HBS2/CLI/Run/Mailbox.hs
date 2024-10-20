module HBS2.CLI.Run.Mailbox where


import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.Merkle

import HBS2.Net.Auth.GroupKeySymm
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Policy.Basic

import HBS2.Base58
import HBS2.System.Dir
import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Storage
import HBS2.KeyMan.Keys.Direct as K

import Codec.Serialise
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.Except
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Coerce
import Data.Either

createShortMessageFromByteString :: forall s m . ( MonadUnliftIO m
                                                 , s ~ HBS2Basic
                                                 , HasStorage m
                                                 )
                            => LBS8.ByteString
                            -> m (Message s)
createShortMessageFromByteString lbs = do
  let ls0 = LBS8.lines lbs
  let (hbs, rest1) = break LBS8.null ls0
  let payload      = dropWhile LBS8.null rest1 & LBS8.unlines
  let headers      = parseTop (LBS8.unpack (LBS8.unlines hbs)) & fromRight mempty

  flagz <- defMessageFlags

  sender <-  headMay [ Left s | ListVal [SymbolVal "sender", HashLike s] <- headers ]
               & orThrowUser "sender not defined"

  let rcpts = [ Left s | ListVal [SymbolVal "recipient", HashLike s] <- headers ]

  sto <- getStorage

  let cms = CreateMessageServices
              sto
              ( runKeymanClientRO . loadCredentials )
              ( runKeymanClientRO . loadKeyRingEntry )

  createMessage cms flagz Nothing sender rcpts mempty (LBS8.toStrict payload)



mailboxEntries :: forall c m . ( IsContext c
                               , MonadUnliftIO m
                               , HasStorage m
                               , Exception (BadFormException c)
                               ) => MakeDictM  c m ()
mailboxEntries = do

  brief "creates a new object of Mailbox.Message from text"
    $ args [arg "string" "filename"]
    $ desc ""
    $ returns "blob" "message"
    $ entry $ bindMatch "hbs2:mailbox:message:create:short" $ \case
        [StringLike fn] -> lift do
          lbs <- liftIO $ LBS8.readFile fn
          mess <- createShortMessageFromByteString lbs
          mkOpaque (serialise mess)

        _ -> throwIO (BadFormException @c nil)


  brief "creates a new multipart message"
    $ desc [qc|
;; creates multipart message

hbs2:mailbox:message:create:multipart [kw k1 v1 kn kv]

WHERE

 k ::= sender | recipient | body | part

 sender ::= HASH(sigil)
 body   ::= STRING
 part   ::= FILENAME

    |]
    $ examples [qc|

[hbs2:peer:storage:put
..hbs2:mailbox:message:create:multipart
...[kw sender      ghna99Xtm33ncfdUBT3htBUoEyT16wTZGMdm24BQ1kh
.......recipient   4e9moTcp9AW13wRYYWg5F8HWooVH1PuQ7zsf5g2JYPWj
.......body        [str:file body.txt]
.......part        patch1.patch
...]]

|]
    $ returns "bytes" "message"
    $ entry $ bindMatch "hbs2:mailbox:message:create:multipart" $ \syn -> lift do

      sto <- getStorage
      let cms = CreateMessageServices
                  sto
                  ( runKeymanClientRO . loadCredentials )
                  ( runKeymanClientRO . loadKeyRingEntry )


      flagz   <- defMessageFlags
      tsender <- newTVarIO Nothing
      tbody   <- newTVarIO (mempty :: LBS.ByteString)
      trcpt   <- newTVarIO mempty
      tparts  <- newTVarIO mempty

      case syn of
         [ListVal (SymbolVal "dict" : parts)] -> do

          for_ parts $ \case
            ListVal [StringLike "sender", HashLike ss] ->  do
              atomically $ writeTVar tsender (Just ss)

            ListVal [StringLike "recipient", HashLike ss] ->  do
              atomically $ modifyTVar trcpt  (ss:)

            ListVal [StringLike "body", StringLike s] -> do
              let lbs = encodeUtf8 (fromString s) & LBS.fromStrict
              atomically $ modifyTVar tbody (LBS.append lbs)

            ListVal [StringLike "part", StringLike fn] -> do
              let what = takeFileName fn & fromString
              let rfn = liftIO (LBS.readFile fn)
              let meta = [("file-name:", what)]
              atomically $ modifyTVar tparts ( [(meta,rfn)] <> )

            _ -> pure ()

         _ -> throwIO (BadFormException @c nil)

      sender <- readTVarIO tsender >>= orThrowUser "sender not set"
      rcpt   <- readTVarIO trcpt <&> fmap Left
      body   <- readTVarIO tbody
      parts  <- readTVarIO tparts
      mess   <- createMessage cms flagz Nothing
                    (Left sender)
                    rcpt
                    parts
                    (LBS.toStrict body)

      mkOpaque (serialise mess)

  entry $ bindMatch "hbs2:mailbox:message:dump" $ nil_ \syn -> lift do
    lbs <- case syn of
            [ HashLike h ] -> do
              sto <- getStorage
              getBlock sto (coerce h) >>= orThrowUser "message not found"

            [ StringLike fn ] -> do
              liftIO $ LBS.readFile fn

            _ -> throwIO (BadFormException @c nil)

    let rms = ReadMessageServices ( liftIO . runKeymanClientRO . extractGroupKeySecret)

    (s,mess,co) <- deserialiseOrFail @(Message HBS2Basic) lbs
                    & orThrowUser "malformed message"
                    >>= readMessage rms

    -- TODO: implement-normally
    liftIO do
      print $ "sender" <+> pretty (AsBase58 s)

      for_ (messageRecipients mess)  $ \r -> do
        print $ "recipient" <+> pretty (AsBase58 r)

      for_ (messageParts mess) $ \p -> do
        print $ "attachment" <+> pretty p

      putStrLn ""

      BS.putStr co

  entry $ bindMatch "hbs2:mailbox:message:read:file" $ nil_ \case
    [StringLike s] -> lift do
      sto <- getStorage
      let rms = ReadMessageServices ( liftIO . runKeymanClientRO . extractGroupKeySecret)

      (s,_,bs)  <- liftIO (LBS.readFile s)
                     <&> deserialiseOrFail @(Message HBS2Basic)
                     >>= orThrowUser "invalid message format"
                     >>= readMessage rms

      liftIO $ BS.putStr bs

    _ -> throwIO (BadFormException @c nil)



  entry $ bindMatch "hbs2:mailbox:message:read:storage" $ nil_ \case
    [HashLike h] -> lift do
      sto <- getStorage
      let rms = ReadMessageServices ( liftIO . runKeymanClientRO . extractGroupKeySecret)

      (s,_,bs) <- getBlock sto (coerce h)
                    >>= orThrowUser "message not found"
                      <&> deserialiseOrFail @(Message HBS2Basic)
                      >>= orThrowUser "invalid message format"
                      >>= readMessage rms

      liftIO $ BS.putStr bs

    _ -> throwIO (BadFormException @c nil)


  entry $ bindMatch "hbs2:mailbox:policy:basic:read:syntax" $ \case
    [ListVal syn] -> do
      po <- parseBasicPolicy syn >>= orThrowUser "malformed policy"
      mkOpaque po

    _ -> throwIO (BadFormException @c nil)

  entry $ bindMatch "hbs2:mailbox:policy:basic:read:file" $ \case
    [StringLike fn] -> lift do

      what <- liftIO (readFile fn)
               <&> parseTop
               >>= either (error.show) pure
               >>= parseBasicPolicy
               >>= orThrowUser "invalid policy"

      mkOpaque what

    _ -> throwIO (BadFormException @c nil)


  entry $ bindMatch "hbs2:mailbox:policy:basic:read:storage" $ \case
    [HashLike href] -> lift do
      sto <- getStorage
      what <- runExceptT (getTreeContents sto href)
                >>= orThrowPassIO
                <&> parseTop . LBS8.unpack
                >>= either (error.show) pure
                >>= parseBasicPolicy
                >>= orThrowUser "invalid policy"
      mkOpaque what

    _ -> throwIO (BadFormException @c nil)


  entry $ bindMatch "hbs2:mailbox:policy:basic:accept:peer" $ \case
    [SignPubKeyLike who, OpaqueVal box] -> lift do
      p <- fromOpaqueThrow @(BasicPolicy HBS2Basic) "expected BasicPolicy" box
      r <- policyAcceptPeer @HBS2Basic p who
      pure $ mkBool @c r

    _ -> throwIO (BadFormException @c nil)


  entry $ bindMatch "hbs2:mailbox:policy:basic:accept:sender" $ \case
    [SignPubKeyLike who, OpaqueVal box] -> lift do
      p <- fromOpaqueThrow @(BasicPolicy HBS2Basic) "expected BasicPolicy" box
      r <- policyAcceptSender @HBS2Basic p who
      pure $ mkBool @c r

    _ -> throwIO (BadFormException @c nil)


  entry $ bindMatch "hbs2:mailbox:policy:basic:dump" $ nil_ $ \case
    [OpaqueVal box] -> lift do
      p <- fromOpaqueThrow @(BasicPolicy HBS2Basic) "expected BasicPolicy" box
      liftIO $ print $ vcat (fmap pretty (getAsSyntax @c p))

    _ -> throwIO (BadFormException @c nil)

