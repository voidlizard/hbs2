module HBS2.CLI.Run.Mailbox where


import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Net.Auth.GroupKeySymm
import HBS2.Peer.Proto.Mailbox

import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Storage
import HBS2.KeyMan.Keys.Direct as K

import Codec.Serialise
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Coerce
import Data.Either

createMessageFromByteString :: forall s m . ( MonadUnliftIO m
                                            , s ~ HBS2Basic
                                            , HasStorage m
                                            )
                            => LBS8.ByteString
                            -> m (Message s)
createMessageFromByteString lbs = do
  let ls0 = LBS8.lines lbs
  let (hbs, rest1) = break LBS8.null ls0
  let payload      = dropWhile LBS8.null rest1 & LBS8.unlines
  let headers      = parseTop (LBS8.unpack (LBS8.unlines hbs)) & fromRight mempty

  flagz <- defMessageFlags

  sender <-  headMay [ Left s | ListVal [SymbolVal "sender:", HashLike s] <- headers ]
               & orThrowUser "sender not defined"

  let rcpts = [ Left s | ListVal [SymbolVal "recipient:", HashLike s] <- headers ]

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
    $ entry $ bindMatch "hbs2:mailbox:message:create" $ \case
        [StringLike fn] -> lift do
          lbs <- liftIO $ LBS8.readFile fn
          mess <- createMessageFromByteString lbs
          let what = serialise mess
          pure $ mkForm @c "blob" [mkStr (LBS8.unpack what)]

        _ -> throwIO (BadFormException @c nil)


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

