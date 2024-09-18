module HBS2.CLI.Run.Internal.Merkle where

import HBS2.CLI.Prelude
import HBS2.Defaults
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.GroupKey as G

import HBS2.Hash
import HBS2.Net.Auth.GroupKeySymm as Symm
import HBS2.Data.Types.Refs
import HBS2.Data.Detect
import HBS2.Merkle
import HBS2.Storage
import HBS2.Storage.Operations.ByteString
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.API.Storage
import HBS2.KeyMan.Keys.Direct

import HBS2.Net.Auth.Schema()

import Codec.Serialise
import Data.Coerce
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as Text
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Control.Monad.Except


--FIXME: move-somewhere-else
getGroupKeyHash :: ( MonadUnliftIO m
                   , HasStorage m
                   , HasClientAPI StorageAPI UNIX m
                   )
                => HashRef
                -> m (Maybe HashRef, MTreeAnn [HashRef])
getGroupKeyHash h = do
  flip runContT pure do
    sto <- lift getStorage

    headBlock <- getBlock sto (fromHashRef h)
                   >>= orThrow MissedBlockError
                   <&> deserialiseOrFail @(MTreeAnn [HashRef])
                   >>= orThrow UnsupportedFormat

    case _mtaCrypt headBlock of
      (EncryptGroupNaClSymm hash _) ->
        pure $ (Just $ HashRef hash, headBlock)
      _ -> pure (Nothing, headBlock)

-- TODO: client-api-candidate
createTreeWithMetadata :: (MonadUnliftIO m)
                       => AnyStorage
                       -> Maybe (GroupKey 'Symm 'HBS2Basic)
                       -> HashMap Text Text
                       -> LBS.ByteString
                       -> m (Either OperationError HashRef)
createTreeWithMetadata sto mgk meta lbs = do -- flip runContT pure do

    let mt = vcat [ pretty k <> ":" <+> dquotes (pretty v) | (k,v) <- HM.toList meta ]
               & show & Text.pack

    case mgk of
      Nothing -> Right <$> createSimpleTree mt
      Just gk -> createEncryptedTree gk mt

  where
    createSimpleTree mt = do
        t0 <- writeAsMerkle sto lbs
                >>= getBlock sto
                >>= orThrowUser "can't read merkle tree just written"
                <&> deserialiseOrFail @(MTree [HashRef])
                >>= orThrowUser "merkle tree corrupted/invalid"

        let mann = MTreeAnn (ShortMetadata mt) NullEncryption t0

        putBlock sto (serialise mann)
          >>= orThrowUser "can't write tree"
          <&> HashRef

    -- FIXME: support-encryption
    createEncryptedTree gk mt = do
      -- 1. find key
      mgks <- runKeymanClient do
                extractGroupKeySecret gk

      gks <- orThrowUser "can't get groupkey's secret" mgks

      -- FIXME: consider-other-nonce-calculation
      --   надо считать начальный нонс (от чего / как?)
      --   нонс: да так-то пофиг от чего, но:
      --     если брать рандомные места в байтстроке --
      --     она зафорсится
      --     что вообще зависит от начального нонса:
      --       если в файл будет допись в конец, то
      --       "старые" блоки останутся такими же, как были
      --       что хорошо для дедуплицирования, но
      --       потенциально это менее безопасно.
      --   можно еще с метаданными похэшировать, тогда
      --   нонс будет более уникальный; но поменялись метаданные -- поменялось всё
      let s0 = LBS.take ( 1024 * 1024 ) lbs

      let (HbSyncHash nonce) = hashObject @HbSync s0
      -- куда-то девать зашифрованные метаданные
      --
      let segments = readChunkedBS lbs defBlockSize

      seb <- G.encryptBlock sto gk (ShortMetadata mt)

      hmeta <- putBlock sto (serialise seb)
                 >>= orThrowUser "can't put block"

      let source = ToEncryptSymmBS gks (Right gk) nonce segments  (AnnHashRef hmeta) Nothing

      runExceptT $ writeAsMerkle sto source <&> HashRef


getTreeContents :: forall m . ( MonadUnliftIO m
                              , MonadIO m
                              , MonadError OperationError m
                   )
                => AnyStorage
                -> HashRef
                -> m LBS.ByteString

getTreeContents sto href = do

  blk <- getBlock sto (coerce href)
           >>= orThrowError MissedBlockError

  let q = tryDetect (coerce href) blk

  case q of

    Merkle _ -> do
      readFromMerkle sto (SimpleKey (coerce href))

    MerkleAnn (MTreeAnn {_mtaCrypt = NullEncryption }) -> do
      readFromMerkle sto (SimpleKey (coerce href))

    MerkleAnn ann@(MTreeAnn {_mtaCrypt = EncryptGroupNaClSymm gkh _}) -> do

      rcpts  <- Symm.loadGroupKeyMaybe @'HBS2Basic sto (HashRef gkh)
                  >>= orThrowError (GroupKeyNotFound 11)
                  <&> HM.keys . Symm.recipients

      let findStuff g = do
            runKeymanClientRO @IO $ findMatchedGroupKeySecret sto g

      readFromMerkle sto (ToDecryptBS (coerce href) (liftIO . findStuff))

    _ -> throwError UnsupportedFormat

