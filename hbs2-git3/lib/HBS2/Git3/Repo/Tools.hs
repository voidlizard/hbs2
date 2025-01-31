{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git3.Repo.Tools where

import HBS2.Git3.Prelude
import HBS2.Git3.State
import HBS2.Git3.Repo.Types

import HBS2.Data.Detect
import HBS2.System.Dir
import HBS2.Net.Auth.GroupKeySymm
import HBS2.CLI.Run.MetaData
import HBS2.Net.Auth.Credentials
import HBS2.KeyMan.Keys.Direct

import HBS2.Git.Local.CLI

import Data.Config.Suckless.Script

import Control.Applicative
import Crypto.Bip39
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.Either
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Maybe
import Data.Text qualified as Text
import Data.Word
import Lens.Micro.Platform
import System.Random hiding (next)

import Streaming.Prelude qualified as S

{- HLINT ignore "Functor law" -}

listRemotes :: MonadIO m => m [(GitRef, GitRepoKey)]
listRemotes = do

  git <- findGitDir >>= orThrow NoGitDir

  conf <- liftIO (readFile (git </> "config"))
            <&> parseTop
            <&> fromRight mempty

  let urls = flip fix (mempty,Nothing,conf) $ \next -> \case
       (acc,_, ListVal [SymbolVal "remote", StringLike x] : rest) ->
         next (acc,Just x, rest)

       (acc, Just x, ListVal [SymbolVal "url", _, RepoURL3 u] : rest) ->
         next ( (fromString x, u) : acc, Nothing, rest)

       (acc, x, _ : rest) -> next ( acc, x, rest)

       (acc,_,[])     -> acc

  pure  urls

resolveRepoKeyThrow :: MonadIO m => [Syntax C] -> m GitRepoKey
resolveRepoKeyThrow = \case
  [ SignPubKeyLike url ] -> pure url
  [ RepoURL url ]  -> pure url
  [ StringLike x ] -> do
    refs <- listRemotes
    lookup (fromString x) refs &  orThrow (GitRemoteKeyNotResolved x)
  x -> throwIO (GitRemoteKeyNotResolved (show $ pretty (mkList x)))

newRemoteName :: MonadIO m => GitRepoKey -> m GitRef
newRemoteName key = do
  refs <- listRemotes <&> HM.fromList

  flip fix Nothing $ \again i -> do

    when (i > Just 128) $ throwIO GitCantGenerateRemoteName

    suff <- case i of
              Nothing -> pure mempty
              Just _ -> do
                p <- randomIO @Word8 <&> Text.pack . show
                pure $ "-" <> p

    name <- toMnemonic (LBS.toStrict . LBS.drop 8 $ serialise key)
                &   orThrow GitCantGenerateRemoteName
                <&> Text.intercalate "-" . take 2 . Text.words
                <&> (<> suff)
                <&> fromString @GitRef . Text.unpack

    if not (HM.member name refs) then pure name
    else again (succ <$> ( i <|> Just 0) )

updateRepoHead :: (HBS2GitPerks m)
               => GitRepoKey
               -> [Syntax C]
               -> [HashRef]
               -> Git3 m ()
updateRepoHead repo manifest gkRefs' = do

  debug "updateRepoHead"

  sto <- getStorage
  lwwAPI <- getClientAPI @LWWRefAPI @UNIX

  creds <- liftIO $ runKeymanClientRO (loadCredentials repo)
               >>= orThrow GitRepoNoAccess

  let (wsk, wpk) = (view peerSignSk creds, view peerSignPk creds)

  let mfs = vcat $ fmap pretty manifest
  manifestTree <- createTreeWithMetadata sto Nothing mempty (LBS8.pack (show mfs))
                     >>= orThrowPassIO

  lwwRef <- getRepoRefMaybe

  let rHeadOld = lwwValue <$> lwwRef

  repoHead <- maybe (pure mempty) (readLogThrow (getBlock sto)) rHeadOld

  oldKeys <- fromMaybe mempty <$> runMaybeT do
               h <- headMay (tailSafe repoHead) & toMPlus
               readLogThrow (getBlock sto) h

  let gkRefs = HS.toList $ HS.fromList (gkRefs' <> oldKeys)

  gkTree <- if null gkRefs
              then pure Nothing
              else do
                tree <- makeMerkle 0 (toPTree (MaxSize defHashListChunk) (MaxNum defTreeChildNum) gkRefs)
                           (\(_,_,bs) -> void $ putBlock sto bs)
                pure $ Just (HashRef tree)

  let refs = manifestTree : maybeToList gkTree

  blk <- makeMerkle 0 (toPTree (MaxSize defHashListChunk) (MaxNum defTreeChildNum) refs)
         (\(_,_,bs) -> void $ putBlock sto bs)

  now <- liftIO getPOSIXTime <&> round
  let box = makeSignedBox wpk wsk (LWWRef now (coerce blk) Nothing)

  callRpcWaitMay @RpcLWWRefUpdate (TimeoutSec 1) lwwAPI box
    >>= orThrow RpcTimeout

updateGroupKey :: HBS2GitPerks m => GitRepoKey -> HashRef -> Git3 m ()
updateGroupKey repo new = do
  waitRepo (Just 10) repo
  sto <- getStorage
  LWWRef{..} <- getRepoRefMaybe >>= orThrow GitRepoRefEmpty
  RepoManifest manifest <- getRepoManifest
  deps <- readLogThrow (getBlock sto) (coerce lwwValue)

  gkNew <- loadGroupKeyMaybe @HBS2Basic sto new
              >>= orThrow  (GitRepoNoGroupKey new)

  flip runContT pure do

    gkth <- ContT $ maybe1 (headMay (tailSafe deps)) none
    gkTree <- readLogThrow (getBlock sto) gkth

    r <- newTVarIO mempty
    lift $ for_ gkTree $ \gh -> void $ runMaybeT do
      gk <- loadGroupKeyMaybe @HBS2Basic sto gh >>= toMPlus
      gkId <- groupKeyId gk & toMPlus
      liftIO $ print $ "gk" <+> pretty gh <+> pretty gkId

      let missed = HM.keysSet $ recipients gkNew `HM.difference` recipients gk

      unless (HS.null missed) do
        atomically $ modifyTVar r (HM.insert gkId (gk,missed))
        none

    keys <- readTVarIO r <&> HM.elems

    oldKeys <- liftIO $ runKeymanClientRO do
      for keys $ \(gk, missed) -> do
        -- FIXME: what-if-no-access?
        --   предполагается, что это делает owner.
        --   owner имеет все ключи.
        --   но это может сделать и чел, который просто имеет все ключи,
        --   но нет, т.к. он не сможет записать lwwref
        gks <- extractGroupKeySecret gk >>= orThrow GitRepoNoAccess
        gkNew <- generateGroupKey @'HBS2Basic (Just gks) (HS.toList missed)
        writeAsMerkle sto (serialise gkNew) <&> HashRef

    let newKeys = HS.toList $ HS.fromList ( new : oldKeys )

    for_ newKeys $ \k -> do
      liftIO $ print $ "new gk" <+> pretty k

    --
    let newManifest0 = flip mapMaybe manifest $ \case
                        ListVal [StringLike "gk", HashLike x]  -> Nothing
                        x ->  Just x
    let newManifest = newManifest0 <> [ mkForm "gk" [ mkSym (show $ pretty new) ] ]

    lift do
      updateRepoHead repo newManifest newKeys
      updateRepoKey repo

