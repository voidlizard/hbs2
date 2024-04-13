module HBS2.Data.KeyRing where

import HBS2.Prelude
import HBS2.Net.Proto.Types
import HBS2.Net.Auth.Credentials

import System.FilePattern.Directory
import System.FilePath
import System.Directory
import Data.List as L
import Data.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Lens.Micro.Platform
import UnliftIO
import Control.Monad.Trans.Maybe
import Data.HashSet qualified as HashSet


splitPattern :: FilePath -> (FilePath, FilePath)
splitPattern fp = (pref, flt)
  where
    pref = joinPath pref'
    flt = case flt' of
      [] -> "*"
      xs -> joinPath flt'
    (pref', flt') = L.span isNotP (splitDirectories fp)
    isNotP s = isNothing (find isP s)
    isP c = c `elem` ("?*" :: [Char])

findFilesBy :: MonadIO m => [FilePath] -> m [FilePath]
findFilesBy fp = liftIO do
  fs <- forConcurrently fp $ \p -> do
    isF <- liftIO $ doesFileExist p
    if isF then do
      pure [p]
    else do
      let (dir, pat) = splitPattern p
      fs <- liftIO $ getDirectoryFiles dir [pat]
      pure $ fmap (dir </>) fs

  pure $ nub $ mconcat fs

findKeyRing :: forall s m . ( MonadUnliftIO m
                            , SerialisedCredentials s
                            , ForHBS2Basic s
                            )
            => [FilePattern]
            -> PubKey 'Sign s
            -> m [FilePath]

findKeyRing fp kr = do

  allFiles <- findFilesBy fp

  kf <- forConcurrently allFiles $ \f -> do
    bs <- liftIO $ BS.readFile f
    let krf = parseCredentials @s (AsCredFile bs)
    let sk = view peerSignPk <$> krf

    if sk == Just kr then
      pure (Just f)
    else
      pure Nothing

  pure (catMaybes kf)

findKeyRingEntries :: forall s m . ( MonadUnliftIO m
                                   , SerialisedCredentials s
                                   , Hashable (PubKey 'Encrypt s)
                                   -- , ForHBS2Basic s
                                   )
                 => [FilePattern]
                 -> [PubKey 'Encrypt s]
                 -> m [KeyringEntry s]

findKeyRingEntries fp pkl = do

  let pks = HashSet.fromList pkl

  fs <- findFilesBy fp

  w <- for fs $ \f -> runMaybeT do
    bs <- liftIO (try @_ @IOException (BS.readFile f))
           >>= toMPlus
    krf <- parseCredentials (AsCredFile bs) & toMPlus
    MaybeT $ pure $ headMay [ e | e@(KeyringEntry pk _ _) <- _peerKeyring krf, pk `HashSet.member` pks ]

  pure $ catMaybes w

