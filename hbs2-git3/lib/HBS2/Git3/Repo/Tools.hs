{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git3.Repo.Tools where

import HBS2.Git3.Prelude
import HBS2.Git3.State
import HBS2.Git3.Repo.Types

import HBS2.System.Dir

import HBS2.Git.Local.CLI

import Data.Config.Suckless.Script

import Control.Applicative
import Crypto.Bip39
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.Either
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Data.Text qualified as Text
import Data.Word
import Lens.Micro.Platform
import System.Random hiding (next)


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


