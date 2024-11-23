module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable as F
import Data.Function
import Data.List qualified as List
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC

import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Merkle
import HBS2.Merkle.Walk
import HBS2.Tests.MapStore
import HBS2.Tests.StoreAsMerkle

mapStoreAsMerkle
    :: (Monad m, StoreAsMerkle (MapStoreT m) (Hash HbSync) LBS.ByteString a)
    => a
    -> MapStoreT m MerkleHash
mapStoreAsMerkle = storeAsMerkle mapStorePutBlock

li :: (Monoid (t a), Applicative t) => a -> Writer (t a) ()
li = tell . pure

fromRightOrM :: (Applicative m) => m b -> Either a b -> m b
fromRightOrM ml = either (const ml) pure

main :: IO ()
main = defaultMain $ testGroup "walk conditional" $ execWriter do
    li $ HU.testCaseSteps "streamMerkleConditional simple" \step' -> do
        flip evalStateT (mempty :: Set (Hash HbSync)) $ runMapStoreT do
            let step = lift . lift . step'
            step "Reading elems:"
            MerkleHash h <- mapStoreAsMerkle $ [payload]

            (either pure (lift . lift . assertFailure . const "Expected `Left`") <=< streamToListEither) do
                streamMerkleConditionalEither @String mapStoreReadBlock (pure . const True) h

            ws <-
                onLeft' (\a -> assertFailure ("Expected `Right` but got " <> show a))
                    =<< streamToListEither do
                        streamMerkleConditionalEither @[HashRef] mapStoreReadBlock (pure . const True) h
            step . show $ ws
            r <- mapM (mapStoreReadBlock . fromHashRef) $ foldWalkSteps ws
            step . show $ r
            lift . lift $ payload @=? (mconcat . catMaybes) r

    li $ HU.testCaseSteps "streamMerkleConditional with caching" \step' -> do
        flip evalStateT (mempty :: Set (Hash HbSync)) $ runMapStoreT do
            let step = lift . lift . step'
            MerkleHash h1 <- mapStoreAsMerkle $ L (batch1 :: [Int])

            MerkleHash h2 <- mapStoreAsMerkle $ T [L (batch1 :: [Int]), L batch2, L batch3]

            step "Reading single branch:"
            ws1 <-
                onLeft' (\a -> assertFailure ("Expected `Right` but got " <> show a))
                    =<< streamToListEither do
                        streamMerkleConditionalEither @[Int] mapStoreReadBlock (pure . const True) h1
            step . show $ ws1

            let
                isKnownHash :: Hash HbSync -> MapStoreT (StateT (Set (Hash HbSync)) IO) Bool
                isKnownHash = lift . gets . Set.member
            let
                markHashAsKnown :: Hash HbSync -> MapStoreT (StateT (Set (Hash HbSync)) IO) ()
                markHashAsKnown = lift . modify' . Set.insert

            step "Mark known hashes:"
            mapM_ (withWalkProcessedTree markHashAsKnown) ws1

            step "Skipping known:"
            wsPartial <-
                onLeft' (\a -> assertFailure ("Expected `Right` but got " <> show a))
                    =<< streamToListEither do
                        streamMerkleConditionalEither @[Int] mapStoreReadBlock (fmap not . isKnownHash) h2
            step . show $ wsPartial
            step . show $ foldWalkSteps wsPartial
            lift . lift $ foldWalkSteps wsPartial @?= mconcat [batch2, batch3]

            step "Reading everything:"
            wsFull <-
                onLeft' (\a -> assertFailure ("Expected `Right` but got " <> show a))
                    =<< streamToListEither do
                        streamMerkleConditionalEither @[Int] mapStoreReadBlock (pure . const True) h2
            step . show $ wsFull
            step . show $ foldWalkSteps wsFull
            lift . lift $ foldWalkSteps wsFull @?= mconcat [batch1, batch2, batch3]
  where
    onLeft' f ea = fromRightOrM (lift . lift . f $ ea) ea

    batch1 = [1 .. 4]
    batch2 = [5 .. 8]
    batch3 = [9 .. 12]

    payload :: LBS.ByteString
    payload = "payload"

    foldWalkSteps :: [WalkStep' (Hash HbSync) [a]] -> [a]
    foldWalkSteps = mconcat . mconcat . fmap F.toList
