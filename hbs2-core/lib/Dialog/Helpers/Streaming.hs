module Dialog.Helpers.Streaming where

import Control.Monad.Fix
import Data.ByteString qualified as BS
import Data.Int
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Streaming as S
import Streaming.Internal
import Streaming.Prelude (cons)
import Streaming.Prelude qualified as S
import UnliftIO.STM
import Prelude hiding (cons)

withEffects
    :: (Functor m, Functor f, s ~ Stream f m r)
    => (forall a. m a -> m a)
    -> s
    -> s
withEffects trans = fix \go -> \case
    Return r -> Return r
    Effect m -> Effect (trans (fmap go m))
    Step f -> Step (fmap go f)
{-# INLINEABLE withEffects #-}

withEffectsMay
    :: (Monad m, Functor f, s ~ Stream f m r)
    => r
    -> (forall a. m a -> m (Maybe a))
    -> s
    -> s
withEffectsMay d trans = fix \go -> \case
    Return r -> Return r
    Effect m -> Effect (fromMaybe (Return d) <$> trans (fmap go m))
    Step f -> Step (fmap go f)
{-# INLINEABLE withEffectsMay #-}

stopOnLeft
    :: (Monad m)
    => (a -> Either r b)
    -> Stream (Of a) m r
    -> Stream (Of b) m r
stopOnLeft f = fix \go -> \case
    Return r -> Return r
    Effect m -> Effect (go <$> m)
    Step (a :> s) -> either Return (\b -> Step (b :> go s)) (f a)
{-# INLINEABLE stopOnLeft #-}

stopAfterLeftMay
    :: (Monad m)
    => (a -> Either (Maybe b, r) b)
    -> Stream (Of a) m r
    -> Stream (Of b) m r
stopAfterLeftMay f = fix \go -> \case
    Return r -> Return r
    Effect m -> Effect (go <$> m)
    Step (a :> s) -> either
          (\(mb, r) -> maybe
                  (Return r)
                  (\b -> Step (b :> Return r))
                  mb)
          (\b -> Step (b :> go s))
          (f a)
{-# INLINEABLE stopAfterLeftMay #-}

stopAfter
    :: (Monad m)
    => (a -> Maybe r)
    -> Stream (Of a) m r
    -> Stream (Of a) m r
stopAfter f = fix \go -> \case
    Return r -> Return r
    Effect m -> Effect (go <$> m)
    Step (a :> s) -> Step (a :> (maybe (go s) Return (f a)))
{-# INLINEABLE stopAfter #-}

headEither
    :: (Monad m)
    => Stream (Of a) m r
    -> m (Either r a)
headEither = fix \go -> \case
    Return r -> pure (Left r)
    Effect ms -> go =<< ms
    Step (a :> _) -> pure (Right a)
{-# INLINEABLE headEither #-}

