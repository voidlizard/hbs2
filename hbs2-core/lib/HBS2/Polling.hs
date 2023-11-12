module HBS2.Polling where

import HBS2.Prelude.Plated
import HBS2.Clock

import Data.Heap (Entry(..))
import Data.Heap qualified as Heap
import Data.Time.Clock
import Data.HashMap.Strict qualified as HashMap
import Lens.Micro.Platform
import Data.Function

data Polling =
  Polling
  { waitBefore   :: NominalDiffTime
  , waitOnEmpty  :: NominalDiffTime
  }

polling :: forall a m . (MonadIO m, Hashable a)
        => Polling
        -> m [(a, NominalDiffTime)]
        -> (a -> m ())
        -> m ()

polling o listEntries action = do

  -- FIXME: might-be-concurrent

  pause (TimeoutNDT (waitBefore o))

  now0 <- getTimeCoarse
  refs0 <- listEntries <&> fmap (set _2 now0) <&> HashMap.fromList

  fix (\next mon -> do
    now <- getTimeCoarse
    refs <- listEntries <&> HashMap.fromList
    let mon' = mon `HashMap.union`
                 HashMap.fromList [ (e, now + toTimeSpec (TimeoutNDT t))
                                  | (e, t) <- HashMap.toList refs
                                  ]

    let q = Heap.fromList [ Entry t e
                          | (e, t) <- HashMap.toList mon'
                          ]

    case Heap.uncons q of
      Just (Entry t r, _) | t <= now -> do
        action r
        next (HashMap.delete r mon')

      Just (Entry t _, _) | otherwise -> do
        pause (TimeoutTS (t - now))
        next mon'

      Nothing -> do
        pause (TimeoutNDT (waitOnEmpty o))
        next mon'

    ) refs0


