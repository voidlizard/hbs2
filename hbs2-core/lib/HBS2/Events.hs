{-# Language FunctionalDependencies #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Events where

import Data.Kind

-- General Events class.
--
-- It's may be way too general.
--
-- e is an 'engine' or 'program' or whatever.
-- If we will not introduce 'e' type,
-- there will be only one type of event
-- per "protocol" (a ) or a any concrete type per program.
--
-- Assume we need different type of event for testing
-- purposes and for production or so on.
--
-- For this situation we introduce 'e', that
-- allow us to do so.
--
-- In our case, e corresponds to an 'engine' that is
-- a concrete network fabrique implementations (UDP, TCP, whatever)
-- that could be implemented with 'e'.
--
-- And 'a' is corresponding to a concrete sub-protocol.
--
-- I suspect that 'e' has a global meaning and
-- represent an 'interpreter'.

data family EventKey e a :: Type
data family Event e a    :: Type

type EventHandler e a m = Event e a -> m ()

class Monad m => EventListener e a m | a -> e where
  subscribe  :: EventKey e a -> EventHandler e a m -> m ()

class Monad m => EventEmitter e a m | a -> e where
  emit  :: EventKey e a -> Event e a ->  m ()

class EventType a  where
  isPersistent :: Bool
  isPersistent = False

instance {-# OVERLAPPABLE #-} EventType any where
  isPersistent = False

