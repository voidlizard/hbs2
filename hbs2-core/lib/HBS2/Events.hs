{-# Language FunctionalDependencies #-}
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

class Monad m => HasEvents e a m | a -> e where

  data family EventKey e a :: Type
  type family Event e a    :: Type

  subscribe  :: EventKey e a -> Event e a -> m ()


