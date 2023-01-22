{-# Language FunctionalDependencies #-}
module HBS2.Net.Proto.Sessions where

import HBS2.Net.Proto.Types

import Data.Typeable
import Data.Dynamic
import Data.Hashable
import Data.Kind

data SKey = forall a . (Unkey a, Eq a, Hashable a) => SKey (Proxy a) Dynamic

class Typeable a => Unkey a where
  unKey :: Proxy a -> Dynamic -> Maybe a

instance Typeable a => Unkey a where
  unKey _ = fromDynamic @a

newSKey :: forall a . (Eq a, Typeable a, Unkey a, Hashable a) => a -> SKey
newSKey s = SKey (Proxy @a) (toDyn s)


instance Hashable SKey where
  hashWithSalt s (SKey p d) = hashWithSalt s (unKey p d)


instance Eq SKey where
  (==) (SKey p1 a) (SKey p2 b) = unKey p1 a == unKey p1 b


data family SessionKey  e p :: Type
type family SessionData e p :: Type


class ( Monad m
      , HasProtocol e p
      , Eq (SessionKey e p)
      , Hashable (SessionKey e p)
      , Typeable (SessionData e p)
      ) => Sessions e p m  | p -> e where



  -- | Session find function.
  -- | It is useful when we want to check if session even
  -- | exists and it will not start a new session.

  find :: SessionKey e p           -- ^ session key
       -> (SessionData e p -> a)  -- ^ modification function, i.e. lens
       -> m (Maybe a)

  -- | Session fetch function.
  -- | It will insert a new session, if default value is Just something.

  fetch  :: Bool                     -- ^ do add new session if not exists
         -> SessionData e p          -- ^ default value in case it's not found
         -> SessionKey e p           -- ^ session key
         -> (SessionData e p -> a )  -- ^ modification function, i.e. lens
         -> m a

  -- | Session update function
  -- | If will create a new session if it does not exist.
  -- | A modified value (or default) value will we saved.

  update :: SessionData e p                      -- ^ default value in case it's not found
         -> SessionKey e p                       -- ^ session key
         -> (SessionData e p -> SessionData e p) -- ^ modification function, i.e. lens
         -> m ()

  expire :: SessionKey e p -> m ()

