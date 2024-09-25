module HBS2.Net.Proto.Sessions where

import Data.Typeable
import Data.Dynamic
import Data.Hashable
import Type.Reflection
import Data.Kind

data SKey = forall a . (Unkey a, Eq a, Hashable a) => SKey !(Proxy a) !SomeTypeRep !Dynamic

class Typeable a => Unkey a where
  unKey :: Proxy a -> Dynamic -> Maybe a

instance Typeable a => Unkey a where
  unKey _ = fromDynamic @a
  {-# INLINE unKey #-}

newSKey :: forall a . (Eq a, Typeable a, Unkey a, Hashable a) => a -> SKey
newSKey s = SKey (Proxy @a) (someTypeRep (Proxy @a)) (toDyn s)
{-# INLINE newSKey #-}


instance Hashable SKey where
  hashWithSalt s (SKey p t d) = hashWithSalt s (t, unKey p d)


instance Eq SKey where
  (==) (SKey p1 ty1 a) (SKey _p2 ty2 b) = ty1 == ty2 && unKey p1 a == unKey p1 b


data family SessionKey  e p :: Type
type family SessionData e p :: Type


class ( Monad m
      -- , HasProtocol e p
      , Eq (SessionKey e p)
      , Hashable (SessionKey e p)
      , Typeable (SessionData e p)
      , Typeable (SessionKey e p)
      ) => Sessions e p m  where



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

