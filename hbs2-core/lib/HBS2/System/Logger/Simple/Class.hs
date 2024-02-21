{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language TypeFamilyDependencies #-}
module HBS2.System.Logger.Simple.Class where

import GHC.TypeLits
import Data.Proxy

class KnownNat (LogLevel p) => HasLogLevel p  where
  type family LogLevel p = (id :: Nat) | id -> p

  logKey :: Int
  logKey = fromIntegral $ natVal (Proxy :: Proxy (LogLevel p))

data TRACE
data DEBUG
data INFO
data ERROR
data WARN
data NOTICE

instance HasLogLevel TRACE where
  type instance LogLevel TRACE = 1

instance HasLogLevel DEBUG where
  type instance LogLevel DEBUG = 2

instance HasLogLevel INFO where
  type instance LogLevel INFO = 3

instance HasLogLevel ERROR where
  type instance LogLevel ERROR = 4

instance HasLogLevel WARN where
  type instance LogLevel WARN = 5

instance HasLogLevel NOTICE where
  type instance LogLevel NOTICE = 6



