{-# LANGUAGE CPP #-}
module HBS2.Clock
  ( module HBS2.Clock
  , module System.Clock
  )where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Fixed
import Data.Int (Int64)
import Data.Proxy
import Data.Time
import Prettyprinter
import System.Clock
import Data.Time.Clock

data TimeoutKind = MilliSeconds | Seconds | Minutes | NomDiffTime | TS

data family Timeout ( a :: TimeoutKind )


newtype Wait a = Wait a
                 deriving newtype (Eq,Show,Pretty)

newtype Delay a = Delay a
                  deriving newtype (Eq,Show,Pretty)


class IsTimeout a where
  toNanoSeconds :: Timeout a -> Int64

  toMicroSeconds :: Timeout a -> Int
  toMicroSeconds x = fromIntegral $ toNanoSeconds x `div` 1000

  toTimeSpec    :: Timeout a -> TimeSpec
  toTimeSpec x = fromNanoSecs (fromIntegral (toNanoSeconds x))

toNominalDiffTime :: IsTimeout t => Timeout t -> NominalDiffTime
toNominalDiffTime = fromRational . (/ (10^6)) . fromIntegral . toMicroSeconds

class IsTimeout a => MonadPause a m where
  pause :: Timeout a -> m ()

instance (IsTimeout a, MonadIO m) => MonadPause a m where
  pause x = liftIO $ threadDelay (toMicroSeconds x)

instance HasResolution a => Pretty (Fixed a) where
  pretty = pretty . show

newtype instance Timeout 'MilliSeconds =
  TimeoutMSec (Fixed E9)
  deriving newtype (Eq,Ord,Num,Real,Fractional,Show,Pretty)

newtype instance Timeout 'Seconds =
  TimeoutSec (Fixed E12)
  deriving newtype (Eq,Ord,Num,Real,Fractional,Show,Pretty)

newtype instance Timeout 'Minutes =
  TimeoutMin (Fixed E9)
  deriving newtype (Eq,Ord,Num,Real,Fractional,Show,Pretty)

newtype instance Timeout 'NomDiffTime =
  TimeoutNDT NominalDiffTime
  deriving newtype (Eq,Ord,Num,Real,Fractional,Show,Pretty)

newtype instance Timeout 'TS =
  TimeoutTS TimeSpec
  deriving newtype (Eq,Ord,Num,Real,Show,Pretty)

instance Pretty NominalDiffTime where
  pretty = viaShow

instance Pretty TimeSpec where
  pretty = viaShow

instance IsTimeout 'MilliSeconds where
  toNanoSeconds (TimeoutMSec x) = round (x * 1e6)

instance IsTimeout 'Seconds where
  toNanoSeconds (TimeoutSec x) = round (x * 1e9)

instance IsTimeout 'Minutes where
  toNanoSeconds (TimeoutMin x) = round (x * 60 * 1e9)

instance IsTimeout 'NomDiffTime where
  toNanoSeconds (TimeoutNDT t) = round (realToFrac (nominalDiffTimeToSeconds t) * 1e9)

instance IsTimeout 'TS where
  toNanoSeconds (TimeoutTS s) = fromIntegral $ toNanoSecs s

class Expires a where
  expiresIn :: Proxy a -> Maybe (Timeout 'Seconds)

  -- FIXME: dangerous!
  expiresIn _ = Nothing

-- | Use coarse clock timer. This timer has 1ms resolution but is much
-- faster comparing to the ordinary one. Is used on Linux, on MacOS
-- provides ordinary one.
getTimeCoarse :: MonadIO m => m TimeSpec
#ifdef linux_HOST_OS
getTimeCoarse = liftIO $ getTime MonotonicCoarse
#else
getTimeCoarse = liftIO $ getTime Monotonic
#endif

