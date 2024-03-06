module HBS2.Peer.Prelude
  ( module HBS2.Prelude.Plated
  , module HBS2.Net.Auth.Credentials
  , defCookieTimeoutSec, defCookieTimeout
  ) where

import HBS2.Prelude.Plated
import HBS2.Net.Auth.Credentials

defCookieTimeoutSec :: Timeout 'Seconds
defCookieTimeoutSec = 7200

defCookieTimeout :: TimeSpec
defCookieTimeout = toTimeSpec defCookieTimeoutSec

