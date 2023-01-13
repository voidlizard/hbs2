{-# Language FunctionalDependencies #-}
module HBS2.Net.Messaging where

import Control.Monad.IO.Class
import Data.Kind
import Prettyprinter

newtype From a = From a
                 deriving newtype (Eq,Show,Pretty)

newtype To a = To a
               deriving newtype (Eq,Show,Pretty)


class Messaging bus addr msg  | bus -> addr, bus -> msg where

  sendTo  :: MonadIO m => bus -> To addr -> From addr -> msg -> m ()
  receive :: MonadIO m => bus -> To addr -> m [msg]




