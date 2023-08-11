module SignalHandlers where

import Control.Exception (Exception, toException)
import Control.Monad
import System.Mem.Weak (deRefWeak)
import System.Posix.Signals
import UnliftIO.Concurrent

newtype SignalException = SignalException Signal
  deriving (Show)
instance Exception SignalException

installSignalHandlers :: IO ()
installSignalHandlers = do
  main_thread_id <- myThreadId
  weak_tid <- mkWeakThreadId main_thread_id
  forM_ [ sigHUP, sigTERM, sigUSR1, sigUSR2, sigXCPU, sigXFSZ ] $ \sig ->
    installHandler sig (Catch $ send_exception weak_tid sig) Nothing
  where
    send_exception weak_tid sig = do
      m <- deRefWeak weak_tid
      case m of
        Nothing  -> return ()
        Just tid -> throwTo tid (toException $ SignalException sig)

