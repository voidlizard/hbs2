{-# Language TemplateHaskell #-}
module HBS2.ScheduledAction
  ( Scheduled
  , scheduleRunPeriod
  , defScheduled
  , runScheduled
  , schedule
  ) where

import HBS2.Prelude.Plated

import Prelude hiding (all)
import Data.Word
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Lens.Micro.Platform
import Control.Monad
import Data.List qualified as List

import Control.Exception qualified as E

import UnliftIO as U

-- NOTE: scheduled-action
--   держит список действий (IO ())
--   привязанных к временным "слотам" (секундам) с точностью до
--   секунды.
--   После наступления секунды --- выполняет список действий,
--   привязанных к слоту, удаляет действия, удаляет слот.
--   Полезно, что бы очищать данные, имеющие продолжительность
--   жизни -- всякие там кэши, хэшмапы и так далее.
--
--   В отличие от Cache, не знает о сути действий ничего,
--   кроме того, что это IO ().
--
--   Может быть (и должно, наверное) быть глобальным на
--   всё приложение

type SlotNum = Word64

data Scheduled =
  Scheduled
  { _scheduleRunPeriod  :: Timeout 'Seconds
  , slots               :: TVar (HashMap SlotNum [IO ()])
  }

makeLenses 'Scheduled

defScheduled :: MonadUnliftIO m => m Scheduled
defScheduled = Scheduled 10 <$> newTVarIO mempty

runScheduled :: MonadUnliftIO m => Scheduled -> m ()
runScheduled sch = forever do
  pause (view scheduleRunPeriod sch)

  now <- getTimeCoarse <&> toNanoSecs <&> (/ (1e9 :: Double)) . realToFrac <&> round

  expireds <- atomically do
    all   <- readTVar (slots sch) <&> HashMap.toList
    let (rest, expireds)  = List.partition ( (>now) . fst) all
    writeTVar (slots sch) (HashMap.fromList rest)
    pure expireds

  for_ expireds $ \(_, all) -> do
    for_ all $ \action -> do
      -- TODO: error-logging-maybe
      liftIO $ void $ action `E.catch` (\(_ :: E.ArithException) -> pure ())
                             `E.catch` (\(_ :: E.IOException)    -> pure ())
                             `E.catch` (\(_ :: E.SomeException)  -> pure ())

schedule :: forall a m . (MonadUnliftIO m, Integral a) => Scheduled -> a -> IO () -> m ()
schedule s ttl what = do
  now <- getTimeCoarse <&> toNanoSecs <&> (/ (1e9 :: Double)) . realToFrac <&> round
  let slot = now + fromIntegral ttl
  atomically $ modifyTVar (slots s) (HashMap.insertWith (<>) slot [what])



