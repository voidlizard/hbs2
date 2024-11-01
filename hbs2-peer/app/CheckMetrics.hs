module CheckMetrics where

import HBS2.Prelude.Plated

import PeerLogger

import System.Metrics
import Data.HashMap.Strict qualified as HashMap

import Streaming.Prelude qualified as S

checkMetrics :: MonadIO m => Store -> AnyProbe -> m ()
checkMetrics store probe = do

  liftIO $ registerGcMetrics store

  let supported = HashMap.fromList $ fmap (,()) [ "rts.gc.current_bytes_used"
                                                , "rts.gc.max_bytes_used"
                                                , "rts.gc.cpu_ms"
                                                , "rts.gc.num_gcs"
                                                , "rts.gc.bytes_allocated"
                                                ]

  forever do
    pause @'Seconds 30
    debug "checkMetrics"
    me <- liftIO $ sampleAll store <&> flip HashMap.intersection supported <&> HashMap.toList
    values <- S.toList_ $ for_ me $ \(k,v) -> do
      vv <- case v of
                 Gauge x   -> S.yield (k, fromIntegral x) >> pure (pretty x)
                 Counter x -> S.yield (k, fromIntegral x) >> pure (pretty x)
                 other     -> pure (pretty (show other))

      debug $ "metric" <+> pretty k <> colon <+> vv

    acceptReport probe values

