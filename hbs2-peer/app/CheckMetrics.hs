module CheckMetrics where

import HBS2.Prelude.Plated
import HBS2.Clock

import PeerLogger

import Control.Monad
import System.Metrics
import Data.HashMap.Strict qualified as HashMap


checkMetrics :: MonadIO m => Store -> m ()
checkMetrics store = do

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
    for_ me $ \(k,v) -> do
      let vv = case v of
                 Gauge x   -> pretty x
                 Counter x -> pretty x
                 other     -> pretty (show other)

      debug $ "metric" <+> pretty k <> colon <+> vv

