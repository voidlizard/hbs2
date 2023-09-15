{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Dialog.Client where

-- import System.Clock
-- import System.Timeout
import Codec.Serialise
import Control.Arrow
import Control.Exception qualified as Exception
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Error.Class
import Control.Monad.Except (ExceptT(..), runExcept, runExceptT)
import Control.Monad.IO.Unlift
import Control.Monad.State.Class as State
import Control.Monad.State.Strict (evalState, evalStateT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as BSL
import Data.Default
import Data.Generics.Labels
import Data.Generics.Product.Fields
import Data.List qualified as List
import Data.Map.Strict as Map
import Data.String.Conversions (cs, ConvertibleStrings)
import Data.Time
import GHC.Generics(Generic)
import Lens.Micro.Platform
import Streaming as S
import Streaming.Prelude qualified as S
import UnliftIO.Exception
import UnliftIO.STM
import UnliftIO.Timeout

import Dialog.Core
import Dialog.Helpers.Streaming

---

dQuery_ :: MonadUnliftIO m
    => RequestParams
    -> DialogClient m peer
    -> peer
    -> Frames
    -> m ()
dQuery_ _par dcli peer rq =
    withClientQuery dcli & \dialf ->
        dialf peer rq' \_flow -> pure ()
  where
    rq' = rq & #unFrames %~ ([serialiseS routerSignature] <>)

--
dQuery1 :: (MonadUnliftIO m)
    => RequestParams
    -> DialogClient m peer
    -> peer
    -> Frames
    -> m Frames

dQuery1 par dcli peer rq = dQuery' par dcli peer rq \flow ->
    either (throwIO . DQuery1Error) (pure . view _2) =<< headEither flow

data DQuery1Error = DQuery1Error RequestResult
  deriving (Show)

instance Exception DQuery1Error

--
dQuery' :: MonadUnliftIO m
    => RequestParams
    -> DialogClient m peer
    -> peer
    -> Frames
    -> (Stream (Of (ResponseHeader, Frames)) m RequestResult -> m r)
    -> m r

dQuery' par dcli peer rq go =
    withClientQuery dcli & \dialf -> do
        dialf peer rq' \flow -> go $
              flow
            & withEffectsMay RequestTimeout (timeout' (requestParamsTimeout par))
            & S.map decodeHeader
            & stopAfterLeftMay (either
                  (\(merr, xs) -> Left (Nothing, RequestErrorBadResponse merr xs))
                  processResponseHeader
                )

  where

    processResponseHeader :: (ResponseHeader, Frames) ->
        Either
            (Maybe (ResponseHeader, Frames), RequestResult)
            (ResponseHeader, Frames)

    processResponseHeader rhxs@(rh, xs) = case ((responseStatusCode . respStatus) rh) of
        Success200 -> Left (Just rhxs, RequestDone)
        SuccessMore -> Right rhxs
        r@BadRequest400 -> Left (Nothing, (RequestFailure r xs))
        r@Forbidden403 -> Left (Nothing, (RequestFailure r xs))
        r@NotFound404 -> Left (Nothing, (RequestFailure r xs))

    rq' = rq & #unFrames %~ ([serialiseS routerSignature] <>)

timeout' :: MonadUnliftIO m => NominalDiffTime -> m a -> m (Maybe a)
timeout' = timeout . round . (* 10^6) . nominalDiffTimeToSeconds

--
decodeHeader :: Frames -> Either (BadResponse, Frames) (ResponseHeader, Frames)
decodeHeader = evalState do
    ex <- runExceptT cutFrameDecode'
    xs <- State.get
    pure $ ex
        & left ((, xs) . maybe ResponseInsufficientFrames ResponseParseError)
        & right (, xs)

data RequestParams = RequestParams
  { requestParamsTimeout :: NominalDiffTime
  }
  deriving (Generic)

instance Default RequestParams where
  def = RequestParams
      { requestParamsTimeout = 5
      }

data DialogClient m p = DialogClient
  { withClientQuery :: ClientQuery m p
  }

type ClientQuery m p = forall r .
       p
    -> Frames
    -> (Stream (Of Frames) m RequestResult -> m r)
    -> m r

withClient :: forall m p i r . MonadUnliftIO m
    => DClient m p i -> (DialogClient m p -> m r) -> m r
withClient dclient go = do
    callerID <- newCallerID

    requestIDtvar <- newTVarIO 1

    -- У обработчика получателя - своё окружение, в которое мы добавляем
    -- обработчики ответов на запросы по requestID
    requestResponseEnv <- newRequestResponseEnv

    let withClientQuery' :: ClientQuery m p
        withClientQuery' = \pid xs handleStream -> do
            requestID <- atomically $ stateTVar requestIDtvar (id &&& succ)

            ch <- newTQueueIO
            let useResponse = RequestResponseHandler @m do
                    atomically . writeTQueue ch
            let
                -- flow :: Stream (Of Frames) m RequestResult
                flow = S.repeatM (atomically (readTQueue ch))

            bracket_
                (setupRepHandler requestResponseEnv requestID useResponse)
                (clearRepHandler requestResponseEnv requestID)
                (do

                    clientSendProtoRequest dclient pid do
                        xs & addEnvelope
                            [ (BSL.toStrict . serialise) callerID
                            , (BSL.toStrict . serialise) requestID
                            ]

                    handleStream flow
                )

    -- Установить в окружении обработчик получателя с callerID
    let callerHandler = CallerHandler $ unFrames >>> \case

            requestIDRaw:xs -> do
                case deserialiseOrFail (BSL.fromStrict requestIDRaw) of
                    Left _ ->
                        -- Если не нашли, ничего не предпринимать
                        -- На этот вопрос уже не ждут ответа
                        pure ()
                    Right requestID -> do
                        mh <- findRepHandler requestResponseEnv requestID
                        forM_ mh \(RequestResponseHandler h) -> h (Frames xs)

            _ -> pure ()

    bracket_
        (setupCallerEnv (clientCallerEnv dclient) callerID callerHandler)
        (clearCallerEnv (clientCallerEnv dclient) callerID)
        (go (DialogClient {withClientQuery = withClientQuery'}))

