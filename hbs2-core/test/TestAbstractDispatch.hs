module TestAbstractDispatch where


import HBS2.Prelude
-- import HBS2.Net.Messaging

import Data.Maybe
import Data.ByteString (ByteString)
-- concrete type or typeclass ?
import Data.Kind
import Data.Word
import Data.Map (Map)
import Data.Map qualified as Map
-- import Data.Proxy

newtype Cookie = Cookie Word32
                 deriving stock (Eq,Ord)

data MessageWithCookie = MessageWithCookie Cookie ByteString

class IsMessageWithCookie msg where
  encode :: msg -> MessageWithCookie
  decode :: MessageWithCookie -> Maybe msg


-- ЧТО МЫ ХОТИМ:
-- СООБЩЕНИЯ РАЗНЫХ ТИПОВ, ДОБАВЛЯТЬ НЕЗАВИСИМО
--
-- ЗАЧЕМ: ХУЙ ЗНАЕТ НА САМОМ ДЕЛЕ
-- НО, ДОПУСТИМ, ХОТИМ НЕЗАВИСИМЫЕ ОБРАБОТЧИКИ ДЛЯ
-- КАЖДОГО ТИПА СООБЩЕНИЯ
--
-- ПОЧЕМУ МЫ ЭТОГО ХОТИМ:
--
--   ЧТО БЫ НЕ ДЕЛАТЬ ОДИН СЛОЖНЫЙ ОБРАБОТЧИК С БОЛЬШИМ СТЕЙТОМ
--   ТАКОЕ УЖЕ ДЕЛАЛИ, ТАМ ХУЙ НОГУ СЛОМИТ ПОТОМ ОТЛАЖИВАТЬ
--   НАДО СДЕЛАТЬ, ЧТО БЫ МОЖНО БЫЛО ОТЛАДИТЬ ПО КУСКАМ
--   ТЕМ БОЛЕЕ, ЧТО ОНИ ДРУГ ОТ ДРУГА НЕ ЗАВИСЯТ
--
-- КАК-ТО ОБЕСПЕЧИМ УНИКАЛЬНОСТЬ КУКИ ДЛЯ КАЖДОГО ТИПА
--
--


data Handler  = forall a . Show a => Handler
                                     { decodeMsg :: ByteString -> Maybe a
                                     , dispatch  :: a -> ()
                                     }

newtype Dispatcher = Dispatcher { actions :: Map Cookie Handler }

-- sendRequest :: forall m msg . (MonadIO m, IsEncoded msg, HasCookie msg) => Dispatcher m -> msg  -> m ()
sendRequest :: forall m msg . (MonadIO m, IsMessageWithCookie msg) => msg -> m ()
sendRequest msg = do
  let coockoo = encode msg
  let wtfmap = mempty :: Map Cookie ()

  -- send (serialize coockoo)

  undefined

dispatcher :: MonadIO m => Dispatcher -> m ()
dispatcher d = do
  -- получили нечто
  -- вытащили куку

  -- КАК БЛЯДЬ? МЫ НЕ ЗНАЕМ ТУТ, ЧТО ЭТО ЗА СООБЩЕНИЕ. ЭТО ЧТО - ПОЛУДЕКОДИРОВАННОЕ
  -- СООБЩЕНИЕ)
  let (MessageWithCookie coo bs)  = undefined :: MessageWithCookie

  -- поискали в мапе по куке
  let found' = Map.lookup coo (actions d)

  -- нашли обработчик
  let found = fromJust found'

  -- декодировали сообщение и пихнули в обработчик
  let _ = let (Handler de dispatch) = found in dispatch (fromJust (de bs))

  -- сработало: ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  --

  -- мы не знаем конкретный тип сообщения и нам пох. обработчик обрабатывает,
  -- мы тут обеспечиваем общий механизм диспечеризации: заводим сессии, ждём
  -- прибиваем сессии, рулим куками

  undefined


testAbstractDispatch :: IO ()
testAbstractDispatch = do
  pure ()

