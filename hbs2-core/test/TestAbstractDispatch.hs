{-# Language FunctionalDependencies #-}
module TestAbstractDispatch where

import HBS2.Prelude
import HBS2.Net.Proto
import HBS2.Net.Messaging
import HBS2.Clock

import Control.Concurrent.Async
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Hashable
import Data.Kind
import Data.Proxy
import Data.Word
import Prettyprinter

import FakeMessaging

-- newtype Cookie = Cookie Word32
--                  deriving stock (Eq,Ord)
--                  deriving newtype Hashable

data family Cookie p :: Type

class Monad m => CookieGenerator p m where
  genCookie :: Hashable s => s -> m (Cookie p)

class Monad m => HasTimeout msg m where
  timeoutFor :: Proxy msg -> m (Timeout 'Seconds)

-- still okay
class IsEncoded p msg | msg -> p where
  data family Encoded  p :: Type
  encode :: msg -> Encoded p
  decode :: Encoded p -> Maybe msg

-- still okay
data MessageWithCookie p = MessageWithCookie (Cookie p) (Encoded p)

-- ЧТО МЫ ХОТИМ:
-- СООБЩЕНИЯ РАЗНЫХ ТИПОВ. ОБРАБАТЫВАТЬ НЕЗАВИСИМО, В РАЗНЫХ ОБРАБОТЧИКАХ
--
-- ПОЧЕМУ МЫ ЭТОГО ХОТИМ:
--
--   ЧТО БЫ НЕ ДЕЛАТЬ ОДИН СЛОЖНЫЙ ОБРАБОТЧИК С БОЛЬШИМ СТЕЙТОМ
--   ТАКОЕ УЖЕ ДЕЛАЛИ, ТАМ ХУЙ НОГУ СЛОМИТ ПОТОМ ОТЛАЖИВАТЬ
--   НАДО СДЕЛАТЬ, ЧТО БЫ МОЖНО БЫЛО ОТЛАДИТЬ ПО КУСКАМ
--   ТЕМ БОЛЕЕ, ЧТО ОНИ ДРУГ ОТ ДРУГА НЕ ЗАВИСЯТ
--

data Handler p m  = forall msg . IsEncoded p msg =>
  Handler ( (From p, Cookie p) -> msg -> m () )

-- for convenience
handler :: forall msg m p . (IsEncoded p (msg p))
        => ((From p, Cookie p) -> msg p -> m ())
        -> Handler p m

handler = Handler

data Fabrique p = forall bus . Messaging bus p (MessageWithCookie p)
  =>  Fabrique bus

data Dispatcher p m =
  Dispatcher
  { self     :: Peer p
  , handlers :: Cache (Cookie p) (Handler p m)
  , fabriq   :: Fabrique p  -- СЮДОЙ ПИХАТЬ СООБЩЕНИЯ
  }

newDispatcher :: (MonadIO m, (Messaging bus p (MessageWithCookie p)))
              => Peer p
              -> bus
              -> m (Dispatcher p m)

newDispatcher me bus = do
  let fab = Fabrique bus
  cache <- liftIO $ Cache.newCache Nothing
  pure $ Dispatcher me cache fab

sendRequest :: forall p msg m. ( MonadIO m
                               , IsEncoded p msg
                               , Messaging (Fabrique p) p (MessageWithCookie p)
                               , Hashable (Cookie p)
                               , CookieGenerator p m
                               , HasTimeout msg m
                               )
            => Dispatcher p m
            -> Peer p
            -> Maybe (Cookie p)
            -> msg
            -> Handler p m
            -> m ()

sendRequest d p mbCoo msg answ = do
  cookie  <- maybe (genCookie p) pure mbCoo
  timeout <- timeoutFor (Proxy @msg) <&> Just . toTimeSpec
  liftIO $ Cache.insert' (handlers d) timeout cookie answ
  sendTo (fabriq d) (To p) (From (self d)) (MessageWithCookie cookie (encode msg))

dispatcher :: forall p m . ( MonadIO m, Hashable (Cookie p)
                           , Messaging (Fabrique p) p (MessageWithCookie p)
                           )
           => Dispatcher p m
           -> m ()

dispatcher d = fix \next -> do

  -- FIXME: if receive is non-blocking we'll get a busy loop
  -- FIXME: if receive is blocking - we'll block here forever
  received <- receive (fabriq d) (To (self d))

  for_ received $ \(who, MessageWithCookie coo bs) -> do

    -- поискали в мапе по куке
    found <- liftIO $ Cache.lookup (handlers d) coo

    case found of
      Nothing -> pure () -- NO HANDLER FOUND FOR COOKIE CASE
    -- декодировали сообщение и пихнули в обработчик
      Just (Handler dispatch) -> maybe (pure ()) (dispatch (who,coo)) (decode bs)
    --                                 ^^^^^^^^^ CAN NOT DECODE CASE

  next

data PingPong p = Ping
                | Pong


newtype instance Cookie Fake = CookieFake Word32
                               deriving stock (Eq)
                               deriving newtype (Hashable,Num,Pretty)


instance CookieGenerator Fake IO where
  genCookie _ = pure 0

instance IsEncoded Fake (PingPong p) where
  data instance Encoded Fake = PingPong p
  encode = undefined
  decode = undefined

instance Messaging (Fabrique Fake) Fake (MessageWithCookie Fake) where
  sendTo = undefined
  receive = undefined

instance HasTimeout (PingPong Fake) IO where
  timeoutFor _ = pure 0.1


testAbstractDispatch :: IO ()
testAbstractDispatch = do

  let peers = [1..2] :: [Peer Fake]

  bus  <- newFakeP2P @Fake @(MessageWithCookie Fake) True

  for_ peers $ \p -> do
    disp <- newDispatcher p bus
    dispThread <- async (dispatcher disp)

    for_ [ px | px <- peers, px /= p ] $ \pip -> do

      sendRequest disp pip Nothing (Ping @Fake) $ handler $ \(From who, coo) ->
        \case
          Pong -> liftIO $ print $ "got pong" <+> brackets (pretty coo)
          Ping -> do
            liftIO $ print $ "got ping" <+> pretty who <+> brackets (pretty coo)
            sendRequest disp who (Just coo) (Pong @Fake) $ handler @PingPong (\_  _ -> pure () )

    cancel dispThread

