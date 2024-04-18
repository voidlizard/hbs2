{-# Language RecordWildCards #-}
{-# Language PatternSynonyms, ViewPatterns #-}
module Main where

import HBS2.Prelude
import HBS2.Base58
import HBS2.OrDie

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Codec.Serialise
import Lens.Micro.Platform
import Control.Monad.Trans.Cont

import Control.Monad
import UnliftIO

-- желаемое поведение: добавить в новую версию A какое-нибудь поле так,
-- что бы предыдущие записи продолжали десериализоваться без этого поля,
-- а новое поле было бы пустым, если его нет -- в новой версии.


data A0 =
     A0 { a0Int :: Int }
  deriving stock (Generic,Show)

instance Serialise A0

data A1 =
    A11 { a1Int :: Int }
  | A12 { a1Int :: Int, _a1Str :: Maybe String }
  deriving stock (Generic,Show)


instance Serialise A1

a1Str :: Lens A1 A1 (Maybe String) (Maybe String)
a1Str = lens g s
  where
    g (A11{})   = Nothing
    g (A12{..}) = _a1Str

    s x@(A11{}) _ = x
    s x@(A12{}) w = x { _a1Str = w }

-- меняем тип:
--   старая версия ломается точно, голова остаётся той версии, которая была
--   новая версия: должна понимать и старую, и новую голову.
--


data W = A | B0 | B1 Bool

isB a = case a of
          B0   -> True
          B1 _ -> True
          _    -> False

-- -- Определяем паттерн-синоним для B, который будет сопоставлять B0 и B1
pattern B :: Bool -> W
pattern B b <- (isB -> b)

{-# COMPLETE A, B #-}

-- -- Функция test использует паттерн B для сопоставления с образцом
test :: W -> String
test w = case w of
  B val -> "Match B with value " ++ show val
  A     -> "Match A"


runWithAsync :: IO ()
runWithAsync = do

  hSetBuffering stdout LineBuffering

  flip runContT pure do

    t1 <- ContT $ withAsync do
            forever do
              print "PIU"
              pause @'Seconds 1

    q <- ContT $ withAsync do
               pause @'Seconds 10
               print "FUCKIG QUIT"

    pysh <- ContT $ withAsync $ forever do
               pause @'Seconds 2
               print "PYSHPYSH"

    void $ waitAnyCatchCancel [t1,q,pysh]


testCont :: IO ()
testCont = do

  flip runContT pure do
    for_ [1..10] $ \i -> do
      callCC \next -> do

        when (even i) do
          next ()

        liftIO $ print i


main :: IO ()
main = do
  print "1"
  let a1 = serialise (A0 22) & deserialiseOrFail @A1
  let a2 = serialise (A11 22) & deserialiseOrFail @A0
  let a3 = serialise (A12 22 (Just "X1")) & deserialiseOrFail @A0
  let a4 = serialise (A12 22 (Just "X1")) & deserialiseOrFail @A1

  -- let a0bs = serialise (A0 22) :: LBS.ByteString
  -- let a1bs = serialise (A1 22)  -- & LBS.toStrict & toBase58
  -- let a1  = deserialise @A a1bs
  -- let a0  = deserialise @A0 a0bs

  print a1
  print $ a1 <&> view a1Str
  print $ a4 <&> view a1Str
  print a2
  print a3
  print $ a1 <&> set a1Str (Just "JOPAKITA")
  print $ a4 <&> set a1Str (Just "JOPAKITA")

  pure ()


