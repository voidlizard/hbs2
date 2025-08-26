{-# LANGUAGE ScopedTypeVariables #-}
module Tee (withTeeLogging) where

import Control.Concurrent (forkFinally)
import Control.Monad (void, when)
import qualified Data.ByteString as BS
import Data.Word (Word8)
-- import System.IO
import GHC.IO.Handle (hDuplicateTo,hDuplicate)
import System.Posix.IO
import System.Posix.Types (Fd)
import UnliftIO
import UnliftIO.IO.File

withTeeLogging :: forall a m . MonadUnliftIO m => FilePath -> m a -> m a
withTeeLogging logPath action = withBinaryFile logPath AppendMode \hLog -> do
    hSetBuffering hLog NoBuffering
    -- Сохраняем реальные stdout/stderr
    hOut <- liftIO $ hDuplicate stdout
    hErr <- liftIO $ hDuplicate stderr
    liftIO do
      hSetBuffering hOut NoBuffering
      hSetBuffering hErr NoBuffering

    bracket makePipe (\(r,w) -> mapM_ hClose [r,w]) $ \(hR, hW) -> do
      bracket_
        (liftIO ( do hDuplicateTo hW stdout
                     hDuplicateTo hW stderr
                     hClose hW ))
        (liftIO ( do
            hDuplicateTo hOut stdout
            hDuplicateTo hErr stderr
            hFlush hOut
            hFlush hErr))
        $ do
          -- Поток-перехватчик
          _ <- liftIO $ forkFinally (pump hR hOut hErr hLog) (\_ -> hClose hR)
          action
  where
    makePipe = liftIO do
      (rfd :: Fd, wfd :: Fd) <- createPipe
      hR <- fdToHandle rfd
      hW <- fdToHandle wfd
      hSetBuffering hR NoBuffering
      hSetBuffering hW NoBuffering
      pure (hR, hW)

    pump hR hOut hErr hLog = loop
      where
        loop = do
          bs <- BS.hGetLine hR
          if BS.null bs
            then pure ()
            else do
              BS.hPut hOut bs >> hFlush hOut
              BS.hPut hLog (stripANSI bs) >> hFlush hLog
              loop

-- ---- ANSI stripper ----
-- Удаляем распространённые последовательности:
--   CSI:  ESC '[' ... <final @..~>
--   OSC:  ESC ']' ... BEL (0x07) или ESC '\'
--   Single ESC: ESC <printable>
stripANSI :: BS.ByteString -> BS.ByteString
stripANSI = go
  where
    esc  = 0x1b :: Word8
    bel  = 0x07 :: Word8
    bksl = fromIntegral (fromEnum '\\') :: Word8
    lbr  = fromIntegral (fromEnum '[') :: Word8
    rbr  = fromIntegral (fromEnum ']') :: Word8

    go bs =
      case BS.uncons bs of
        Nothing -> BS.empty
        Just (c, rest)
          | c /= esc  -> BS.cons c (go rest)
          | otherwise -> dropEsc rest

    -- после ESC
    dropEsc s =
      case BS.uncons s of
        Nothing       -> BS.empty
        Just (c1, r1)
          | c1 == lbr -> go (BS.drop 1 $ dropCSI r1)    -- ESC [
          | c1 == rbr -> go (dropOSC r1)    -- ESC ]
          | otherwise -> go r1              -- Прочие короткие ESC-послед.

    -- CSI: ESC '[' ... <final in 0x40..0x7E>
    dropCSI = BS.dropWhile (not . isFinal)
      where
        isFinal w = w >= 0x40 && w <= 0x7e
        -- также съедаем финальный байт, если он есть
        -- делаем это в вызывающем месте: go (dropCSI r1)

    -- OSC: ESC ']' ... (BEL | ESC '\')
    dropOSC = goOSC
      where
        goOSC s =
          case BS.uncons s of
            Nothing -> BS.empty
            Just (w, r)
              | w == bel                 -> r
              | w == esc, startsWithBS bksl r -> BS.drop 1 r  -- пропустить '\'
              | otherwise                -> goOSC r

        startsWithBS x s = case BS.uncons s of
                             Just (y, _) -> y == x
                             _           -> False
