module Main where

import HBS2.Prelude.Plated

import HBS2.System.Logger.Simple.ANSI as Exported
import HBS2.System.Dir
import HBS2.Misc.PrettyStuff as Exported

import Data.Config.Suckless.Script
import Data.Config.Suckless.Script.File

import Data.List (sort,foldl')
import Control.Monad.Trans.Cont
import System.Environment qualified as E
import System.Exit (exitSuccess)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Codec.Serialise
import Lens.Micro.Platform
import Streaming.Prelude qualified as S

-- debugPrefix :: LoggerEntry -> LoggerEntry
debugPrefix = toStderr . logPrefix "[debug] "

setupLogger :: MonadIO m => m ()
setupLogger = do
  setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStdout . logPrefix ""
  pure ()

flushLoggers :: MonadIO m => m ()
flushLoggers = do
  silence

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE


type TestScripPerks m = ( MonadIO m, MonadUnliftIO m)

quit :: MonadIO m => m ()
quit = liftIO exitSuccess

data E a =
  N (Map FilePath (E a)) | L a
  deriving stock (Eq,Ord,Generic)

instance Serialise a => Serialise (E a)

-- newtype Directory = Directory (HashMap FilePath E)
--                     deriving stock (Generic)
--                     deriving newtype (Eq,Show,Semigroup,Monoid)

-- instance Serialise Directory

-- Вставка одного пути в структуру
insert :: [FilePath] -> a -> E a -> E a
insert [] v (N children) = L v  -- Если дошли до конца пути, превращаем узел в лист с значением
insert [] _ (L v) = L v         -- Если уже лист, оставляем его как есть
insert (p:ps) v (N children) =  -- Если остались элементы пути, продолжаем вставку
    N (Map.alter update p children)
  where
    update Nothing = Just (insert ps v (N Map.empty))
    update (Just child) = Just (insert ps v child)
insert _ _ (L _) = error "assertion"

buildTrie :: [([FilePath], a)] -> E a
buildTrie paths = foldl' (\trie (path, v) -> insert path v trie) (N Map.empty) paths

-- Преобразование структуры в список путей с значениями
toList :: E a -> [([FilePath], a)]
toList = go []
  where
    go prefix (L v) = [(prefix, v)]
    go prefix (N children) = concatMap (\(p, subtree) -> go (prefix ++ [p]) subtree) (Map.toList children)


listDirFiles :: MonadIO m => FilePath -> m [FilePath]
listDirFiles root = do
  S.toList_ <$> glob ["**/*"] [] root $ \fn -> do
    S.yield fn
    pure True

-- buildDirectory :: [FilePath] -> Directory
-- buildDirectory entries = do
  -- get entry
  --   split entry
  --     for each part:
  --
  -- mempty


theDict :: forall m . ( TestScripPerks m
                      ) => Dict C m
theDict = do
  makeDict @C do
    -- TODO: write-man-entries
    myHelpEntry
  where

    myHelpEntry = do
        entry $ bindMatch "--help" $ nil_ $ \case
          HelpEntryBound what -> do
            helpEntry what
            quit

          _ -> helpList False Nothing >> quit

        entry $ bindMatch "hello" $ nil_ $ const $ do
          liftIO $ putStrLn "hello"

        entry $ bindMatch "dir:list" $ nil_ $ \syn -> do
          let root = case syn of
                       [StringLike p] -> p
                       _ -> "."

          what <- listDirFiles root <&> sort

          liftIO $ mapM_ putStrLn what

        entry $ bindMatch "dir:list:build" $ nil_ $ \syn -> do
          let root = case syn of
                       [StringLike p] -> p
                       _ -> "."

          what <- listDirFiles root <&> fmap ((, ()) . splitPath)

          let trie = buildTrie what

          liftIO $ LBS.hPutStr stdout (serialise what) >> hFlush stdout

main :: IO ()
main = flip runContT pure do

  setupLogger

  ContT $ bracket none $ const do
    silence

  argz <- liftIO $ E.getArgs
  cli <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  let dict = theDict
  void $ lift $ run dict cli




