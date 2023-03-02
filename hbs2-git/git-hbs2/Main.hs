module Main where

import HBS2.Prelude
import HBS2.System.Logger.Simple hiding (info)

import HBS2Git.App
import HBS2Git.Export
import HBS2Git.ListRefs

import Options.Applicative as O
import Control.Monad

-- TODO: hbs2-git-state-init
--   Создать / обновить стейт в XDG_DATA_HOME/.hbs2-git, включая БД
--   Обновление стейта есть лог операций

-- TODO: hbs2-git-state-log
--   Определить операции над стейтом


-- TODO: git-repo-state-definition
--   Состоянием репозитория является значение всех ссылок + транзитивное
--   замыкание всех git-объектов, на которые ссылаются ссылки.
--   Таким образом, состояние репозитория == транзитивное замыкание
--   всех git-объектов, включая repo-head, где repo-head это все
--   локальные ссылки.
--
--   Операции:
--    1.   Получить state
--    1.1  Взять те ссылки из .git/refs/heads которые указаны в настройках
--    1.2  Отсортировать по имени
--    1.3  Получить значения Ei ( значение == git-commit )
--    1.4  Получить транзитивное замыкание T для каждого Ei
--    1.5  Построить дерево M для всех элементов из T
--    1.6  Сохранить дерево M
--    1.7  Дерево M определяет текущее состояние репозитория
--    1.8  Обновить значение ссылки R: Rn = M, где Rn - новое
--         значение ссылки
--    1.9  Уведомить об изменении значения ссылки R.
--
--   Вопросы:
--    2.   Где хранить ссылки из (1.1)
--    2.1  .git/config (?)
--    2.2  .hbs2-git/config (?)
--    2.3  $XDG_SHARE/.hbs2-git/repo/config
--
--    2.1 (+1). Лучше UX, не плодит новых файлов настроек
--    2.1 (-1). Убогий формат .git/config, придется прилагать специальные усилия,
--              что бы им управлять (задваивания и т.п.)
--    2.1 (-1)  Error-prone, можно ломать настройки git и приложений, которые хранят
--              своё барахло в .git/config
--
--    2.2  (+1) Более удобный формат конфига, проще парсить и поддерживать
--    2.2  (-1) Лишний конфиг в репозитории может раздражать людей
--
--    2.3  (+1) Лучше UX, нет лишнего мусора в репозитории
--    2.3  (-1) Хуже UX, непонятно, что происходит если настройки
--              менялись без ведома пользователя или он не понимал,
--              что они меняются
--    2.3 (+0.25) Можно писать для каждой операции, какой конфиг меняется
--    2.3 (+0.25) Можно все операции конфигурирования делать командами, записывая их в лог
--    2.3 (-1)  Непонятно, как поддерживать консистентность .hbs2-git/repo/ и repo
--    2.3 (+1)  По абсолютному пути на диске
--
--    Сценарий git-clone:
--
--    3.1 Пользователь делает git clone hbs2://ref
--    3.2 git вызывает git-remote-hbs2
--    3.3 git делает туда fetch
--    3.4 git-remote-hbs2 должен:
--    3.4.1 Получить все ссылки
--    3.4.1 Для каждой ссылки получить транзитивное замыкание T
--    3.4.2 Для каждого объекта из T - сделать git hash-object --literally
--
--    * git-remote-hbs2 узнаёт URI репозитория (ссылку) из аргументов git-remote-hbs2
--    * git-remote-hbs2 достёт merkle-дерево по ссылкие (текущее значение ссылки, R)
--    * git-remote-hbs2 для каждого git-объекта делает git hash-object --literally
--    * git-remote-hbs2 возвращает все значения ссылок (repo-head) из merkle-дерева M (R = M)
--
--    * У каждого M только одно значение repo-head

--    Сценарий git-fetch:
--    Полностью Аналогично git clone

--    Сценарий import
--    Пока у нас нет ссылок, нам надо проверять работоспособность сценария "git clone".
--    Для этого можно построить транзитивное замыкание Tr для репозитория и замокать
--    значение ссылки, либо - отдельной командой из полученного merkle дерева импортировать
--    всё в репозиторий (что делать со ссылками?)

-- TODO: hbs2-git-cli-skeleton
--  скелет обработки аргументов командной строки

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "hbsync block fetch"
  <> progDesc "fetches blocks from hbsync peers"
  )
  where
    parser ::  Parser (IO ())
    parser = hsubparser (  command "export"    (info pExport (progDesc "export repo"))
                        <> command "dump"      (info pDump (progDesc "dump repo state tree"))
                        <> command "list-refs" (info pListRefs (progDesc "list refs"))
                        )


    pExport = do
      ref <- strArgument (metavar "HASH-REF")
      pure $ runApp WithLog (runExport ref)

    pListRefs = do
      pure $ runApp NoLog runListRefs

    pDump = do
      ref <- strArgument (metavar "HASH-REF")
      refv <- optional $ strArgument (metavar "HASH-REF-VAL")
      pure $ runApp WithLog (runDumpStateTree ref refv)

