module Main where

import Fixme
-- import Fixme.Run
import Fixme.Run
import System.Environment

-- TODO: fixme-new
--   $author: Dmitry Zuikov <dzuikov@gmail.com>
--   $milestone: undefined
--   $priority: ASAP
--  после майских:
--  1.  fixme переезжает в дерево hbs2, конкретно в hbs2-git

--  2.  fixme преобразуется в утилиту для генерации отчётов по репозиторию git
--
--  3.  fixme генерирует поток фактов про репозиторий git, включая записи todo/fixme
--
--  4.  fixme начинает генерировать PR-ы в формате git (у гита есть простенькие пулл-реквесты!)
--      и умеет постить их куда там их следует постить
--
--  5.  fixme получает ограничитель глубины сканирования и фильтр бранчей,
--      что бы не окочуриваться на больших проектах
--
--  6.  fixme генерирует настройки по умолчанию, включая .gitignore
--
--  7.  fixme позволяет явно задавать лог изменений статуса, беря его как из
--      .fixme/log так и откуда скажут
--
--  8.  fixme интегрируется в hbs2-git-dashboard
--
--  9.  fixme временно получает название fixme2 или nfixme или hfixme (не решил пока),
--      потом возвращается к старому названию
--
--  10. fixme умеет постить записи в своём формате в hbs2 или же умеет любые источники дампить в своём формате так,
--        что бы hbs2-git мог запостить их в соответствующий рефчан
--
--  11. fixme оформляет либу для экстракции фактов из git, которую будет использовать и hbs2-git-dashboard
--
--  12. hbs2-git-dashboard понимает и уважает каталог настроек .fixme , а стейт берёт прямо оттуда

--  открытые вопросы:

--  hbs2-git использует fixme или fixme использует hbs2

--  переводить fixme на fuzzy-parse или нет (скорее, да)

--  переводить ли suckless-conf на fuzzy-parse сейчас (или хрен пока с ним)

--  встроить ли jq внутрь или лучше дать доступ к sql запросам по json

main :: IO ()
main = do

  -- TODO: discover-config
  --
  -- TODO: local-config-has-same-name-with-binary
  --
  -- TODO: per-user-config-has-same-name-with-binary
  --
  -- TODO: per-user-config-added-after-per-project-config

  -- TODO: scan-all-sources
  --   for-source-from-con

  runFixmeCLI runCLI

-- FIXME: test-fixme
--   $workflow: wip
--   $assigned: voidlizard
--
--   Тестовый тикет с параметрами

