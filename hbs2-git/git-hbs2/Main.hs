module Main where

import qualified MyLib (someFunc)

-- TODO: hbs2-git-state-init
--   Создать / обновить стейт в XDG_DATA_HOME/.hbs2-git, включая БД
--   Обновление стейта есть лог операций

-- TODO: hbs2-git-state-log
--   Определить операции над стейтом


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
