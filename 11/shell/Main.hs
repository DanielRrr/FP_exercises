module Main where

-- Для работы со строкой ввода удобно испольовать библиотеку readline.
-- Ее может понадобится установить: cabal install readline.
import System.Console.Readline
import qualified System.Directory as D
import Control.Monad.Trans
import System.Environment
import System.Process
import System.Exit
import System.IO

import Shell

-- Функция command cmd args выполняет комманду cmd с аргументами args.
-- Сейчас реализовано несколько встроенных комманд: cd, pwd.
-- Если это не встроенная команда, то пытаемся запустить внешнее приложение.
-- Вам нужно изменить этот код, реализуя задания из файла shell_task.txt.
command :: String -> [String] -> Shell ()
command "pwd" _ = getCurrentDirectory >>= liftIO . putStrLn
command "cd" [] = liftIO D.getHomeDirectory >>= setCurrentDirectory
command "cd" (d:_) = setCurrentDirectory d
command cmd args = liftIO $ do
    exitCode <- rawSystem cmd args
    case exitCode of
        ExitFailure n -> do
            prog <- getProgName
            hPutStrLn stderr $ prog ++ ": " ++ cmd ++ ": " ++ msg n
        _ -> return ()
  where
    msg 127 = "command not found"
    msg n = "failed (exit " ++ show n ++ ")"

parseAndExecute :: String -> Shell ()
parseAndExecute str = case words str of
    [] -> return ()
    cmd:args -> command cmd args

prompt = "$ "

repl :: Shell ()
repl = do
    d <- getCurrentDirectory
    input <- liftIO $ readline (d ++ prompt)
    case input of
        Nothing -> liftIO $ putStrLn ""
        Just str -> parseAndExecute str >> repl

main :: IO ()
main = runShell repl
