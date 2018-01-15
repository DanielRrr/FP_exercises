{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shell
    ( Shell, runShell
    , getCurrentDirectory, setCurrentDirectory
    ) where

import Control.Monad.State
import Control.Applicative
import qualified System.Directory as D

-- Вам понадобится добавить другие поля в эту запись
data ShellState = ShellState
    { currentDirectory :: String
    -- В принципе, можно обойтись и без этого поля, просто использовать реальную текущую директорию процесса.
    -- Но для примера я буду использовать состояние для хранения текущей директории.
    }

newtype Shell a = Shell { unShell :: StateT ShellState IO a }
    deriving (Functor, Applicative, Monad, MonadState ShellState)

runShell :: Shell a -> IO a
runShell (Shell m) = do
    s <- D.getCurrentDirectory
    evalStateT m $ ShellState
        { currentDirectory = s
        }

getCurrentDirectory :: Shell String
getCurrentDirectory = Shell $ do
    p <- get
    return (currentDirectory p)

setCurrentDirectory :: String -> Shell ()
setCurrentDirectory s = Shell $ modify $ \p -> p { currentDirectory = s }

-- Исправьте это определение так, чтобы исключения, которые бросает m, ловились бы, и выводились в stderr.
instance MonadIO Shell where
    liftIO m = Shell $ liftIO m
