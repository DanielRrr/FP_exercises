{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StateIO
    ( StateIO
    , runStateIO, execStateIO, evalStateIO
    ) where

import Control.Monad.State
import Data.IORef

-- Реализуйте монаду State, используя IORef для хранения состояния.
newtype StateIO s a = StateIO { getStateIO :: IORef s -> IO a }

instance Monad (StateIO s) where
    return = undefined
    (>>=) = undefined

instance MonadState s (StateIO s) where
    get = undefined
    put = undefined

runStateIO :: StateIO s a -> s -> IO (a,s)
runStateIO = undefined

execStateIO :: StateIO s a -> s -> IO s
execStateIO = undefined

evalStateIO :: StateIO s a -> s -> IO a
evalStateIO = undefined
