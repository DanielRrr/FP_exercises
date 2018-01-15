import Data.IORef

-- Реализуйте различные вариации на тему while

while :: Monad m => m Bool -> m a -> m [a]
while = undefined

while_ :: Monad m => m Bool -> m a -> m ()
while_ = undefined

doWhile :: Monad m => (a -> Bool) -> m a -> m [a]
doWhile = undefined

doWhile_ :: Monad m => (a -> Bool) -> m a -> m ()
doWhile_ = undefined

-- Реализуйте while' через while_
while' :: Monad m => m Bool -> m ()
while' = undefined

----------------------------------------------------------

-- Пример использования while.
fac :: Int -> IO [Int]
fac n = do
    i <- newIORef 0
    r <- newIORef 1
    while (readIORef i >>= \i' -> return $ i' < n) $ do
        r' <- readIORef r
        i' <- readIORef i
        writeIORef i (i' + 1)
        writeIORef r ((i' + 1) * r')
        return r'

