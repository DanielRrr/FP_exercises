module Counter
    ( Counter
    , tick
    , runCounter
    ) where

-- Монада Counter считает количество тиков, т.е. вызовов функции tick
data Counter a = Counter Int a

-- Возвращает результат вычислений и количество тиков
runCounter :: Counter a -> (a, Int)
runCounter = undefined

instance Monad Counter where
    return = undefined
    (>>=) = undefined

tick :: Counter ()
tick = undefined
