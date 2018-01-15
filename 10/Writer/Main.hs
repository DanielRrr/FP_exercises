import Control.Monad.Writer
import Test.HUnit

-- Монаду Writer можно использовать для сбора какой-либо дополнительной информации в процессе вычисления.
-- Например, в этом задании необходимо собрать строку, в которой будет записано выражения, которое было вычислено в процессе работы функции fac.
-- Конкретный формат строки описан в тестах.
-- 
-- Для решения используйте только функции из класса Monad и функцию tell.
fac :: Int -> Writer String Int
fac = undefined

main = fmap (const ()) $ runTestTT $ test
    [ runWriter (fac 0) ~?= (1,"1")
    , runWriter (fac 1) ~?= (1,"1")
    , runWriter (fac 2) ~?= (2,"(1) * 2")
    , runWriter (fac 3) ~?= (6,"((1) * 2) * 3")
    , runWriter (fac 4) ~?= (24,"(((1) * 2) * 3) * 4")
    , runWriter (fac 5) ~?= (120,"((((1) * 2) * 3) * 4) * 5")
    ]