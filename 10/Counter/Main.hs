import Counter

-- Эти две функции отличаются от обычных тем, что, вызывая tick, они считают сколько шагов заняло их выполнение.
filter' :: (a -> Bool) -> [a] -> Counter [a]
filter' = undefined

append :: [a] -> [a] -> Counter [a]
append = undefined

-- Реализуйте qsort через filter' и append
qsort :: Ord a => [a] -> Counter [a]
qsort = undefined

-- Первый вызов должен занимать большее количество тиков ~ в 2 раза
main = do
    print $ runCounter $ qsort [1..15]
    print $ runCounter $ qsort [8,4,12,2,6,10,14,1,3,5,7,9,11,13,15]
