import Test.HUnit
-- Нужно поставить библиотеку hunit:
-- cabal install hunit

-- 1. fun четные числа в нечетных позициях (нумеруя с 0) умножает на 2, остальные не изменяет.
-- (0.5 балла)
fun :: [Integer] -> [Integer]
fun = undefined

-- 2. fibs возвращает бесконечный список чисел Фибоначчи.
-- (0.5 балла)
fibs :: [Integer]
fibs = fmap fib [0..] where 
  fib n = helper 0 1 n
  helper a b n | n == 0 = a
  helper a b n | n > 0 = helper b (a + b) (n - 1)
  helper a b n | n < 0 = helper b (a - b) (n + 1)

-- 3a. isPrime проверяет простоту числа.
-- (0.5 балла)
isPrime :: Integer -> Bool
isPrime = undefined

-- 3b. primes возвращает бесконечный список простых чисел.
-- (0.5 балла)
primes :: [Integer]
primes = undefined

-- 4. swap i j меняет местами i и j элементы.
--    Эта функция должна работать для бесконечных списков.
--    Напишите эффективную реализацию, что, в частности, означает, что нельзя использовать (!!).
-- (1.5 балла)
swap :: Int -> Int -> [a] -> [a]
swap = undefined

-- 5a. takeLast n xs возвращает последние n элементов списка xs.
-- (0.5 балла)
takeLast :: Int -> [a] -> [a]
takeLast = undefined

-- 5b. Решите задачу 5a так, чтобы она работала за O(1) по памяти, если считать первый аргумент фиксированным.
--     Проверить количество потребляемой памяти можно следующим образом:
--     1. Создаем файл, где пишем функцию takeLast и следующую функцию main:
--        main = length (takeLast 20 [1..10000000]) `seq` return ()
--     2. Компилируем ghc -O2 TakeLast.hs
--     3. Запускаем ./TakeLast +RTS -s.
--     4. Смотрим на графу maximum residency.
--     5. Меняем число 10000000, смотрим как меняется потребление памяти.
-- (2.5 балла)

-- 6. Назовем элементы, которые удовлетворяют предикату p хорошими, остальные плохими.
-- Тогда mapl p f xs выбрасывает плохие элементы, а блоки подряд идущих хороших элементов,
-- которые разделяются плохими, отправляет в функцию f и возвращает список результатов.
-- Заметьте, что в функцию f никогда не передаются пустые списки.
-- (1 балл)
mapl :: (a -> Bool) -> ([a] -> b) -> [a] -> [b]
mapl = undefined

-- 7. Напишите функции unlines и unwords, используя функцию intercalate.
-- (1 балл)
unlines' :: [String] -> String
unlines' = undefined

unwords' :: [String] -> String
unwords' = undefined

-- 8. Напишите аналоги функций lines и words, используя функцию mapl.
--    Функция words' xs возвращает список слов в строке xs, которые были разделены (одним и более) пробельными символами.
--    Функция lines' работает аналогично, но разделяются слова по символам перевода строки.
-- (1 балл)
lines' :: String -> [String]
lines' = undefined

words' :: String -> [String]
words' = undefined

main = fmap (\_ -> ()) $ runTestTT $ test
    $    label "fun"
    [ fun [1,3,6,10,15,21,28,30,60] ~?= [1,3,6,20,15,21,28,60,60]
    , take 11 (fun fibs) ~?= [1,1,2,3,5,16,13,21,34,55,89]
    ] ++ label "fibs"
    [ take 10 fibs ~?= [1,1,2,3,5,8,13,21,34,55]
    , fibs !! 1000 ~?= 70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501
    ] ++ label "primes"
    [ take 10 primes ~?= [2,3,5,7,11,13,17,19,23,29]
    , primes !! 1000 ~?= 7927
    ] ++ label "swap"
    [ swap 1 2 [3,4,5,6] ~?= [3,5,4,6]
    , swap 2 0 "abcd" ~?= "cbad"
    , swap 100 7 [1..10] ~?= [1..10]
    ] ++ label "takeLast"
    [ takeLast 5 [1..20] ~?= [16,17,18,19,20]
    , takeLast 5 [1,2,3] ~?= [1,2,3]
    ] ++ label "mapl"
    [ mapl (\x -> x `mod` 7 /= 3) id [1..20] ~?= [[1,2],[4,5,6,7,8,9],[11,12,13,14,15,16],[18,19,20]]
    , mapl (not . isPrime) sum [1..20] ~?= [1,4,6,27,12,45,18,20]
    ] ++ label "unlines'"
    [ unlines' ["abc","def","ghi"] ~?= "abc\ndef\nghi"
    , unlines' ["foo","","","bar"] ~?= "foo\n\n\nbar"
    ] ++ label "unwords'"
    [ unwords' [] ~?= ""
    , unwords' ["a","b","cde"] ~?= "a b cde"
    ] ++ label "lines'"
    [ lines' "" ~?= []
    , lines' "abc" ~?= ["abc"]
    , lines' "abc  def \n\n foo bar  \n" ~?= ["abc  def ", " foo bar  "]
    , lines' "abc  def \n\n foo bar  \n " ~?= ["abc  def ", " foo bar  ", " "]
    ] ++ label "words'"
    [ words' "" ~?= []
    , words' "abc" ~?= ["abc"]
    , words' "abc  def \n\n foo bar  \n" ~?= ["abc", "def", "foo", "bar"]
    ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
