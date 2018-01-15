-- 1. fib n вовзращает n-ое число Фибоначчи.
--    Функция должна работать за линейное вермя и определена для всех целых n.
--    Для отрицательных n значение определяется по формуле fib n = fib (n + 2) - fib (n + 1).
--    (1 балл)
fib :: Integer -> Integer
fib n = helper 0 1 n
helper a b n | n == 0 = a
helper a b n | n > 0 = helper b (a + b) (n - 1)
helper a b n | n < 0 = helper b (a - b) (n + 1)

-- 2. Определите функцию, вычисляющую двойной факториал, то есть произведение натуральных чисел, не превосходящих заданного числа и имеющих ту же четность.
--    (1 балл)
doubleFac :: Integer -> Integer
doubleFac 0 = 1
doubleFac 1 = 1
doubleFac n = n * doubleFac (n - 2)

-- 3a. Написать функцию, возвращающую количество цифр числа.
--     Для целочисленного деления можете использовать функции div и mod.
--    (0.5 балла)
numberOfDigits :: Int -> Int
numberOfDigits n = length (show n)

-- 3b. Написать функцию, возвращающую сумму цифр числа.
--    (0.5 балла)
sumOfDigits :: Integer -> Integer
sumOfDigits 0 = 0
sumOfDigits n = sumOfDigits (n `div` 10) + (n `mod` 10)

-- 4. gcd' возвращает НОД.
--    (1 балл)
gcd' :: Integer -> Integer -> Integer
gcd' 0 m = m
gcd' n 0 = n
gcd' n m = if n < m then gcd' n (m `mod` n) else gcd' m (n `mod` m)

-- 5. minp p возвращает минимальное по модулю число x такое, что p x == True. Если такого x не существует, minp не завершается.
--    (1 балл)
minp :: (Integer -> Bool) -> Integer
minp = undefined

-- 6. integral f a b возвращает значение определенного интеграла функции f на отрезке [a,b].
--    Для реализации можете использовать метод трапеций.
--    (2 балла)
integral :: (Double -> Double) -> Double -> Double -> Double
integral f a b = onestep ((f a + f b)/2) (a + h) 1
	where
		n = 100000
		h = (b - a) / n
		onestep acc x i
			| i == n   = h*acc
			| otherwise  = onestep (acc+(f x)) (x + h) (i + 1)

-- 7a. Так как Хаскелл -- типизированный язык, мы не можем реализовать комбинатор неподвижной точки так, как мы это делали в нетипизированном лямбда-исчислении.
--    Но его можно реализовать, используя общую рекурсию, которая есть в Хаскелле.
--    Напишите fix.
--    (0.5 балла)
fix :: (a -> a) -> a
fix g = let x = g x in x

-- 7b. Реализуйте факториал при помощи fix.
--    (0.5 балла)
facFix :: Integer -> Integer
facFix = fix (\ _ n -> if n > 0 then n * facFix n else 1)

-- 8. Реализуйте ряд операций над числами Чёрча.
--    Термы должн иметь указанные незакомментированные типы.
--    (2 балла)

-- 8a. Ноль
cZero :: (a -> a) -> a -> a
cZero = undefined

-- 8b. Операция увеличения на 1
cSuc :: ((a -> a) -> a -> a) -> (a -> a) -> a -> a
cSuc = undefined

-- 8c. Преобразование в Int
cEval :: ((Int -> Int) -> Int -> Int) -> Int
cEval = undefined

-- 8d. Сложение
cAdd :: ((a -> a) -> a -> a) -> ((a -> a) -> a -> a) -> (a -> a) -> a -> a
cAdd = undefined

-- 8e. Умножение
cMul :: ((a -> a) -> a -> a) -> ((a -> a) -> a -> a) -> (a -> a) -> a -> a
cMul = undefined

-- 8f. Возведение в степень
cExp = undefined
