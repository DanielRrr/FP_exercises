fib :: Integer -> Integer
fib n = helper 0 1 n
helper a b n | n == 0 = a
helper a b n | n > 0 = helper b (a + b) (n - 1)
helper a b n | n < 0 = helper b (a - b) (n + 1)

doubleFac :: Integer -> Integer
doubleFac 0 = 1
doubleFac 1 = 1
doubleFac n = n * doubleFac (n - 2)

numberOfDigits :: Int -> Int
numberOfDigits n = length (show n)

sumOfDigits :: Integer -> Integer
sumOfDigits 0 = 0
sumOfDigits n = sumOfDigits (n `div` 10) + (n `mod` 10)

gcd' :: Integer -> Integer -> Integer
gcd' 0 m = m
gcd' n 0 = n
gcd' n m = if n < m then gcd' n (m `mod` n) else gcd' m (n `mod` m)

minp :: (Integer -> Bool) -> Integer
minp p = nu p where
  nu p = head [ x | x <- [0..], p x]

integral :: (Double -> Double) -> Double -> Double -> Double
integral f a b = onestep ((f a + f b)/2) (a + h) 1
	where
		n = 100000
		h = (b - a) / n
		onestep acc x i
			| i == n   = h*acc
			| otherwise  = onestep (acc+(f x)) (x + h) (i + 1)

fix :: (a -> a) -> a
fix g = let x = g x in x

facFix :: Integer -> Integer
facFix = fix (\ _ n -> if n > 0 then n * facFix n else 1)

cZero :: (a -> a) -> a -> a
cZero f x = x

cOne :: (a -> a) -> a -> a
cOne f x = f x

cSuc :: ((a -> a) -> a -> a) -> (a -> a) -> a -> a
cSuc n f x = f (n f x)

cAdd :: ((a -> a) -> a -> a) -> ((a -> a) -> a -> a) -> (a -> a) -> a -> a
cAdd f g h a = f h (g h a)

cMul :: ((a -> a) -> a -> a) -> ((a -> a) -> a -> a) -> (a -> a) -> a -> a
cMul f g x a = f (g x) a
