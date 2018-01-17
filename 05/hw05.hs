import Test.HUnit
import Data.List
import Data.Char

fun :: [Integer] -> [Integer]
fun = \xs -> [ if odd x && even y then 2 * x else x | (x,y) <- zip [0..] xs]

fibs :: [Integer]
fibs = fmap fib [1..] where
  fib n = helper 0 1 n
  helper a b n | n == 0 = a
  helper a b n | n > 0 = helper b (a + b) (n - 1)
  helper a b n | n < 0 = helper b (a - b) (n + 1)

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = null [x | x <- [2..n-1], n `mod` x == 0]

primes :: [Integer]
primes = filter isPrime [2..]

-- 4. swap i j меняет местами i и j элементы.
--    Эта функция должна работать для бесконечных списков.
--    Напишите эффективную реализацию, что, в частности, означает, что нельзя использовать (!!).
-- (1.5 балла)
swap :: Int -> Int -> [a] -> [a]
swap n m xs = undefined

takeLast :: Int -> [a] -> [a]
takeLast n l = reverse (take n $ reverse l)

mapl :: (a -> Bool) -> ([a] -> b) -> [a] -> [b]
mapl prop f l = case span prop l of
  ([],[]) -> []
  ([],y:ys) -> mapl prop f ys
  (xs,ys) -> f xs : mapl prop f ys

unlines' :: [String] -> String
unlines' = intercalate "\n"

unwords' :: [String] -> String
unwords' = intercalate " "

lines' :: String -> [String]
lines' = mapl (/= '\n') id

words' :: String -> [String]
words' = mapl (not . isSpace) id

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
