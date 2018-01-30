module Hw06 where

import qualified Data.Map as M
import Prelude hiding (lookup)
import Test.HUnit

data Complex = Complex { real :: Double, im :: Double } deriving (Show, Eq)

fromDouble :: Double -> Complex
fromDouble d = Complex d 0

i :: Complex
i = Complex 0 1

infixl 6 +., -.
(+.) :: Complex -> Complex -> Complex
(+.) (Complex x1 y1) (Complex x2 y2) = Complex (x1 + x2) (y1 + y2)

(-.) :: Complex -> Complex -> Complex
(-.) (Complex x1 y1) (Complex x2 y2) = Complex (x1 - x2) (y1 - y2)

infixl 7 *., /.
(*.) :: Complex -> Complex -> Complex
(*.) (Complex x1 y1) (Complex x2 y2) = Complex (x1 * x2 - y1 * y2) (x1 * y2 + x2 * y1)

(/.) :: Complex -> Complex -> Complex
(/.) (Complex x1 y1) (Complex x2 y2) =
      Complex ((x1 * x2 + y1 * y1)/(x2 * x2 + y2 * y2)) ((x2 * y1 - x1 * y2)/(x2 * x2 + y2 * y2))

conj :: Complex -> Complex
conj (Complex a b) = Complex a (-b)

testsComplex =
    [ i *. i ~?= fromDouble (-1)
    , fromDouble 3 +. i ~?= Complex 3 1
    , fromDouble 3 *. i ~?= Complex 0 3
    , (fromDouble 3 +. fromDouble 4 *. i) *. (fromDouble 4 +. fromDouble 3 *. i) ~?= fromDouble 25 *. i
    , conj (fromDouble 3 +. fromDouble 4 *. i) ~?= fromDouble 3 -. fromDouble 4 *. i
    , fromDouble 2 /. (fromDouble 1 +. i) ~?= fromDouble 1 -. i
    ]

data Tree a = Node { value :: a, children :: [Tree a] }

height :: Tree a -> Int
height (Node x l) = maximum (fmap height l) + 1

avg :: Tree Int -> Int
avg = undefined

-- (c) Возвращает ширину дерева
-- Ширина дерева определяется следующим образом:
-- Количество вершин на определенном уровне называется шириной уровня.
-- Ширина дерева - это максимальная ширина уровня по всем уровням.
width :: Tree a -> Int
width = undefined

-- tests

(tree1, tree2, tree3) =
    ( b [b [l [b []],
            l [b [],
               l [b [l [],
                     l [],
                     b []],
                  l []]]],
         b [],
         b [],
         l []]
    , b [b [b [],
            b [b [],
               b []]],
         b [b [],
            l [b [],
               b []]],
         l [b []]]
    , b [tree1, tree2]
    )
  where l = Node 500; b = Node 300

(testsHeight, testsAvg, testsWidth) = (
    [ height tree1 ~?= 6
    , height tree2 ~?= 4
    , height tree3 ~?= 7
    ],
    [ avg tree1 ~?= 393
    , avg tree2 ~?= 330
    , avg tree3 ~?= 362
    ],
    [ width tree1 ~?= 4
    , width tree2 ~?= 5
    , width tree3 ~?= 7
    ])

------------------------------------------------------------------------------
-- 3

data BinTree a = BinLeaf | BinNode (BinTree a) a (BinTree a)

-- Определите тип путей в дереве.
-- Каждый элемент этого типа должен указывать максимум на один узел дерева.
data BinPath = Undefined deriving (Show,Eq)

-- (a) Напишите функцию, возвращающую элемент дерева по указонному пути, если он присутствует в этом дереве.
getElem :: BinTree a -> BinPath -> Maybe a
getElem = undefined

-- (b) Напишите функцию, принимающую некоторый элемент и возвращающую путь в дереве к любому узлу, содержащему элемент, равный данному, если такой узел существует.
findElem :: Eq a => BinTree a -> a -> Maybe BinPath
findElem = undefined

-- tests

testsPath =
    [ (findElem tree1 22 >>= getElem tree1) ~?= Just 22
    , (findElem tree1 22 >>= getElem tree2) ~?= Nothing
    , (findElem tree1 22 >>= getElem tree3) ~?= Just 'c'
    , findElem tree2 22 ~?= Nothing
    ]
  where
    tree1 =
        b (b (b l 3 l) 4
             (b l 6 l)) 8
          (b (b l 9 l) 5
             (b (b l 22 l) 11 l))
    tree2 = b l 10 (b (b l 15 l) 11 l)
    tree3 =
        b l 'a'
          (b l 'b'
             (b (b l 'c' l) 'd' l))
    b = BinNode
    l = BinLeaf

------------------------------------------------------------------------------
-- 4

data Value = I Int | B Bool deriving (Eq, Show)
data BinOp = Plus | Mul | Minus | Less | Greater | Equals
data UnOp = Neg | Not
data Expr = BinOp BinOp Expr Expr | UnOp UnOp Expr | Const Value | If Expr Expr Expr | Var String
data Statement = Assign String Expr | While Expr Statement | Compound [Statement]

infixr 0 @=
(@=) = Assign
(.+) = BinOp Plus
(.-) = BinOp Minus
(.*) = BinOp Mul
(.<) = BinOp Less
int = Const . I
bool = Const . B
neg = UnOp Neg

type Error = String

-- evalExpr m e интерпретирует выражение e, в m передается значение переменных.
-- evalExpr возвращает либо успешно вычисленный результат, либо список ошибок.
-- Ошибки бывают двух видов: необъявленная переменная и несоответствие типов.
-- Возвращается список ошибок, т.к. выражение может содержать больше одной ошибки.
evalExpr :: M.Map String Value -> Expr -> Either [Error] Value
evalExpr = undefined

-- evalStatement принимает текущее значение переменных и statement и возвращает новое значение переменных после его выполнения.
evalStatement :: M.Map String Value -> Statement -> Either [Error] (M.Map String Value)
evalStatement = undefined

-- tests

max' x y = If (x .< y) y x
expr1 = Var "x" .+ int 3
expr2 = If (Var "x") (Var "y" .- int 3) (int 2)
stat1 = Compound
    [ "x" @= int 3 .+ int 4
    , "y" @= Var "x" .* int 6
    , "z" @= neg $ max' (Var "x") (Var "y")
    ]
stat2 = Compound
    [ "r" @= int 1
    , "i" @= int 0
    , While (Var "i" .< Var "n") $ Compound
        [ "i" @= Var "i" .+ int 1
        , "r" @= Var "r" .* Var "i"
        ]
    ]

testsExpr = [ errorsCount (evalExpr M.empty expr1) ~?= 1
            , evalExpr (M.fromList [("x", B True), ("y", I 5)]) expr2 ~?= Right (I 2)
            , evalExpr (M.fromList [("x", B False), ("y", B False)]) expr2 ~?= Right (I 2)
            , errorsCount (evalExpr (M.fromList [("x", B True), ("y", B False)]) expr2) ~?= 1
            , fmap (M.lookup "z") (evalStatement M.empty stat1) ~?= Right (Just $ I $ -42)
            , fmap (M.lookup "r") (evalStatement (M.fromList [("n", I 6)]) stat2) ~?= Right (Just $ I 720)
            ]
  where errorsCount = either length (const 0)

------------------------------------------------------------------------------
-- 5. Реализовать двоичное дерево поиска без балансировки.

data Map k v = Leaf | Branch k v (Map k v) (Map k v)

empty :: Map k v
empty = Leaf


lookup :: Ord k => k -> Map k v -> Maybe v
lookup key Leaf = Nothing
lookup key (Branch x y left right) = case compare key x of
                                      LT -> lookup key left
                                      EQ -> Just y
                                      GT -> lookup key right

insert :: Ord k => k -> v -> Map k v -> (Map k v, Maybe v)
insert key value m = (insert' key value m, lookup key m)
      where
        insert' key value Leaf = Branch key value Leaf Leaf
        insert' key value (Branch x y left right) = case compare key x of
                                                      EQ -> Branch key value left right
                                                      LT -> Branch key value (insert' key value left) right
                                                      GT -> Branch key value left (insert' key value right)

delete :: Ord k => k -> Map k v -> Maybe (Map k v)
delete = undefined

fromList :: Ord k => [(k, v)] -> Map k v
fromList [] = Leaf
fromList (x:xs) = undefined

toList :: Map k v -> [(k, v)]
toList Leaf = []
toList (Branch key value left right) = toList left ++ [(key, value)] ++ toList right

-- tests

sort :: Ord a => [a] -> [a]
sort = map fst . toList . fromList . map (\x -> (x, ()))

------------------------------------------------------------------------------
-- main

main = fmap (\_ -> ()) $ runTestTT $ test
      $  label "complex" testsComplex
      ++ label "height" testsHeight
      ++ label "avg" testsAvg
      ++ label "width" testsWidth
      ++ label "path" testsPath
      ++ label "Expr" testsExpr
      ++ label "Map" -- можете сами написать тесты на каждую функцию :)
            [ sort [10,24,13,56,35,13,6,23] ~?= [6,10,13,23,24,35,56] ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
