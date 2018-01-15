import Test.HUnit
import qualified Data.Map as M
import Data.Monoid

import Complex
import Tree
import Xor
import Maybe

-- Complex

testCasesComplex =
    [ (Complex 3 7, "3 + 7 * i")
    , (Complex (-3) 7, "-3 + 7 * i")
    , (Complex 3 (-7), "3 - 7 * i")
    , (Complex (-3) (-7), "-3 - 7 * i")
    , (Complex 3 0, "3")
    , (Complex (-3) 0, "-3")
    , (Complex 0 7, "7 * i")
    , (Complex 0 (-7), "-7 * i")
    ]

testsComplex =
    [ i * i ~?= -1
    , 3 + i ~?= Complex 3 1
    , 3 * i ~?= Complex 0 3
    , (3 + 4 * i) * (4 - 3 * i) ~?= 24 + 7 * i
    , 3.2 + 1.4 * i ~?= Complex 3.2 1.4
    , 2 / (1 + i) ~?= 1 - i
    ] ++ map (\(c,s) -> show c ~?= s) testCasesComplex
      ++ map (\(c,s) -> read s ~?= c) testCasesComplex

-- Tree

tree1 = Node "a" [Node "b" [Node "f" []], Node "c" [Node "d" []], Node "e" []]
tree2 = Node 1 [Node 2 [Node 4 [], Node 5 []], Node 3 []]
tree3 = Node "1" [Node "2" [Node "4" [], Node "5" []], Node "3" []]

testsTree =
    [ show tree1 ~?= "\"a\":{\"b\":{\"f\"},\"c\":{\"d\"},\"e\"}"
    , show tree2 ~?= "1:{2:{4,5},3}"
    , fmap show tree2 ~?= tree3
    , (read "1:{2:{4,5},3}" :: Tree Int) ~?= tree2
    , (reads "1:{2:{4,5},}" :: [(Tree Int,String)]) ~?= [(Node 1 [],":{2:{4,5},}")]
    , (reads ",1:{2:{4,5},}" :: [(Tree Int,String)]) ~?= []
    ]

-- Maybe1

testsMaybe1 =
    [ mempty ~?/= Maybe1 (Nothing :: Maybe [()])
    , mempty <> Maybe1 (Just [True,False]) ~?= Maybe1 (Just [True,False])
    , Maybe1 (Just [True,False]) <> mempty ~?= Maybe1 (Just [True,False])
    , mempty <> Maybe1 (Nothing :: Maybe [()]) ~?= Maybe1 Nothing
    , Maybe1 (Nothing :: Maybe [()]) <> mempty ~?= Maybe1 Nothing
    , (Maybe1 tt <> Maybe1 tt) <> Maybe1 tt ~?= Maybe1 tt <> (Maybe1 tt <> Maybe1 tt)
    , (Maybe1 ff <> Maybe1 tt) <> Maybe1 tt ~?= Maybe1 ff <> (Maybe1 tt <> Maybe1 tt)
    , (Maybe1 tt <> Maybe1 ff) <> Maybe1 tt ~?= Maybe1 tt <> (Maybe1 ff <> Maybe1 tt)
    , (Maybe1 ff <> Maybe1 ff) <> Maybe1 tt ~?= Maybe1 ff <> (Maybe1 ff <> Maybe1 tt)
    , (Maybe1 tt <> Maybe1 tt) <> Maybe1 ff ~?= Maybe1 tt <> (Maybe1 tt <> Maybe1 ff)
    , (Maybe1 ff <> Maybe1 tt) <> Maybe1 ff ~?= Maybe1 ff <> (Maybe1 tt <> Maybe1 ff)
    , (Maybe1 tt <> Maybe1 ff) <> Maybe1 ff ~?= Maybe1 tt <> (Maybe1 ff <> Maybe1 ff)
    , (Maybe1 ff <> Maybe1 ff) <> Maybe1 ff ~?= Maybe1 ff <> (Maybe1 ff <> Maybe1 ff)
    , (Maybe1 Nothing <> Maybe1 tt) <> Maybe1 tt ~?= Maybe1 Nothing <> (Maybe1 tt <> Maybe1 tt)
    , (Maybe1 Nothing <> Maybe1 tt) <> Maybe1 ff ~?= Maybe1 Nothing <> (Maybe1 tt <> Maybe1 ff)
    , (Maybe1 Nothing <> Maybe1 ff) <> Maybe1 tt ~?= Maybe1 Nothing <> (Maybe1 ff <> Maybe1 tt)
    , (Maybe1 Nothing <> Maybe1 ff) <> Maybe1 ff ~?= Maybe1 Nothing <> (Maybe1 ff <> Maybe1 ff)
    , (Maybe1 tt <> Maybe1 Nothing) <> Maybe1 tt ~?= Maybe1 tt <> (Maybe1 Nothing <> Maybe1 tt)
    , (Maybe1 tt <> Maybe1 Nothing) <> Maybe1 ff ~?= Maybe1 tt <> (Maybe1 Nothing <> Maybe1 ff)
    , (Maybe1 ff <> Maybe1 Nothing) <> Maybe1 tt ~?= Maybe1 ff <> (Maybe1 Nothing <> Maybe1 tt)
    , (Maybe1 ff <> Maybe1 Nothing) <> Maybe1 ff ~?= Maybe1 ff <> (Maybe1 Nothing <> Maybe1 ff)
    , (Maybe1 tt <> Maybe1 tt) <> Maybe1 Nothing ~?= Maybe1 tt <> (Maybe1 tt <> Maybe1 Nothing)
    , (Maybe1 tt <> Maybe1 ff) <> Maybe1 Nothing ~?= Maybe1 tt <> (Maybe1 ff <> Maybe1 Nothing)
    , (Maybe1 ff <> Maybe1 tt) <> Maybe1 Nothing ~?= Maybe1 ff <> (Maybe1 tt <> Maybe1 Nothing)
    , (Maybe1 ff <> Maybe1 ff) <> Maybe1 Nothing ~?= Maybe1 ff <> (Maybe1 ff <> Maybe1 Nothing)
    ]

-- Maybe2

testsMaybe2 =
    [ mempty ~?/= Maybe2 (Nothing :: Maybe [()])
    , mappend (Maybe2 $ Just [True,False]) mempty ~?= Maybe2 (Just [True,False])
    , mappend mempty (Maybe2 (Nothing :: Maybe [()])) ~?= Maybe2 Nothing
    , mappend (Maybe2 (Nothing :: Maybe [()])) mempty ~?= Maybe2 Nothing
    , (Maybe2 tt <> Maybe2 tt) <> Maybe2 tt ~?= Maybe2 tt <> (Maybe2 tt <> Maybe2 tt)
    , (Maybe2 ff <> Maybe2 tt) <> Maybe2 tt ~?= Maybe2 ff <> (Maybe2 tt <> Maybe2 tt)
    , (Maybe2 tt <> Maybe2 ff) <> Maybe2 tt ~?= Maybe2 tt <> (Maybe2 ff <> Maybe2 tt)
    , (Maybe2 ff <> Maybe2 ff) <> Maybe2 tt ~?= Maybe2 ff <> (Maybe2 ff <> Maybe2 tt)
    , (Maybe2 tt <> Maybe2 tt) <> Maybe2 ff ~?= Maybe2 tt <> (Maybe2 tt <> Maybe2 ff)
    , (Maybe2 ff <> Maybe2 tt) <> Maybe2 ff ~?= Maybe2 ff <> (Maybe2 tt <> Maybe2 ff)
    , (Maybe2 tt <> Maybe2 ff) <> Maybe2 ff ~?= Maybe2 tt <> (Maybe2 ff <> Maybe2 ff)
    , (Maybe2 ff <> Maybe2 ff) <> Maybe2 ff ~?= Maybe2 ff <> (Maybe2 ff <> Maybe2 ff)
    , (Maybe2 Nothing <> Maybe2 tt) <> Maybe2 tt ~?= Maybe2 Nothing <> (Maybe2 tt <> Maybe2 tt)
    , (Maybe2 Nothing <> Maybe2 tt) <> Maybe2 ff ~?= Maybe2 Nothing <> (Maybe2 tt <> Maybe2 ff)
    , (Maybe2 Nothing <> Maybe2 ff) <> Maybe2 tt ~?= Maybe2 Nothing <> (Maybe2 ff <> Maybe2 tt)
    , (Maybe2 Nothing <> Maybe2 ff) <> Maybe2 ff ~?= Maybe2 Nothing <> (Maybe2 ff <> Maybe2 ff)
    , (Maybe2 tt <> Maybe2 Nothing) <> Maybe2 tt ~?= Maybe2 tt <> (Maybe2 Nothing <> Maybe2 tt)
    , (Maybe2 tt <> Maybe2 Nothing) <> Maybe2 ff ~?= Maybe2 tt <> (Maybe2 Nothing <> Maybe2 ff)
    , (Maybe2 ff <> Maybe2 Nothing) <> Maybe2 tt ~?= Maybe2 ff <> (Maybe2 Nothing <> Maybe2 tt)
    , (Maybe2 ff <> Maybe2 Nothing) <> Maybe2 ff ~?= Maybe2 ff <> (Maybe2 Nothing <> Maybe2 ff)
    , (Maybe2 tt <> Maybe2 tt) <> Maybe2 Nothing ~?= Maybe2 tt <> (Maybe2 tt <> Maybe2 Nothing)
    , (Maybe2 tt <> Maybe2 ff) <> Maybe2 Nothing ~?= Maybe2 tt <> (Maybe2 ff <> Maybe2 Nothing)
    , (Maybe2 ff <> Maybe2 tt) <> Maybe2 Nothing ~?= Maybe2 ff <> (Maybe2 tt <> Maybe2 Nothing)
    , (Maybe2 ff <> Maybe2 ff) <> Maybe2 Nothing ~?= Maybe2 ff <> (Maybe2 ff <> Maybe2 Nothing)
    ]

tt = Just (Xor True)
ff = Just (Xor False)

-- main

(~?/=) :: (Eq a, Show a) => a -> a -> Test
x ~?/= y = TestCase $ assertBool (show x ++ " shoud not be equal to " ++ show y) (x /= y)

main = fmap (\_ -> ()) $ runTestTT $ test $
       label "Complex" testsComplex
    ++ label "Tree" testsTree
    ++ label "Maybe1" testsMaybe1
    ++ label "Maybe2" testsMaybe2
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
