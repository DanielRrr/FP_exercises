import Control.Monad
import qualified Data.Map as M
import Test.HUnit

import Expr
import Eval

-- Реализуйте функции, вычисляющие выражения, используя монаду Eval.

getInt :: Eval Value -> Eval Integer
getInt m = undefined

getBool :: Eval Value -> Eval Bool
getBool m = undefined

if' :: Eval Value -> Eval () -> Maybe (Eval ()) -> Eval ()
if' c t e = undefined

evalExpr :: Expr -> Eval Value
evalExpr = undefined

evalStatement :: Statement -> Eval ()
evalStatement = undefined

------------------------------------------------------------------------------------------------
-- tests

test1 = Var "x" .+ Var "y" .* Const (I 3) .+ Var "z" .==
        Var "y" .+ Const (I 5) .* Var "y" .+ Const (I 7) .* Var "z" .+ Var "y" .* Const (I 3)

test2 = neg (Const $ I 5) .+ neg (Const $ I 3) .* Const (I 2) .- Const (I 7)

test3 = Compound
    [ "r" $= Const (I 1)
    , While (Var "n" .> Const (I 0)) $ Compound
        [ "r" $= Var "r" .* Var "n"
        , "n" $= Var "n" .- Const (I 1)
        ]
    ]

main = fmap (\_ -> ()) $ runTestTT $ test
    [ hasErrors (runEval (evalExpr test1) $ M.fromList [("x",I 3),("y",I 5),("f",I 5)])
    , hasErrors (runEval (evalExpr test1) $ M.fromList [("x",B True),("y",I 5),("z",I 5)])
    , let m = M.fromList [("x",I 24),("y",I 5),("z",I 2)] in runEval (evalExpr test1) m ~?= (Just (B False), [], m)
    , let m = M.fromList [("x",I 42),("y",I 5),("z",I 2)] in runEval (evalExpr test1) m ~?= (Just (B True ), [], m)
    , runEval (evalExpr test2) M.empty ~?= (Just (I (-18)), [], M.empty)
    , runEval (evalStatement test3) (M.fromList [("n",I 5)]) ~?= (Just (), [], M.fromList [("n",I 0),("r",I 120)])
    ]
  where
    hasErrors (_,es,_) = TestCase $ assertBool "This program should have errors" $ not (null es)
