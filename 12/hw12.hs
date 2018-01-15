{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error
import qualified Data.Map as M

data Term = Var String | App Term Term | Lam String Term
data Type = TVar Int | Arr Type Type

instance Show Type where
    showsPrec _ (TVar i) = showString "x" . shows i
    showsPrec p (Arr x y) = showParen (p > 0) $ showsPrec 1 x . showString " -> " . showsPrec 0 y

type Equation = (Type,Type)
type Solution = M.Map Int Type

-- solveEquation может завершиться неуспехом, если возникает рекурсивное уравнение x = t, где x -- переменная, а t -- тип, в котором встречается x.
-- В этом случае нужно вернуть Left (x,t).
-- Иначе нужно вернуть Right от решения.
solveEquations :: [Equation] -> Either (Int,Type) Solution
solveEquations = undefined

-- substSolution s t подставляет в t вместо всех переменных значения из решения s.
substSolution :: Solution -> Type -> Type
substSolution = undefined

genFreshVar :: MonadState Int m => m Int
genFreshVar = undefined

getVarType
    :: (Ord a, MonadReader (M.Map a t) m, MonadError a m)
    => a -> m t
getVarType = undefined

addEquation :: MonadWriter [Equation] m => Equation -> m ()
addEquation = undefined

withVarAdded
    :: MonadReader (M.Map String Type) m
    => String -> Type -> m a -> m a
withVarAdded = undefined

-- mkEquation состовляет список уравнений. используя функции genFreshVar, getVarType, addEquation и withVarAdded.
mkEquations :: 
    ( MonadState Int m
    , MonadWriter [Equation] m
    , MonadReader (M.Map String Type) m
    , MonadError String m
    ) => Term -> m Type
mkEquations = undefined

inferType :: Term -> Either String Type
inferType t =
    let (r,eqs) = runWriter $ runErrorT $ runReaderT (evalStateT (mkEquations t) 0) M.empty
    in case (r, solveEquations eqs) of
        (Left x, _) -> Left $ "Could not find variable '" ++ x ++ "'"
        (_, Left (x,tt)) -> Left $ "Cannot construct the infinite type: " ++ show (TVar x) ++ " = " ++ show tt
        (Right tt, Right s) -> Right (substSolution s tt)

tests =
    [ Lam "x" (Var "x")
    , Lam "x" (Var "y")
    , Lam "x" $ Lam "y" (Var "x")
    , Lam "x" $ Lam "y" $ Lam "z" $ App (App (Var "x") (Var "z")) $ App (Var "y") (Var "z")
    , Lam "x" $ App (Lam "x" $ Var "x") (Var "x")
    , Lam "x" $ App (Lam "x" $ Var "x") (Var "y")
    , Lam "x" $ App (Var "x") (Var "x")
    , Lam "x" $ Lam "y" $ App (Var "x") $ App (Var "y") (Var "x")
    ]

main = forM_ tests $ \t ->
    case inferType t of
        Left err -> putStrLn err
        Right tt -> print tt
