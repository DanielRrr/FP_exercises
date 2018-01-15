module Eval
    ( Eval, runEval
    , Error, Store
    , update, getVar
    ) where

import qualified Data.Map as M
import Data.List
import Control.Monad

import Expr

type Error = String
type Store = M.Map String Value

-- Монада Eval предназначена для вычисления выражений.
-- В ней инкапсулирована вся необходимая информация и побочные эффекты.

newtype Eval a = Eval { runEval :: Store -> (Maybe a, [Error], Store) }

-- Этот instance объединяет сразу несколько instance для лругих монад
-- Во-первых, State Store
-- Во-вторых, Writer [Error]
-- В-третьих, Maybe
-- Так что можете сверится с тем как определены эти монады.
instance Monad Eval where
    return x = Eval undefined
    Eval m >>= k = Eval undefined

-- MonadPlus - аналог Alternative для монад
-- mzero - вычисление, которое ничего не делает, сразу завершается неуспехом
-- mplus m1 m2 пытается выполнить m1, если тот завершился неуспехом, выполняет m2
instance MonadPlus Eval where
    mzero = Eval undefined
    mplus (Eval l) (Eval r) = Eval undefined

-- update k v обновляет хранилище, записывая по ключу k значение v.
update :: String -> Value -> Eval ()
update k v = Eval undefined

-- gatVar v достает из хранилища значение по ключу k.
-- Если такого ключа нет, то ошибка сохраняется внутри монады.
getVar :: String -> Eval Value
getVar k = Eval undefined
