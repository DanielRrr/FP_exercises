module Maybe where

import Data.Monoid

-- Реализуйте два различных instance Monoid для Maybe так, чтобы mempty не был равен Nothing.
-- Решения, где у mappend аргументы переставлены местами не считаются различными.
-- 
-- Обратите внимание, что в первом случае решение работать должно для любого моноида a.
-- Во втором дополнительно предполагается, что на a определено сравнение.
-- 
-- Операция mappend должна быть ассоциативна, mempty должна быть единицей слева и справа для mappend.

newtype Maybe1 a = Maybe1 { getMaybe1 :: Maybe a } deriving (Eq,Show)

instance Monoid a => Monoid (Maybe1 a) where
    mempty = undefined
    mappend = undefined

newtype Maybe2 a = Maybe2 { getMaybe2 :: Maybe a } deriving (Eq,Show)

instance (Monoid a, Eq a) => Monoid (Maybe2 a) where
    mempty = undefined
    mappend = undefined
