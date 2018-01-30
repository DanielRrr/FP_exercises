module Maybe where

import Data.Monoid
import Control.Applicative

-- Реализуйте два различных instance Monoid для Maybe так, чтобы mempty не был равен Nothing.
-- Решения, где у mappend аргументы переставлены местами не считаются различными.
--
-- Обратите внимание, что в первом случае решение работать должно для любого моноида a.
-- Во втором дополнительно предполагается, что на a определено сравнение.
--
-- Операция mappend должна быть ассоциативна, mempty должна быть единицей слева и справа для mappend.

newtype Maybe1 a = Maybe1 { getMaybe1 :: Maybe a } deriving (Eq,Show)

instance Functor Maybe1 where
  fmap f (Maybe1 Nothing) = Maybe1 Nothing
  fmap f (Maybe1 (Just x)) = Maybe1 (Just (f x))

instance Applicative Maybe1 where
  pure x = Maybe1 (Just x)
  (Maybe1 Nothing) <*> _ = Maybe1 Nothing
  _ <*> (Maybe1 Nothing) = Maybe1 Nothing
  (Maybe1 (Just f)) <*> (Maybe1 (Just x)) = Maybe1 (Just (f x))

instance Monoid a => Monoid (Maybe1 a) where
    mempty = Maybe1 (Just mempty)
    mappend = liftA2 mappend

newtype Maybe2 a = Maybe2 { getMaybe2 :: Maybe a } deriving (Eq,Show)

instance (Monoid a, Eq a) => Monoid (Maybe2 a) where
    mempty = undefined
    mappend = undefined
