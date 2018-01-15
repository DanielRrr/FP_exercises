module Xor where

import Data.Monoid

-- Определите instance Monoid для Bool так, чтобы mappend реализовывал операцию xor.

newtype Xor = Xor { getXor :: Bool } deriving (Eq,Show)

instance Monoid Xor where
    mempty = undefined
    mappend = undefined
