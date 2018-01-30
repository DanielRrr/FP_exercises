module Xor where

import Data.Monoid

newtype Xor = Xor { getXor :: Bool } deriving (Eq,Show)

instance Monoid Xor where
    mempty = Xor False
    mappend = (\x y -> Xor (x `xor` y))
      where
        xor a b = (a || b) && not (a && b)
