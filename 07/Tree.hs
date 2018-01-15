module Tree where

-- (2 балла)

data Tree a = Node { value :: a, children :: [Tree a] } deriving Eq

-- show и read должны работать как описано в тестах в Main.hs
instance Show a => Show (Tree a) where
    show = undefined

instance Read a => Read (Tree a) where
    readsPrec = undefined

instance Functor Tree where
    fmap = undefined
