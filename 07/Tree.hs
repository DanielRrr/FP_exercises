module Tree where

-- (2 балла)

data Tree a = Node { value :: a, children :: [Tree a] } deriving Eq

-- show и read должны работать как описано в тестах в Main.hs
instance Show a => Show (Tree a) where
    show (Node x []) = show x
    show (Node x (y:ys)) = show x ++ ":{" ++ show y ++ showL ys ++ "}:" where
                                                    showL [] = ""
                                                    showL (z:zs) = "," ++ show z ++ showL zs

instance Read a => Read (Tree a) where
    readsPrec = undefined

instance Functor Tree where
    fmap f (Node x xs) = Node (f x) $ map (fmap f) xs
