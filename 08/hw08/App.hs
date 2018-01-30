import Control.Applicative

data Tree a = Node a [Tree a]

instance Functor Tree where
    fmap f (Node x xs) = Node (f x) $ map (fmap f) xs

instance Applicative Tree where
    pure x = Node x []
    (Node f fs) <*> (Node x xs) = Node (f x) (zipWith (<*>) fs xs)
