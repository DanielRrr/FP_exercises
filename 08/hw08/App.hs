import Control.Applicative

data Tree a = Node a [Tree a]

instance Functor Tree where
    fmap = undefined

instance Applicative Tree where
    pure = undefined
    (<*>) = undefined
