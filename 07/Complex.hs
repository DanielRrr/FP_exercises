module Complex where

-- (2 балл)

data Complex = Complex { real :: Double, im :: Double } deriving Eq

instance Num Complex where
    (+) = undefined
    (*) = undefined
    fromInteger = undefined
    negate = undefined
    abs = undefined
    
    signum _ = error "Complex: signum isn't defined"

instance Fractional Complex where
    (/) = undefined
    fromRational = undefined

-- show и read должны работать как описано в тестах в Main.hs
instance Show Complex where
    show = undefined

instance Read Complex where
    readsPrec = undefined

i :: Complex
i = undefined
