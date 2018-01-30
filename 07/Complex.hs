module Complex where

-- (2 балл)

data Complex = Complex { real :: Double, im :: Double } deriving Eq

instance Num Complex where
    (+) (Complex x1 y1) (Complex x2 y2) = Complex (x1 + x2) (y1 + y2)
    (*) (Complex x1 y1) (Complex x2 y2) = Complex (x1 * x2 - y1 * y2) (x1 * y2 + x2 * y1)
    fromInteger n = Complex (fromInteger n) 0
    negate (Complex a b) = Complex (-a) (-b)
    abs (Complex a b) = Complex (sqrt (a * a + b * b)) 0

    signum _ = error "Complex: signum isn't defined"

instance Fractional Complex where
    (/) (Complex x1 y1) (Complex x2 y2) =
          Complex ((x1 * x2 + y1 * y1)/(x2 * x2 + y2 * y2)) ((x2 * y1 - x1 * y2)/(x2 * x2 + y2 * y2))
    fromRational a = Complex (fromRational a) 0

-- show и read должны работать как описано в тестах в Main.hs
instance Show Complex where
    show (Complex x1 y1) = case signum y1 of
                              1.0 -> show x1 ++ " + " ++ show y1 ++ " * i"
                              -1.0 -> show x1 ++ " - " ++ show (abs y1) ++ " * i"

instance Read Complex where
    readsPrec = undefined

i :: Complex
i = Complex 0 1
