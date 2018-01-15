module Fisole(Fisole,runFisole,getCharF,putCharF) where

import System.IO

newtype Fisole a = Fisole { unFisole :: Handle -> IO a }

runFisole :: Fisole a -> String -> IO a
runFisole = undefined

instance Monad Fisole where
    return = undefined
    (>>=) = undefined

getCharF :: Fisole Char
getCharF = undefined

putCharF :: Char -> Fisole ()
putCharF = undefined
