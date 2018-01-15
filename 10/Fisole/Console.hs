module Console(Console,runConsole,getCharC,putCharC) where

-- Этот файл здесь только для примера.

newtype Console a = Console { runConsole :: IO a }

instance Monad Console where
    return = Console . return
    Console m >>= k = Console $ m >>= runConsole . k

getCharC :: Console Char
getCharC = Console getChar

putCharC :: Char -> Console ()
putCharC = Console . putChar
