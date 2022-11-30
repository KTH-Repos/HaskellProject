{- @authors - Melvin & Tomas -}
{- Denna funktion skickar vidare inputs till fh som har 
tre argument -}
fib :: Int -> Int 
fib n = fh 1 0 n 

{- Funktionen tar in tre argument. a & b är två föregående talen 
i följden. n anger hur många steg som återstår fram till resultatet.  -}
fh :: Int -> Int -> Int -> Int
fh a b n | n == 0     = b
         | otherwise  = fh (a+b) a (n-1)