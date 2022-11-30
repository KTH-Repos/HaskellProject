{- @authors - Melvin & Tomas -}

{- Funktionen väljer vartannat element i listan i ordningen 1:st, 3:e osv.
 -}
f :: [a] -> [a]
f [] = []
f(h:t) = h : f(drop 1 t)  

{- Funktionen skyfflar listan genom att välja ut varannat element och sedan skyffla de återstående
elementen i listan. Den tar in hjälp av funktionen f.  -}
skyffla :: [a] -> [a]
skyffla [] = []
skyffla a = f a ++ skyffla(f(drop 1 a)) 
