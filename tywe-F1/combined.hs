module F1 where
import Data.Char

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

{- @authors - Melvin & Tomas -}

{- Funktionen tar in en lista av chars. Den kollar om ett element i listan är konsonant så läggs några
bokstaver efter elementet.  -}
rovarsprak :: [Char] -> [Char]
rovarsprak [] = []
rovarsprak (h:t) | h `elem` "bcdfghjklmnpqrstvwxz" = h : 'o' : h : rovarsprak t     --Om ett element i listan är en konsonant sä läggs ett 'o' och en kopia av elementet.
                 | otherwise = h : rovarsprak t                                     --Annars är elementet en vokal och inget händer.

{- @authors - Melvin & Tomas -}

{- Funktionen tar in en lista av chars. Den kollar om första elementet är en konsonant.
Om ja, då dropas de två följande chars. Annars (om char är vokal) forstätter den med nästa element.  -}
karpsravor :: [Char] -> [Char]
karpsravor [] = []
karpsravor (h:t) | h `elem` "bcdfghjklmnpqrstvwxz" = h : karpsravor(drop 2 t) 
                 | otherwise = h : karpsravor t 

{- @authors - Melvin & Tomas -}

{- Funktionen beräknar medellängden av en lista av char genom att dela antalet bokstaver med antalet 
ord i en sträng. Den tar hjälp av letterCount och wordCount -}
medellangd :: [Char] -> Double
medellangd s = (letterCount s) / (wordCount s True)

{- Funktionen tar in en lista av chars och räknar hur många bokstaver det finns. 
Detta görs genom att ta hjälp av isAlpha.  -}
letterCount :: [Char] -> Double
letterCount [] = 0
letterCount (h:t) | isAlpha h = 1 + letterCount t       --Om ett element i listan är en bokstav så ökar antalet bokstaver med 1
                  | otherwise = letterCount t           --Annars är elementet inte en bokstav

{- Funktionen tar in en lista av chars och returnerar antal ord i strängen.
- ready = om vi stötter på en bokstav är det ett nytt ord. 
- isAlpha = library funktion som kollar ifall en char är en bokstav.  -}
wordCount :: [Char] -> Bool -> Double
wordCount [] _ = 0
wordCount (h:t) ready | ready && isAlpha h = 1 + wordCount t False      --Om ett element i listan är en bokstav och det föregående elementet var inte en bokstav ökar antalet ord med 1  
                      | ready && not (isAlpha h) = wordCount t True     --Om ett element i listan inte är en bokstav och det föregående elemntet var inte en bokstav görs inget
                      | not ready && isAlpha h = wordCount t False      --Om ett element i listan är en bokstav och det föregående elementet var en bokstav så är det slutet eller mitten av ett ord 
                      | otherwise = wordCount t True                    --Om ett element i listan inte är en bokstav och det föregående elementet var en bokstav så har ett ord precis tagit slut

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

