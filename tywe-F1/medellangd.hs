{- @authors - Melvin & Tomas -}

import Data.Char

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
wordCount (h:t) ready | ready && isAlpha h = 1 + wordCount t False      --Om ett element i listan är en bokstav och det föregående elementet inte var en bokstav ökar antalet ord med 1  
                      | ready && not (isAlpha h) = wordCount t True     --Om ett element i listan inte är en bokstav och det föregående elementet inte var en bokstav görs inget
                      | not ready && isAlpha h = wordCount t False      --Om ett element i listan är en bokstav och det föregående elementet var en bokstav så är det slutet eller mitten av ett ord 
                      | otherwise = wordCount t True                    --Om ett element i listan inte är en bokstav och det föregående elementet var en bokstav så har ett ord precis tagit slut (otherwise == not ready && not(isAlpha h))