{- @authors - Melvin & Tomas -}

{- Funktionen tar in en lista av chars. Den kollar om första elementet är en konsonant.
Om ja, då dropas de två följande chars. Annars (om char är vokal) forstätter den med nästa element.  -}
karpsravor :: [Char] -> [Char]
karpsravor [] = []
karpsravor (h:t) | h `elem` "bcdfghjklmnpqrstvwxz" = h : karpsravor(drop 2 t) 
                 | otherwise = h : karpsravor t 

