{- @authors - Melvin & Tomas -}

{- Funktionen tar in en lista av chars. Den kollar om ett element i listan är konsonant så läggs några
bokstaver efter elementet.  -}
rovarsprak :: [Char] -> [Char]
rovarsprak [] = []
rovarsprak (h:t) | h `elem` "bcdfghjklmnpqrstvwxz" = h : 'o' : h : rovarsprak t     --Om ett element i listan är en konsonant sä läggs ett 'o' och en kopia av elementet.
                 | otherwise = h : rovarsprak t                                     --Annars är elementet en vokal och inget händer.