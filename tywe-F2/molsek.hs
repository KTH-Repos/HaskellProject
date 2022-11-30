{-Authors: Melvin & Tomas-}

module F2 where
import Data.List
--import Distribution.PackageDescription.FieldGrammar (validateTestSuite)
data MolSeq = Molekyl String String Bool deriving (Show)                         -- 
data Profile = Matrix String Int Bool [[(Char, Int )]] deriving (Show)           -- matrix = list of sequences, String = name, Int = number of sequences, Bool = Dna or protein? 

{-Author: Tomas-}
isDNA :: String -> Bool             --Returns true if DNA, otherwise false if protein
isDNA [] = True
isDNA (h:t) | h `elem` "ACGT" = isDNA t
            | otherwise = False

{-Author: Tomas-}

string2seq :: String -> String -> MolSeq                   --Check if a sequence is of protein or DNA. 
string2seq name sek | isDNA sek = Molekyl name sek True    --True is DNA, otherwise false for protein
                    | otherwise = Molekyl name sek False

{-Author: Tomas-}

seqType::MolSeq -> Bool                 --Return true if sequence is of DNA, otherwise false (for protein)
seqType (Molekyl _ _ isDNA) = isDNA

{-Author: Tomas-}

seqName :: MolSeq -> String             --Return the name of a molseq datatype
seqName (Molekyl name _ _) = name

{-Author: Tomas-}

seqSequence :: MolSeq -> String         --Return the sequence of a molseq datatype
seqSequence (Molekyl _ sek _) = sek

{-Author: Tomas-}

seqLength :: MolSeq ->  Int             --Return the length of a sequence of a molseq
seqLength (Molekyl _ sek _) = length sek

{-Author: Tomas-}

humDist :: String -> String -> Double      --Calculate Hamming-distance
humDist [] [] = 0.0
humDist (h1:t1) (h2:t2) | h1 /= h2 = 1 + humDist t1 t2
                        | otherwise = humDist t1 t2

{-Author: Tomas-}

seqDistance :: MolSeq -> MolSeq -> Double       --Calculate the sequence distance of two similar molseq datatypes
seqDistance (Molekyl name sek1 id1) (Molekyl _ sek2 id2) | id1 /= id2 = error "wrong types"
                                                         | id1 = jukesCan(humDist sek1 sek2 / fromIntegral (seqLength (Molekyl name sek1 id1)))    --DNA
                                                         | otherwise = poisMod(humDist sek1 sek2 / fromIntegral (seqLength (Molekyl name sek1 id1)))  --PROTEIN

{-Author: Tomas-}

jukesCan :: Double -> Double                --Implementation of the formula of Jukes-Cantor to find evolutionary distance of two DNAs  
jukesCan a | a > 0.74 = 3.3
           | otherwise = -3/4 * log (1 - (4*a)/3)

{-Author: Tomas-}

poisMod :: Double -> Double                 --Implementation of the formula of Poissson modell to find evolutionary distance of two proteins
poisMod a  | a > 0.94 = 3.7
           | otherwise = -19/20 * log (1 - (20*a)/19)
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-Author: Melvin-}

molseqs2profile :: String -> [MolSeq] -> Profile --Store information about the profile in the matrix
molseqs2profile name molseqs = Matrix name (length molseqs) (seqType (head molseqs)) (makeProfileMatrix molseqs)

{-Author: Melvin-}

profileName :: Profile -> String --Returns the name of the profile
profileName (Matrix name _ _ _) = name

{-Author: Melvin-}

isDNAForProfile :: Profile -> Bool  --True if the Profile is DNA and not Protein
isDNAForProfile (Matrix _ _ isDNA _) = isDNA 

{-Author: Melvin-}

profileFrequency :: Profile -> Int -> Char -> Double --Returns the normalised frequency of a character at an index in a profile
profileFrequency (Matrix _ numSeqs _ mat) i c = fromIntegral((matchFstTuple (mat !! i) c))  / (fromIntegral numSeqs)

{-Author: Melvin-}

matchFstTuple :: (Eq a) => [(a, b)] -> a -> b --Looks through a list of tuples. Returns the second item in the tuple when the first item matches the parameter/index
matchFstTuple (head:xs) index | fst head == index = snd head
                              | otherwise = matchFstTuple xs index

{-Author: Melvin-}
                              
profileDistanceMatrix :: Profile -> Profile -> [[Double]]                                                       --Calculate the differences between corresponding rows&columns of the profiles
profileDistanceMatrix (Matrix mName mNumSeq mIsDNA m) (Matrix mpName mpNumSeq mpIsDNA mp) = answer where   
    letters = if mIsDNA then nucleotides else aminoacids                                                        --Evaluates if the profiles are DNA or Protein
    answer = profileRowsDistances (Matrix mName mNumSeq mIsDNA m) (Matrix mpName mpNumSeq mpIsDNA mp) letters   --Uses a helper function to find the distances between corresponding rows&columns

{-Author: Melvin-}

profileRowsDistances :: Profile -> Profile -> [Char] -> [[Double]]                                              --Recursive helper function
profileRowsDistances _ _ [] = []                                                                                --Recurses over the letters in the Char-array (corresponding to rows in the profiles) 
profileRowsDistances m mp (c:cs) = (profileRowsDistancesHelper m mp c) : profileRowsDistances m mp cs           --Uses another helper function to join the row-wise distances into a distance matrix

{-Author: Melvin-}

profileRowsDistancesHelper :: Profile -> Profile -> Char -> [Double]                                                        --Calculates the list of differences between two rows
profileRowsDistancesHelper (Matrix mName mNumSeq mIsDNA m) (Matrix mpName mpNumSeq mpIsDNA mp) c = list where
    cols = length m                                                                                               --Count number of columns
    list1 = zipWith3 (profileFrequency) (replicate cols (Matrix mName mNumSeq mIsDNA m)) [0..(cols-1)] (replicate cols c)       --Calculate relative frequencies in first matrix
    list2 = zipWith3 (profileFrequency) (replicate cols (Matrix mpName mpNumSeq mpIsDNA mp)) [0..(cols-1)] (replicate cols c)   --Calculate relative frequencies in second matrix
    list = zipWith (\x y -> abs (x - y)) list1 list2                                                                        --Subtract relative frequencies

{-Author: Melvin-}

profileDistance :: Profile -> Profile -> Double
profileDistance m mp = matrixSum (profileDistanceMatrix m mp) --Sums the distance matrix of two profiles

{-Author: Melvin-}

matrixSum :: (Num a) => [[a]] -> a      --Sum a matrix of numbers
matrixSum m = sum ( map sum m)

{- Rad (i & ii) Varje bokstav i sekvensen får ett 0 i första steget av deklarationen. 
Ex: zip "ACCCTGG" (replicate (length "ACCCTGG") 0) ger ut
[('A',0),('C',0),('C',0),('C',0),('T',0),('G',0),('G',0)] -} 
{- Rad (iii) Få ut sekvensen av varje MolSeq -} 
{- Rad (iv) Lambda funktionen sorterar och grupperar bokstäverna först. Sen räknas och 
skrivs för varje bokstav antalet gånger de förekommer. Ex: om s1 innehåller lambda 
funktionen ger F2> s1 "AACCTTGG" följande [('A',2),('C',2),('G',2),('T',2)] -}
{- Lambda funktionen i sista raden joinar två listor av tupler av [(Char,Int)] sådana att
om det finns två tupler med samma char i l respektiv defaults slås ihop dem.   -}

{-Author: Labbhäftet-}
nucleotides = "ACGT"
aminoacids = sort "ARNDCEQGHILKMFPSTWYV"
makeProfileMatrix :: [MolSeq] -> [[(Char, Int )]]
makeProfileMatrix [] = error "Empty sequence list"
makeProfileMatrix sl = res
    where
        t = seqType (head sl)::Bool
        defaults =
            if t then
                zip nucleotides (replicate (length nucleotides) 0) -- Rad (i)   
            else
                zip aminoacids (replicate (length aminoacids) 0) -- Rad (ii)
        strs = map seqSequence sl -- Rad (iii)
        tmp1 = map (map (\x -> ((head x), (length x))) . group . sort) (transpose strs) -- Rad (iv)
        equalFst a b = (fst a) == (fst b)
        res = map sort (map (\l -> unionBy equalFst l defaults) tmp1)
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-Author: Tomas-}
class Evol a where                          --Declaration of the typeclass Evol with corresponding methods
    distance :: a -> a -> Double 
    name :: a -> String
    isDna :: a -> Bool                        -- Checks If molseq or profile is of DNA....
    --returnDnaSeq :: a -> String             -- Only for MolSeq

{-Author: Tomas-}
instance Evol MolSeq where                                          --Create instance of molseq as part of Evol 
    distance molSeq1 molSeq2 = seqDistance molSeq1 molSeq2 
    name molseq = seqName molseq
    isDna molseq = seqType molseq

{-Author: Tomas-}
instance Evol Profile where                                         --Create instance of profile as part of evol 
    distance profile1 profile2 = profileDistance profile1 profile2
    name profile = profileName profile
    isDna profile = isDNAForProfile profile

{-Author: Tomas-}
distanceMatrix :: Evol a => [a] -> [(String, String, Double)]               --Calculate the distance of two Evol types placed in a list
distanceMatrix [] = []
distanceMatrix (x:xs) = [(name x, name x, distance x x)] ++ distanceMatrixHelper x xs ++ distanceMatrix xs 

{-Author: Tomas-}
distanceMatrixHelper :: Evol a => a -> [a] -> [(String, String, Double)]        --Helping method that recursively calculates evolutioinary distance between an Evol and a list of Evol 
distanceMatrixHelper x [] = []      
distanceMatrixHelper x (h:t) = (name x, name h, distance x h) : distanceMatrixHelper x t

