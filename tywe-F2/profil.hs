import Data.List

data Profile = Matrix String Int Bool               -- matrix = list of sequences, String = name, Int = number of sequences, Bool = Dna or protein? 

nucleotides = "ACGT"
aminoacids = sort "ARNDCEQGHILKMFPSTWYV"
makeProfileMatrix :: [MolSeq] -> [[(Char, Int )]]
makeProfileMatrix [] = error "Empty sequence list"
makeProfileMatrix sl = res
    where
        t = seqType (head sl)
        defaults =
            if t then
                zip nucleotides (replicate (length nucleotides) 0) -- Rad (i)
            else
                zip aminoacids (replicate (length aminoacids) 0) -- Rad (ii)
        strs = map seqSequence sl -- Rad (iii)
        tmp1 = map (map (\x -> ((head x), (length x))) . group . sort) (transpose strs) -- Rad (iv)
        equalFst a b = (fst a) == (fst b)
        res = map sort (map (\l -> unionBy equalFst l defaults) tmp1)




--data MolSeq = Molekyl String String Bool deriving (Show)