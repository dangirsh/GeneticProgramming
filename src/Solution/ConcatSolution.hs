{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


module Solution.ConcatSolution (
    ConcatSolution
) where


import Solution (Solution, Fitness, randomSol, fitness, sizeSol, mate, cmpSol, hashSol)
import Common (m, randInt, randElem, random, crossover_p)
import Allele (Allele, Stack, Term(Term), mutateAllele, randAllele, input, output, stackFunc, name)
import Control.Monad (replicateM)
import Control.Applicative ((<$>))
import Foreign.Marshal.Utils (fromBool)
import Data.Hashable (hash)


type ConcatSolution = [Allele]


instance Solution ConcatSolution where

    randomSol = replicateM m randAllele

    fitness = _fitness evenParity 7

    mate (p1, p2) = do
        r <- random
        if r < crossover_p then --FIXME
            reduce <$> crossover p1 p2 >>= mutate
        else
            randomSol

    sizeSol = length

    cmpSol s1 s2 =
        let c = compare (fitness s1) (fitness s2) in
        if c == EQ then
            compare (length s2) (length s1)
        else
            c

    hashSol = hash . concatMap name


-- idea: [t1, t2, t3] --> f1 . f2 . f3
evalSol :: ConcatSolution -> Stack -> Maybe Stack
evalSol [] input = Just input
evalSol (x:xs) input = stackFunc x input >>= evalSol xs


evenParity :: [Bool] -> Bool
evenParity = even . sum . map (\b -> if b then 1 else 0)


-- # of times it gets even parity correct for inputs of length l
_fitness f l sol = sum $ map counter inputs
    where
        inputs = replicateM l [True, False]
        counter input = case evalSol sol input of
            Nothing     -> 0
            Just []     -> error "Logic error - should never happen!"
            Just (x:xs) -> fromBool $ x == f input


-- return indicies of terms in sol that have inp inputs and out ouputs
splits :: Int -> Int -> ConcatSolution -> [Int]
splits inp out sol = map fst $ filter matches $ zip [0..(length sol - 1)] sol
    where
        matches (_, Term {input = i, output = o, stackFunc = _, name = _}) =
            i == inp && o == out


reduce :: ConcatSolution -> ConcatSolution
reduce [] = []
reduce (x:[]) = [x]
reduce (x1:x2:xs) =
    case (name x1, name x2) of
        ("not", "not") -> reduce xs
        otherwise      -> x1:x2:reduce xs


-- RS
crossover :: ConcatSolution -> ConcatSolution -> IO ConcatSolution
crossover p1 p2 =
    let l = length p1 in
    if l < 3 then return p2 else do
        i <- randInt (l - 1)
        let (a, _) = splitAt (i + 1) p1
        let x = last a
        case splits (input x) (output x) p2 of
            [] -> return p1
            xs -> randElem xs >>= \j -> return $ a ++ snd (splitAt j p2)


safeTail [] = []
safeTail xs = tail xs

-- RS
mutate :: ConcatSolution -> IO ConcatSolution
mutate cs = do
    let l = length cs
    i <- randInt (l - 2)
    ma <- mutateAllele (cs !! i)
    let (xs, ys) = splitAt i cs
    return $ xs ++ [ma] ++ safeTail ys

