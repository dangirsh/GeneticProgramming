module Solution (
    Solution,
    Fitness,
    randomSol,
    fitness,
    mate,
    cmpSol
) where


import Common (m, randInt, randElem)
import Allele (Allele, Stack, Term(Term), mutateAllele, randAllele, input, output, stackFunc, name)
import Control.Monad (replicateM)

type Solution = [Allele]


type Fitness = Int


-- RS
randomSol :: IO Solution
randomSol = sequence $ replicate m randAllele


-- RS
--fitness :: Solution -> Fitness
--fitness solution = let (a, _) = foldl countInOrder (0, -1) solution in a
--    where
--        countInOrder (count, last) next
--            | next > last = (count + 1, next)
--            | otherwise = (count - 1, next)


-- compileSol [t1, t2, t3] --> f1 . f2 . f3
compileSol :: Solution -> (Stack -> Stack)
compileSol [] = id
compileSol sol = foldl (.) id $ map stackFunc sol


evenParity :: [Bool] -> Bool
evenParity = even . sum . map (\b -> if b then 1 else 0)


fitness :: Solution -> Fitness
fitness = _fitness 10 5

-- # of times it gets even parity correct (for i inputs of length l)
-- currently only takes the i lexographically first inputs. change to be random!
_fitness i l sol = sum $ map counter inputs
    where
        inputs = take i $ replicateM l [True, False]
        eval = compileSol sol
        counter input = if (head (eval input) == evenParity input) then 1 else 0

-- return indecies of terms in sol that have inp inputs and out ouputs
splits :: Int -> Int -> Solution -> [Int]
splits inp out sol = map fst $ filter matches $ zip [0..((length sol) - 1)] sol
    where
        matches (_, Term {input = i, output = o, stackFunc = _, name = _}) =
            i == inp && o == out


-- RS
crossover :: Solution -> Solution -> IO Solution
crossover p1 p2 = do
    i <- randInt (m - 1)
    let (a, _) = splitAt (i + 1) p1
    let x = last a   -- deal with small m case later
    case splits (input x) (output x) p2 of
        [] -> return p1
        xs -> (randElem xs) >>= \j -> return $ a ++ (snd $ splitAt j p2)


-- RS
mutate :: Solution -> IO Solution
mutate solution = do
    i <- randInt m
    ma <- mutateAllele (solution !! i)
    let (xs, _:ys) = splitAt i solution
    return $ xs ++ [ma] ++ ys


mate :: (Solution, Solution) -> IO Solution
mate (p1, p2) = crossover p1 p2 -- >>= mutate


cmpSol :: Solution -> Solution -> Ordering
cmpSol s1 s2 = compare (fitness s1) (fitness s2)