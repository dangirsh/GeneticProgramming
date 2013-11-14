module Solution (
    Solution,
    Fitness,
    randomSol,
    fitness,
    mate,
    cmpSol,
    hashSol
) where


import Common (m, randInt, randElem)
import Allele (Allele, Stack, Term(Term), mutateAllele, randAllele, input, output, stackFunc, name)
import Control.Monad (replicateM)
import Control.Applicative ((<$>))
import Foreign.Marshal.Utils (fromBool)
import Data.Hashable (hash)

type Solution = [Allele]


type Fitness = Int


-- RS
randomSol :: IO Solution
randomSol = replicateM m randAllele


-- idea: [t1, t2, t3] --> f1 . f2 . f3
evalSol :: Solution -> Stack -> Maybe Stack
evalSol [] input = Just input
evalSol (x:xs) input = stackFunc x input >>= evalSol xs


evenParity :: [Bool] -> Bool
evenParity = even . sum . map (\b -> if b then 1 else 0)


fitness :: Solution -> Fitness
fitness = _fitness 3
-- # of times it gets even parity correct for inputs of length l
_fitness l sol = sum $ map counter inputs
    where
        inputs = replicateM l [True, False]
        counter input = case evalSol sol input of
            Nothing     -> -1
            Just []     -> error "Logic error - should never happen!"
            Just (x:xs) -> fromBool $ x == evenParity input


-- return indicies of terms in sol that have inp inputs and out ouputs
splits :: Int -> Int -> Solution -> [Int]
splits inp out sol = map fst $ filter matches $ zip [0..(length sol - 1)] sol
    where
        matches (_, Term {input = i, output = o, stackFunc = _, name = _}) =
            i == inp && o == out


reduce :: Solution -> Solution
reduce [] = []
reduce (x:[]) = [x]
reduce (x1:x2:xs) =
    case (name x1, name x2) of
        ("not", "not") -> reduce xs
        otherwise      -> x1:x2:(reduce xs)


-- RS
crossover :: Solution -> Solution -> IO Solution
crossover p1 p2 =
    let l = length p1 in
    if l < 3 then return p2 else do
        i <- randInt (l - 1)
        let (a, _) = splitAt (i + 1) p1
        let x = last a
        case splits (input x) (output x) p2 of
            [] -> return p1
            xs -> randElem xs >>= \j -> return $ a ++ snd (splitAt j p2)


-- RS
mutate :: Solution -> IO Solution
mutate solution = do
    let l = length solution
    i <- randInt (l - 1)
    ma <- mutateAllele (solution !! i)
    let (xs, _:ys) = splitAt i solution
    return $ xs ++ [ma] ++ ys


mate :: (Solution, Solution) -> IO Solution
mate (p1, p2) = reduce <$> crossover p1 p2 -- >>= mutate


cmpSol :: Solution -> Solution -> Ordering
cmpSol s1 s2 = compare (fitness s1) (fitness s2)


hashSol :: Solution -> Int
hashSol = hash . concat . map name