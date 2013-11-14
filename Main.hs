module Main where


import Common (g, n, average)
import Solution (Solution, fitness)
import Population (Population, nextPop, sortPop, initPop, diversity)
import System.Random (getStdGen, setStdGen)


topSol :: Population -> Solution
topSol = head . sortPop


printIfNth :: Show b => (a -> b) -> Int -> String -> Int -> a -> IO ()
printIfNth f n s i x = if (i `mod` n) == 0 then putStr s >> putStrLn (show $ f x) else return ()


printDiversity :: Int -> Population -> IO ()
printDiversity = printIfNth diversity 10 "Diversity: "


printFitness :: Int -> Population -> IO ()
printFitness = printIfNth (fitness . topSol) 10 "Fitness: "


printLength :: Int -> Population -> IO ()
printLength = printIfNth (average . map length) 10 "Length: "


evolve :: Population -> IO Population
evolve = f g
    where
        f 0 pop = return pop
        f i pop = do
            printDiversity i pop
            printFitness i pop
            printLength i pop
            nextPop pop >>= f (i - 1)


results :: IO Solution
results = initPop >>= evolve >>= return . topSol


main :: IO ()
main = do
    gen <- getStdGen
    setStdGen gen
    r <- results
    print r
    print $ fitness r