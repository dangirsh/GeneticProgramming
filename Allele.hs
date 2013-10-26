module Allele (
    Allele,
    mutateAllele
) where


import Common
import System.Random (randomRIO)


type Allele = Int


mutationSize = 0.1 :: Float


mutateAllele :: Allele -> IO Allele
mutateAllele a = do
    p <- random
    x <- randomRIO (-1, 1)
    return $ if p < mutation_p then a + x else a