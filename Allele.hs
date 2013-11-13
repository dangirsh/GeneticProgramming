module Allele (
    Allele,
    Stack,
    Term(Term),
    mutateAllele,
    randAllele,
    stackFunc,
    input,
    output,
    name,
    notTerm,
    andTerm
) where


import Common (randElem)
import System.Random (randomRIO)


type Stack = [Bool]


data Term = Term { input :: Int,
                   output :: Int,
                   stackFunc :: (Stack -> Stack),
                   name :: String
                 }


instance Show Term where
    show Term { input = _, output = _, stackFunc = _, name = name} = name


type Allele = Term


trueFunc = (True:)


falseFunc = (False:)


andFunc [] = [True] --error "bleh"
andFunc (x:[]) = [True] -- error "bleh2"
andFunc (x1:x2:xs) = (x1 && x2):xs


notFunc [] = [True] --error "wheeh"
notFunc (x:xs) = (not x):xs


trueTerm = Term { input = 0, output = 1, stackFunc = trueFunc, name = "true"}
falseTerm = Term { input = 0, output = 1, stackFunc = falseFunc, name = "false"}
andTerm = Term { input = 2, output = 1, stackFunc = andFunc, name = "and"}
notTerm = Term { input = 1, output = 1, stackFunc = notFunc, name = "not"}


randAllele :: IO Allele
randAllele = randElem [trueTerm, falseTerm, andTerm, notTerm]


-- mutationSize = 0.1 :: Float


-- RS
--mutateAllele :: Allele -> IO Allele
--mutateAllele a = do
--    p <- random
--    x <- randomRIO (-1, 1)
--    return $ if p < mutation_p then a + x else a

mutateAllele :: Allele -> IO Allele
mutateAllele a = return a