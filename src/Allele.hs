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
    andTerm,
    orTerm
) where


import Common (randElem)
import System.Random (randomRIO)


type Stack = [Bool]


data Term = Term { input :: Int,
                   output :: Int,
                   stackFunc :: Stack -> Maybe Stack,
                   name :: String
                 }


instance Show Term where
    show Term { input = _, output = _, stackFunc = _, name = name} = name


type Allele = Term


trueFunc :: Stack -> Maybe Stack
trueFunc xs = Just (True:xs)


falseFunc :: Stack -> Maybe Stack
falseFunc xs = Just (False:xs)


binFunc :: (Bool -> Bool -> Bool) -> Stack -> Maybe Stack
binFunc _ [] = Nothing
binFunc _ (x:[]) = Nothing
binFunc f (x1:x2:xs) = Just (f x1 x2:xs)


andFunc :: Stack -> Maybe Stack
andFunc = binFunc (&&)


orFunc :: Stack -> Maybe Stack
orFunc = binFunc (||)


xorFunc :: Stack -> Maybe Stack
xorFunc = binFunc xor
    where
        xor True False = True
        xor False True = True
        xor _ _ = False


norFunc :: Stack -> Maybe Stack
norFunc = binFunc (\a b -> not (a || b))


nandFunc :: Stack -> Maybe Stack
nandFunc = binFunc (\a b -> not (a && b))


notFunc :: Stack -> Maybe Stack
notFunc [] = Nothing
notFunc (x:xs) = Just (not x:xs)


trueTerm = Term { input = 0, output = 1, stackFunc = trueFunc, name = "true"}
falseTerm = Term { input = 0, output = 1, stackFunc = falseFunc, name = "false"}
andTerm = Term { input = 2, output = 1, stackFunc = andFunc, name = "and"}
nandTerm = Term { input = 2, output = 1, stackFunc = nandFunc, name = "nand"}
orTerm = Term { input = 2, output = 1, stackFunc = orFunc, name = "or"}
norTerm = Term { input = 2, output = 1, stackFunc = norFunc, name = "nor"}
xorTerm = Term { input = 2, output = 1, stackFunc = xorFunc, name = "xor"}
notTerm = Term { input = 1, output = 1, stackFunc = notFunc, name = "not"}


randAllele :: IO Allele
randAllele = randElem [andTerm, orTerm, nandTerm, norTerm, notTerm, xorTerm]
--randAllele = randElem [andTerm, orTerm, nandTerm, norTerm, notTerm]
--randAllele = randElem [notTerm, xorTerm]


mutateAllele :: Allele -> IO Allele
mutateAllele a = randAllele