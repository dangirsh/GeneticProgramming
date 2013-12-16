{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Solution.ConcatSolution where


import GP
import Common (randomElem, randomInt, numInputs)
import Solution
import Term
import Term.ConcatTerm
import Operator
import Control.Monad (replicateM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (asks)
import Data.List (findIndices)
--import Data.Vector (Vector) -- TODO (swap for list)


type ConcatSolution = [ConcatTerm]


instance Solution [] ConcatTerm where


    --randomSol = replicateM m $ randomElem nonTerminals

    randomSol = do
        m <- asks solutionSize
        lift $ replicateM m $ randomElem nonTerminals

    evalSol [] (top:_) = Just $ top
    evalSol ((Op f _ _):xs) stack = f stack >>= evalSol xs
    evalSol _ _ = error "Stack underflow."


    crossover = twoPtCrossover
    --crossover = onePtCrossover

    mutate = return


splits :: ConcatSolution -> ConcatSolution -> Int -> [Int]
splits sol1 sol2 i = findIndices (<=((stackSizes sol1) !! i)) $ stackSizes sol2
    where
        stackSizes = scanl count numInputs
        count c (Op _ a _) = c - a + 1 -- all output airities = 1


slice :: (Int, Int) -> [a] -> [a]
slice (a, b) = take (b - a) . drop a


onePtCrossover :: ConcatSolution -> ConcatSolution -> IO ConcatSolution
onePtCrossover sol1 sol2 =
    case sol1 of
        [] -> return sol2
        _  -> do
                i1 <- randomInt $ length sol1
                case splits sol1 sol2 i1 of
                    [] -> return sol1
                    xs -> do
                            i2 <- randomElem xs
                            return $ slice (0, i1) sol1 ++ slice (i2 + 1, length sol2) sol2


twoPtCrossover :: ConcatSolution -> ConcatSolution -> IO ConcatSolution
twoPtCrossover sol1 sol2 = do
    i1A <- randomInt $ length sol1
    i1B <- randomInt $ length sol1
    let [splits2A, splits2B] = map (splits sol1 sol2) [i1A, i1B]
    case [(x, y) | x <- splits2A, y <- splits2B, x < y] of
        [] -> return sol1
        xs -> do
                (i2A, i2B) <- randomElem xs
                return $ slice (0, i1A) sol1 ++
                         slice (i2A, i2B + 1) sol2 ++
                         slice (i1B + 1, length sol1) sol1