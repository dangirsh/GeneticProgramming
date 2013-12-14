{-# LANGUAGE MultiParamTypeClasses #-}

module Solution.TreeSolution where


import Solution
import Common
import Tree
import Term.TreeTerm
import Operator
import Value
import Term
import Control.Monad (replicateM)
import Data.Hashable (hash)


type TreeSolution = Tree TreeTerm


instance Solution Tree TreeTerm where

    randomSol = randomTree terminals nonTerminals airity depth
        where
            avBranchNum = intAverage $ map airity (nonTerminals :: [TreeTerm])
            depth = round $ logBase avBranchNum (fromIntegral m)


    evalSol (Node (Terminal (Const x)) _) = return $ Just x
    evalSol (Node (Terminal (Input n)) _) = do
        i <- (!!n)
        return $ Just i
    evalSol (Node (NonTerminal (Op f a _)) children)
        | a == length children = do
            outputs <- sequence (map evalSol children)
            return $ sequence outputs >>= f
        | otherwise = error "Invalid tree."


    crossover sol1 sol2 = do
        node1 <- randomNode sol1
        case filter (compatible node1) . toNodeList $ sol2 of
            [] -> return sol1
            xs -> do
                    node2 <- randomElem xs
                    return $ swapSubtree node1 node2 sol1
        where
            compatible (Node x1 xs1) (Node x2 xs2) =
                case (x1, x2) of
                    ((Terminal _), (Terminal _)) -> True
                    ((NonTerminal (Op _ a1 _)), (NonTerminal (Op _ a2 _))) -> a1 == a2
                    _ -> False


    mutate = return