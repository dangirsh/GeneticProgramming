{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Solution.TreeSolution where


import Solution
import Common
import Tree
import Term
import Term.Boolean (boolTerms)
import Control.Monad (replicateM)
import Foreign.Marshal.Utils (fromBool)
import Data.Hashable (hash)
import Prelude hiding (foldr)
import Data.Foldable (Foldable, foldMap, foldr)


type TreeSolution = Tree TreeTerm


instance Solution TreeSolution where

    randomSol = randomTree terminals nonTerminals branchNum depth
        where
            terminals = [t | t@(Terminal _) <- boolTerms]
            nonTerminals = [t | t@(NonTerminal _) <- boolTerms]
            branchNum (Terminal _) = 0
            branchNum (NonTerminal (Op _ airity _)) = airity
            avBranchNum = intAverage $ map branchNum nonTerminals
            depth = round $ logBase avBranchNum (fromIntegral m)


    fitness sol = sum $ map (fromBool . isMatch) inputs
        where
            isMatch input = (evalSol input sol) == (Just $ trueSolution input)


    --mate _ = randomSol
    mate (p1, p2) = do
        r <- random
        if r < crossover_p then --FIXME
            crossover p1 p2 >>= mutate
        else
            randomSol


    sizeSol = foldr (\_ c -> c + 1) 0


    cmpSol s1 s2 =
        case compare (fitness s1) (fitness s2) of
            EQ -> compare (sizeSol s2) (sizeSol s1)
            x  -> x


evalSol :: InputType -> TreeSolution -> Maybe ValueType
evalSol _     (Node (Terminal (Const x)) _) = Just x
evalSol input (Node (Terminal (Input n)) _) = Just $ input !! n
evalSol input (Node (NonTerminal (Op f airity _)) children)
    | airity == length children = mapM (evalSol input) children >>= f
    | otherwise = error "Invalid tree."


crossover :: TreeSolution -> TreeSolution -> IO TreeSolution
crossover sol1 sol2 = do
    node1@(Node x xs) <- randomNode sol1
    case filter (compatibleNode node1) . toNodeList $ sol2 of
        [] -> return sol1
        xs -> do
                node2 <- randomElem xs
                return $ swapSubtree node1 node2 sol1
    where
        compatibleNode (Node x1 xs1) (Node x2 xs2) = compatible x1 x2


mutate :: TreeSolution -> IO TreeSolution
mutate = return