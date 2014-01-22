module Term.TreeTerm where

import Term
import GP
import Value (Value (Input))
import Operator (BaseOp, Op (Op))
import Control.Applicative ((<$>))

data TreeTerm a = Terminal (Value a)
                | NonTerminal (BaseOp a) deriving (Show, Eq)


instance Term TreeTerm where

    compatible (Terminal _) (Terminal _) = True
    compatible (NonTerminal (Op _ a1 _)) (NonTerminal (Op _ a2 _)) = a1 == a2
    compatible _ _ = False

    terminals = do
        vals <- getReprParam values
        n <- getReprParam numInputs
        return $ map convertVal $ vals ++ map Input [0..(n - 1)]

    nonTerminals = do
        ops <- getReprParam operators
        return $ map convertOp ops

    airity (Terminal _) = 0
    airity (NonTerminal (Op _ a _)) = a

    convertVal = Terminal

    convertOp = NonTerminal
