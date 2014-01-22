{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Term.ConcatTerm where

import GP
import Term
import Operator (Op (Op))
import Value


type StackOp a = Op [a] [a]


data ConcatTerm a = MkCT (Op [a] [a]) deriving (Eq)


instance Show (ConcatTerm a) where
    show (MkCT (Op _ _ n)) = n


instance Term ConcatTerm where

    compatible (MkCT (Op _ a1 _)) (MkCT (Op _ a2 _)) = a1 == a2

    terminals = return []

    nonTerminals = do
        consts <- getReprParam values
        ops <- getReprParam operators
        return $ map convertVal consts ++ map convertOp ops

    airity (MkCT (Op _ a _)) = a

    convertVal (Const x) = MkCT (Op (x:) 0 "const")
    convertVal _ = undefined

    convertOp (Op f a n) = MkCT (Op g a n)
        where
            g stack = f (take a stack) : drop a stack
