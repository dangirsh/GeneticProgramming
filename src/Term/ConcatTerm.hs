{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Term.ConcatTerm where

import Term
import Operator
import Value
import Common


type ConcatTerm = Op InputType InputType


instance Term ConcatTerm where
    compatible (Op _ a1 _) (Op _ a2 _) = a1 == a2
    terminals = []
    nonTerminals = map convertConst consts ++ map convertOp ops
    airity (Op _ a _) = a



convertConst :: TreeValue -> ConcatTerm
convertConst (Const x) = Op (Just . (x:)) 0 "const"
convertConst _ = undefined


convertOp :: TreeOp -> ConcatTerm
convertOp (Op f a n) = Op g a n
    where
        g stack = (f stack) >>= return . (: drop a stack)
