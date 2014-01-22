module Operator where


data Op a b = Op (a -> b) Int String


type BaseOp a = Op [a] a


instance Eq (Op a b) where
    Op _ airity1 name1 == Op _ airity2 name2 =
        airity1 == airity2 && name1 == name2


instance Show (Op a b) where
    show (Op _ _ name) = show name


unOp :: (Bool -> Bool, String) -> BaseOp Bool
unOp (f, name) = Op op 1 name
    where
        op (x:[]) = f x
        op _ = error "Airity mismatch!"


unOps :: [BaseOp Bool]
unOps = map unOp [(not, "not")]


binOp :: (Bool -> Bool -> Bool, String) -> BaseOp Bool
binOp (f, name) = Op op 2 name
    where
        op (x1:x2:[]) = f x1 x2
        op _ = error "Airity mismatch!"


binOps :: [BaseOp Bool]
binOps = map binOp [((&&), "and"), ((||), "or"), ((/=), "xor")]


ops :: [BaseOp Bool]
ops = unOps ++ binOps