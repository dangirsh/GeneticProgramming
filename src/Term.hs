module Term where


import Common


data Value = Const ValueType | Input Int deriving (Show, Eq)


data Op = Op (InputType -> Maybe ValueType) Int String


instance Eq Op where
    Op _ airity1 name1 == Op _ airity2 name2 =
        airity1 == airity2 && name1 == name2


instance Show Op where
    show (Op _ _ name) = show name


data TreeTerm = Terminal Value | NonTerminal Op deriving (Show, Eq)


instance Compatible TreeTerm where
    compatible (Terminal _) (Terminal _) = True
    compatible (NonTerminal (Op _ a1 _)) (NonTerminal (Op _ a2 _)) = a1 == a2
    compatible _ _ = False
