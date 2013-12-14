module Term where

class Term t where
    compatible :: t -> t -> Bool
    terminals :: [t]
    nonTerminals :: [t]
    airity :: t -> Int