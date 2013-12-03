module Tree where


import Common
--import Data.Functor (Functor, fmap)
import Control.Monad (replicateM)
import Control.Applicative (pure, (<$>), (<*>))
import Prelude hiding (foldr, mapM)
import Data.Foldable (Foldable, foldMap, foldr)
--import Data.Traversable (Traversable, traverse, mapM)
import Data.Monoid ((<>))


data Tree a = Node a [Tree a] deriving Eq


instance (Show a) => Show (Tree a) where
    show = f 0
        where
            f i (Node x xs) = indent i ++ show x ++ "\n" ++ concatMap (f (i + 1)) xs
            indent i = concat (replicate i "  ")


--instance Functor Tree where
--    fmap f (Node x xs) = Node (f x) (fmap (fmap f) xs)


--instance Traversable Tree where
--    traverse f (Node x xs) = Node <$> f x <*> traverse (traverse f) xs


instance Foldable Tree where
    foldMap f (Node x xs) = f x <> foldMap (foldMap f) xs


instance (Compatible a) => Compatible (Tree a) where
    compatible (Node x1 xs1) (Node x2 xs2) =
        (compatible x1 x2) && (length xs1 == length xs2)


randomTree :: [a] -> [a] -> (a -> Int) -> Int -> IO (Tree a)
randomTree terminals nonTerminals branchNum = f
    where
        f i | i <= 1 = do
                terminal <- randomElem terminals
                return $ Node terminal []
            | otherwise = do
                nonTerminal <- randomElem nonTerminals
                children <- replicateM (branchNum nonTerminal) $ f $ i - 1
                return $ Node nonTerminal children


swapSubtree :: (Eq a) => Tree a -> Tree a -> Tree a -> Tree a
swapSubtree node newNode this@(Node x xs) =
    if this == node then
        newNode
    else
        Node x $ map (swapSubtree node newNode) xs


toList :: Tree a -> [a]
toList (Node x xs) =  x : concatMap toList xs


toNodeList :: Tree a -> [Tree a]
toNodeList node@(Node x xs) = node : concatMap toNodeList xs


randomNode :: Tree a -> IO (Tree a)
randomNode root = fst $ foldl f (undefined, 1) (toNodeList root)
    where
        f (c, i) x = (
            random >>= \r -> if r < (1 / i) then return x else c
           ,i + 1)