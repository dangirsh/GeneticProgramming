{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Solution where

import GP
--import Common (inputs, trueSolution, InputType, ValueType)
import Term
import Prelude hiding (foldr)
import Data.Foldable (Foldable, foldr)
import Foreign.Marshal.Utils (fromBool)
import Control.Applicative ((<$>), (<*>))


type Fitness = Double


-- TODO: add type family to avoid repetitive r (t a)

class (Eq a, Eq (r (t a)), Foldable r, Term t, Show (r (t a))) => Solution r t a | r -> t where

    randomSol :: (GP a) (r (t a))

    evalSol :: r (t a) -> [a] -> (GP a) a

    mutate :: r (t a) -> GP a (r (t a))

    crossover :: r (t a) -> r (t a) -> GP a (r (t a))

    fitness :: r (t a) -> (GP a) Fitness
    fitness sol = do
        inps <- getReprParam inputs
        ts <- getReprParam trueSolution
        sum <$> map fromBool <$> mapM (isMatch ts) inps
        where
            isMatch ts i = (ts i ==) <$> evalSol sol i

    mate :: (r (t a), r (t a)) -> (GP a) (r (t a))
    mate (p1, p2) = do -- crossover p1 p2
         c <- crossover p1 p2
         m <- getParam solutionSize
         if sizeSol c > (3 * m) then randomSol else return c

    sizeSol :: r (t a) -> Int
    sizeSol = foldr (\_ c -> c + 1) 0

    cmpSol :: r (t a) -> r (t a) -> (GP a) Ordering
    cmpSol s1 s2 = do
        cmpFit <- compare <$> (fitness s1) <*> (fitness s2)
        case cmpFit of
            EQ -> return $ compare (sizeSol s1) (sizeSol s2)
            x  -> return x

    -- Implement for better diversity metrics
    -- distance :: r (t a) -> r (t a)