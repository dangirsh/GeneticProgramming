{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Solution where

import GP
import Common (inputs, trueSolution, InputType, ValueType)
import Term
import Prelude hiding (foldr)
import Data.Foldable (Foldable, foldr)
import Foreign.Marshal.Utils (fromBool)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (asks)

type Fitness = Double


class (Eq (r t), Foldable r, Term t, Show (r t)) => Solution r t | r -> t where

    randomSol :: GP (r t)

    evalSol :: r t -> InputType -> Maybe ValueType

    mutate :: r t -> IO (r t)

    crossover :: r t -> r t -> IO (r t)

    fitness :: r t -> Fitness
    fitness sol = sum $ map (fromBool . isMatch) inputs
        where
            isMatch input = (evalSol sol input) == (Just $ trueSolution input)

    mate :: (r t, r t) -> GP (r t)
    mate (p1, p2) = do
         c <- lift $ crossover p1 p2
         m <- asks solutionSize
         if sizeSol c > (3 * m) then randomSol else return c

    sizeSol :: r t -> Int
    sizeSol = foldr (\_ c -> c + 1) 0

    cmpSol :: r t -> r t -> Ordering
    cmpSol s1 s2 =
        case compare (fitness s1) (fitness s2) of
            EQ -> compare (sizeSol s2) (sizeSol s1)
            x  -> x