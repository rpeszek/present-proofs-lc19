module Motivation.DepTyped 

import Data.Vect
import Data.Fin

%default total

-- get still allows for programming errors
-- Note: Idris knows get is total!
get : Fin n -> Vect n a -> a
get k (x :: xs) = x -- wrong

-- show how limited solution space is by having Idris interactively implement the solution
-- reverse of append which is also very precise
vsplitAt : (n : Nat) -> Vect (n + m) a -> (Vect n a, Vect m a)










