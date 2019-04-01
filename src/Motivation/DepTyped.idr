module Motivation.DepTyped 

import Data.Vect
import Data.Fin

%default total

-- show how limited solution space is by having idris derive the solution

get : Vect n a -> Fin n -> a 


