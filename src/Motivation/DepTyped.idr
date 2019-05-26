module Motivation.DepTyped

import Data.Vect
import Data.Fin

-- NOTE totality check, gentlemen's agreement when defining proofs in Haskell
%default total

infixl 3 !!

-- get still allows for programming errors
-- Note: Idris knows get is total!
(!!) : Fin n -> Vect n a -> a
(!!) k (x :: xs) = x -- wrong



-- implement using Idris interactive features
-- note increase precision of this type over the above
-- reverse of append which is also very precise
vsplitAt : (n : Nat) -> Vect (n + m) a -> (Vect n a, Vect m a)
