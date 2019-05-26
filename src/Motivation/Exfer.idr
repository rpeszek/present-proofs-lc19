module Motivation.Exfer

import Data.Vect

-- NOTE totality check, gentlemen's agreement when defining proofs in Haskell 
%default total


-- implement using Idris interactive features
-- note increase precision of this type over List

mapV : (a -> b) -> Vect m a -> Vect m b

appendV : Vect n a -> Vect m a -> Vect (n + m) a


---
-- possible rearrangements

rL : List a -> List a

rV : Vect n a -> Vect n a


-- Next: (code) Present/AnIntro.hs
