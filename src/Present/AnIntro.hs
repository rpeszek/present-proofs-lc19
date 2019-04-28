
module Present.AnIntro where 

import qualified Data.Maybe as Mbe  

-- Curry-Howard

{- | Conjunction elimination in propositional logic

 P ^ Q
 -----   ( (P ^ Q) -> P )
   P 
-} 

conjunctionElim :: (a,b) -> a
conjunctionElim = fst

{- | Disjunction cunfusa - No such proposition

 P v Q  
 -----  ( (P v Q) -> P )
   P
-}
unsafeLeft :: Either a b -> a
unsafeLeft (Left a) = a 
unsafeLeft (Right _) = error "that is not right!"

-- | Not a proposition
-- copy of actual Data.Maybe function
fromJust :: Maybe a -> a
fromJust = Mbe.fromJust

-- can we make these check out?
    