
module Present.AnIntro where 

import qualified Data.Maybe as Mbe  

-- Curry-Howard vs Imperative Gentlemen's Agreements

{- | Conjunction elimination in propositional logic

 P ^ Q
 -----   ( (P ^ Q) -> P )
   P 
-} 

fst :: (a,b) -> a
fst (a,b) = a

{- |  (Imperative Laguage Semantic Rule)
      Disjunction Cunfusa - No such proposition

 P v Q  
 -----  ( (P v Q) -> Q )
   P
-}
unsafeRight :: Either e a -> a
unsafeRight (Right a) = a
unsafeRight (Left _) = error "problem!"

-- | (Java null, Python None, C++ nullptr/NULL, ...) 
-- Copy of actual Data.Maybe.fromJust 
fromJust :: Maybe a -> a
fromJust = Mbe.fromJust

-- can we make these safe?
    
-- Next: (code) Present/MaybeB.hs    