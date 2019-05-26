
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



-- Note: totality issues
-- can prove any nonsense by throwing exception!

{- |  (Imperative Exception System)
      Disjunction Cunfusa - No such proposition

 P v Q
 -----  ( (P v Q) -> Q )
   P
-}
unsafeRight :: Either e a -> a
unsafeRight (Right a) = a
unsafeRight (Left _) = error "problem!"

-- | (Imperative Null Semantic Rule)
fromJust :: Maybe a -> a
fromJust = Mbe.fromJust

-- can we make these safe?

-- Next: (code) Present/MaybeB.hs
