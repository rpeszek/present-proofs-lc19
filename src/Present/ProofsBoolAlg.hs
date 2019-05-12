{-# LANGUAGE 
    TypeOperators
  , DataKinds
  , GADTs
  , TypeFamilies
  , KindSignatures
  , PolyKinds
  , ScopedTypeVariables
  , AllowAmbiguousTypes
#-}

module Present.ProofsBoolAlg where

import           Data.Type.Equality ((:~:)(Refl))
import           Present.MaybeB hiding (secondKnown)
import           Data.Singletons.Prelude.Bool
import           Data.Singletons
import           Data.Semigroup


-- | use of 'Refl' indicates that GHC knows it
-- how?
th0 ::  forall b . True :~: (True :|| b)
th0 = Refl   

-- | GHC does not know these:
-- th1' :: True :~: (b :|| True)
-- th1' b = Refl   

-- th1'' :: SBool b -> True :~: (b :|| True)
-- th1'' b = Refl   

{- Think of `SBool b` as 
data SBool (b :: Bool) where
  SFalse :: SBool False
  STrue :: SBool True 
-}

th1 :: SBool b -> True :~: (b :|| True)
th1 b = case b of 
   SFalse -> Refl
   STrue -> Refl

orCommutes :: SBool b1 -> SBool b2 -> (b1 :|| b2) :~: (b2 :|| b1)  
orCommutes b1 b2 = case b1 of
   SFalse -> case b2 of 
       SFalse -> Refl
       STrue -> Refl
   STrue -> th1 b2  -- why is this equivalent? think about it! Hint is `th0`
  

------------------------------------------
-- We are ready to implement 'secondKnown'
------------------------------------------

secondKnown :: Semigroup m => SBool b -> MaybeB b m -> MaybeB 'True m -> MaybeB 'True m
secondKnown b x1 x2 = case th1 b of 
    Refl -> append x1 x2

secondKnown2 :: Semigroup m => Sing b -> MaybeB b m -> MaybeB 'True m -> MaybeB 'True m
secondKnown2 b x1 x2 = case orCommutes b STrue of 
    Refl -> append x1 x2





-- (syntax) polymorphic versions from singletons
secondKnown3 :: Semigroup m => Sing b -> MaybeB b m -> MaybeB 'True m -> MaybeB 'True m
secondKnown3 = secondKnown

secondKnown4 :: forall m b . (Semigroup m, SingI b) => MaybeB b m -> MaybeB 'True m -> MaybeB 'True m
secondKnown4 x1 x2 = case th1 (sing :: Sing b) of 
    Refl -> append x1 x2


-- Next (back to slides)