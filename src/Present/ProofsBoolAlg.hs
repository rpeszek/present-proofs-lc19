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
-- (this is called definitional)
-- how?
th0 ::  forall b . True :~: (True :|| b)
th0 = Refl   

-- | GHC does not know these:
-- 
-- th1' :: True :~: (b :|| True)
-- th1' b = Refl   

-- th1'' :: SBool b -> True :~: (b :|| True)
-- th1'' b = Refl   

{- Think of `SBool b` as 
data SBool (b :: Bool) where
  SFalse :: SBool False
  STrue :: SBool True 

remember:

(||) :: Bool -> Bool -> Bool
True || b = True
False || b = b
-}

-- ^ brittleness alert: 
--   should programmer know that (||) was defined pattern by matching on first variable not second?
--   Drawback of using value level expressions at type level!
--   To prove X requires knowing implementation of X - dah


-------------------------
-- proofs 
-------------------------

th1 :: SBool b -> 
       True :~: (b :|| True)
th1 b = case b of 
   SFalse -> Refl
   STrue -> Refl

orCommutes :: SBool b1 -> SBool b2 -> 
              (b1 :|| b2) :~: (b2 :|| b1)  
orCommutes b1 b2 = case b1 of
   SFalse -> case b2 of 
       SFalse -> Refl
       STrue -> Refl
   STrue -> th1 b2  -- why this works? think about it! Hint is `th0`
                    -- OPINION: this is not best code, 
                    -- split case would be (||) implementation agnostic
  

------------------------------------------
-- We are ready to implement 'secondKnown'
------------------------------------------
secondKnown :: Semigroup m => SBool b -> 
               MaybeB b m -> MaybeB 'True m -> MaybeB 'True m
secondKnown b x1 x2 = case th1 b of 
    Refl -> append x1 x2

secondKnown2 :: Semigroup m => Sing b -> 
                MaybeB b m -> MaybeB 'True m -> MaybeB 'True m
secondKnown2 b = case orCommutes b STrue of 
    Refl -> append 


-- OPINIONATED improvement:
firstKnown2 :: Semigroup m => Sing b -> MaybeB True m -> MaybeB b m -> MaybeB 'True m
firstKnown2 b = case orCommutes STrue b of 
    Refl -> append



-- Side notes:
-- (syntax) polymorphic versions from singletons
secondKnown3 :: Semigroup m => Sing b -> 
                MaybeB b m -> MaybeB 'True m -> MaybeB 'True m
secondKnown3 = secondKnown

secondKnown4 :: forall m b . (Semigroup m, SingI b) => 
                MaybeB b m -> MaybeB 'True m -> MaybeB 'True m
secondKnown4 x1 x2 = case th1 (sing :: Sing b) of 
    Refl -> append x1 x2


-- Next (back to slides)