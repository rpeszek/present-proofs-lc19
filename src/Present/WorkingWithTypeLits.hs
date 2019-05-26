{-# LANGUAGE
    TypeOperators
  , DataKinds
  , TypeFamilies
  , PolyKinds
  , StandaloneDeriving
  , GADTs
  -- , ScopedTypeVariables
#-}

-- | Shows why Peano defined Nat theorems (ProofsNatAlg)
-- do not translate directly to TypeLits.
-- The reason is that Peano 'Nat'
-- (as defined in 'Data.Nat' in this project) is defined recursively
-- and so is '+' providing definitionally true propositions
-- we can hook into.  This is not true in TypeLits, we have nothing
-- to base proofs on.
module Present.WorkingWithTypeLits where

import           GHC.TypeLits
import           Data.Type.Equality ((:~:)(Refl), sym)
import           Present.ProofsNatAlg (believeMeEq)

-- allows me to treat TypLits Nat as Peano
data NatPeano (n :: Nat) where
      PeanoZ :: NatPeano 0
      PeanoS :: KnownNat n => NatPeano n -> NatPeano (1 + n)
deriving instance Show (NatPeano n)

-- | Automatic in ProofsNatAlg
-- this comes from free if '+' is defined by pattern matching on first variable
--   @s n + m = s (n + m)@
-- for TypeLits I need to add it
-- note Developer needs to care about implementation details
-- note also no (KnownNat left, KnownNat right) constraint
-- to avoid constraint issues with privately defined type class.
definitional ::
                NatPeano left -> NatPeano right ->
                (1 + left) + right :~: 1 + (left + right)
definitional n m = believeMeEq


{- |
 Automatic in ProofsNatAlg
  (reversed succesor equation in def of '+')
-}
assoc1 :: (KnownNat left, KnownNat right) => NatPeano left -> NatPeano right ->
        1 + (left + right) :~: ((1 + left) + right) -- RHS same as (1 + left + right)
assoc1 n m = sym $ definitional n m

-- | GHC knows that automatically:
ind ::
       left :~: right -> (1 + left) :~: (1 + right)
ind proof =  case proof of Refl -> Refl


------------------------------------------
-- ProofsNatAlg reproduced for TypeLits
------------------------------------------

-- same as in ProofsNatAlg
lemma1 :: NatPeano mx -> 0 + mx :~: mx + 0
lemma1 PeanoZ = Refl
lemma1 (PeanoS k) = case lemma1 k of
                        Refl -> Refl

-- | needs evidence from 'definitional' and 'assoc1'
lemma2 :: (KnownNat left, KnownNat right) => NatPeano left -> NatPeano right ->
          (left + (1 + right)) :~: (1 + (left + right))
lemma2 PeanoZ right        = Refl
{-
  needed:
  ((1 + left) + (1 + right)) :~: (1 + ((1 + left) + right)
  proof:
  ((1 + left) + (1 + right))
      :def       = 1 + (left + (1 + right))
      :indlifted = 1 + (1 + (left + right))
      :assoc1    = (1 + ((1 + left) + right)
-}
lemma2 (PeanoS left) right =
  case definitional left (PeanoS right) of -- ^ had to be added
      Refl -> case (lemma2 left right) of -- ^ ind is implicit
        Refl -> case assoc1 left right of
          Refl -> Refl


-- | needs evidence from 'definitional'
plusCommutative :: (KnownNat left, KnownNat right) => NatPeano left -> NatPeano right -> ((left + right) :~: (right + left))
plusCommutative left right = case left of
            PeanoZ  ->  lemma1 right
            (PeanoS k) -> case definitional k right of -- had to be added
               Refl -> case plusCommutative k right of
                  Refl -> sym (lemma2 right k)
