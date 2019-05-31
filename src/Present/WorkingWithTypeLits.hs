{-# LANGUAGE
    TypeOperators
  , DataKinds
  , TypeFamilies
  , PolyKinds
  , StandaloneDeriving
  , GADTs
  -- , ScopedTypeVariables
#-}

-- | Shows why previously defined Peano Nat theorems (ProofsNatAlg)
-- do not translate directly to TypeLits.
-- The reason is that recursive implementation of '+' spilled into type level
-- providing `definitional1` equalities that ProofsNatAlg.hs used implicitly.
-- This is not true for TypeLits, we have nothing
-- to base the proofs on.

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
-- this associativity property comes
-- for free if '+' is defined by pattern matching on first variable
--   @s n + m = s (n + m)@
-- for TypeLits I need to add it
-- note Developer needs to care about implementation details
-- note also no (KnownNat left, KnownNat right) constraint
-- to avoid constraint issues with privately defined type class.
definitional1 ::
                NatPeano left -> NatPeano right ->
                (1 + left) + right :~: 1 + (left + right)
definitional1 n m = believeMeEq


{- |
 Automatic in ProofsNatAlg
  (reversed succesor equation in def of '+')
-}
definitional2 :: (KnownNat left, KnownNat right) => NatPeano left -> NatPeano right ->
        1 + (left + right) :~: ((1 + left) + right) -- RHS same as (1 + left + right)
definitional2 n m = sym $ definitional1 n m

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

-- | needs evidence from 'definitional1' and 'definitional2'
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
      :definitional2    = (1 + ((1 + left) + right)
-}
lemma2 (PeanoS left) right =
  case definitional1 left (PeanoS right) of -- ^ had to be added
      Refl -> case (lemma2 left right) of -- ^ ind is implicit
        Refl -> case definitional2 left right of
          Refl -> Refl


-- | needs evidence from 'definitional1'
plusCommutative :: (KnownNat left, KnownNat right) => NatPeano left -> NatPeano right ->
                   ((left + right) :~: (right + left))
plusCommutative left right = case left of
            PeanoZ  ->  lemma1 right
            (PeanoS k) -> case definitional1 k right of -- had to be added
               Refl -> case plusCommutative k right of
                  Refl -> sym (lemma2 right k)


-- OPINION: Make client code (+) implementation agnostic
-- Peano (Data.Nat) proofs should not have relied on implicit
-- (1 + left) + right :~: 1 + (left + right)
-- 1 + (left + right) :~: ((1 + left) + right)
-- definitional1 equalities. These base propositions should have been stated explicitly
-- and used in client code when relevant ... (linting tool/type checker plugin for this?)

-- Next: (back to slides)
