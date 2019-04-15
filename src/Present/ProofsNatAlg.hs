{-# LANGUAGE 
    TypeOperators
  , DataKinds
  , GADTs
  , TypeFamilies
  , KindSignatures
  , PolyKinds
#-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}

module Present.ProofsNatAlg (
  plusCommutative
) where

import Data.Nat
import Data.Type.Equality ((:~:)(Refl), sym)
import Unsafe.Coerce


-- | Using rewrite rule eliminates run time cost of proofs
-- Error prone and not ideal.
plusCommutative :: SNat left -> SNat right -> ((left + right) :~: (right + left))
{-# INLINE CONLIKE [1] plusCommutative #-}
-- {-# NOINLINE plusCommutative #-}
plusCommutative left right = case left of 
            SZ  ->  lemma1 right
            -- | recursive
            (SS k)  ->  case plusCommutative k right of Refl -> sym (lemma2 right k)

{-# RULES "proof" forall l r. plusCommutative l r = unsafeCoerce Refl #-} 



lemma1 :: SNat mx -> 'Z + mx :~: mx + 'Z
lemma1 SZ     = Refl
-- | recursive
lemma1 (SS k) = case lemma1 k of  
                        Refl -> Refl

lemma2 :: SNat left -> SNat right -> (left + 'S right) :~: 'S (left + right)
lemma2 SZ right        = Refl
-- | recursive
lemma2 (SS left) right = case lemma2 left right of 
                        Refl -> Refl


