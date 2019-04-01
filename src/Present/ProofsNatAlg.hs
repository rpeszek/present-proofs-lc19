{-# LANGUAGE 
    TypeOperators
  , DataKinds
  , GADTs
  , TypeFamilies
  , KindSignatures
  , PolyKinds
#-}

module Present.ProofsNatAlg where

import Data.Nat
import Data.Type.Equality ((:~:)(Refl), sym)

lemma1 :: SNat mx -> 'Z + mx :~: mx + 'Z
lemma1 SZ     = Refl
lemma1 (SS k) = case lemma1 k of  
                        Refl -> Refl

lemma2 :: SNat left -> SNat right -> (left + 'S right) :~: 'S (left + right)
lemma2 SZ right        = Refl
lemma2 (SS left) right = case lemma2 left right of 
                        Refl -> Refl

plusCommutative :: SNat left -> SNat right -> ((left + right) :~: (right + left))
plusCommutative SZ right = lemma1 right
plusCommutative (SS k) right = case plusCommutative k right of Refl -> sym (lemma2 right k)