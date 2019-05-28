{-# LANGUAGE
    TypeOperators
  , DataKinds
  , TypeFamilies
  , PolyKinds
  , TypeApplications
#-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}

-- | About eliminating run time cost of proofs
-- example of (not very maintainable) recursive proofs

module Present.ProofsNatAlg (
  plusCommutative
  , believeMeEq
) where

import           Data.Nat -- defined in this package
import           Data.Type.Equality ((:~:)(Refl), sym)
import           Unsafe.Coerce
import qualified Test.QuickCheck as Property
import           Data.Singletons
import           Debug.Trace


--  recursive lemmas

lemma1 :: SNat mx ->
          'Z + mx :~: mx + 'Z
lemma1 SZ     = Refl
-- | recursive
lemma1 (SS k) = case lemma1 k of
                        Refl -> Refl

lemma2 :: SNat left -> SNat right ->
          (left + 'S right) :~: 'S (left + right)
lemma2 SZ right        = Refl
-- | recursive
lemma2 (SS left) right = case lemma2 left right of
                        Refl -> Refl


-- | recursive proof
-- rewrite rule for this proof eliminates run-time cost
plusCommutative :: SNat left -> SNat right ->
                   ((left + right) :~: (right + left))
plusCommutative left right = case left of
            SZ  ->  traceShow "1" $ lemma1 right
            -- | recursive
            (SS k)  ->  traceShow "2" $ case plusCommutative k right of Refl -> sym (lemma2 right k)

------------------------------------------
-- SOLUTION 1 Rewrite Rules
------------------------------------------

-- Following rule is dangerous (remove r and l and it still compiles, but will crash the app!):
--
-- {-# NOINLINE plusCommutative #-}
-- {-# RULES "proof" forall l r. plusCommutative l r = unsafeCoerce Refl #-}
--
-- ref https://typesandkinds.wordpress.com/2016/07/24/dependent-types-in-haskell-progress-report/
--
-- Better approach (uncomment to work):

-- {-# NOINLINE plusCommutative #-}
-- {-# RULES "proof" forall l r. plusCommutative l r = believeMeEq #-}

believeMeEq :: a :~: b
believeMeEq = unsafeCoerce Refl

-- NOTE on ghci evaluation, evaluate 'test1', not just 'plusCommutative s4 s1'
-- to see rules take effect
test1 = plusCommutative (sFromTL @ 4) (sFromTL @ 1)

------------------------------------------
-- SOLUTION 2 proof combinator
------------------------------------------
proveEqFast :: a :~: b ->  a :~: b
proveEqFast _ = believeMeEq

-- comment RULES above
-- before running
test2 = proveEqFast (plusCommutative s4 s1)

------------------------------------------
-- No termination worries
------------------------------------------
termProp :: (Integer, Integer) -> Bool
termProp (i, j) = (integerToNat i) `withSomeSing` (\ni ->
                  (integerToNat j) `withSomeSing` (\nj ->
                    case plusCommutative ni nj of Refl -> True
                    ))

testTerm = Property.quickCheck termProp

-- Next: (back to slides)
