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

------------
-- Examples from Type Equality slide
------------

module Present.ProofExtras where

import           Data.Type.Equality
import           GHC.TypeLits
import           Data.Singletons.Prelude.Bool
-- import           Data.Singletons.TypeLits


test1 = Refl :: 5 :~: 5  -- GOOD
-- test10  = Refl :: 4 :~: 5 -- Error

test2 = Refl :: 2 + 3 :~: 3 + 2 -- GOOD
-- test20 :: SNat n1 -> SNat n2 -> n1 + n2 :~: n2 + n1
-- test20 _ _ = Refl  -- Error

-- same for Bool

testb1 = Refl :: True :~: True 
-- testb10 = Refl :: True :~: False -- Error

testb2 = Refl :: (False :|| True) :~: (True :|| False)
-- testb20 :: SBool b1 -> SBool b2 -> (b1 :|| b2) :~: (b2 :|| b1)
-- testb20 = Refl -- Error