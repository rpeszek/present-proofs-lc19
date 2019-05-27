{-# LANGUAGE 
      KindSignatures
      , DataKinds
      , GADTs
      , TypeOperators
      , TypeInType
      , AllowAmbiguousTypes
#-}

module Wrapup.DepTyped where

import           Data.Vect  -- defined in this package
import           Data.Nat   -- defined in this package
import           Present.MaybeB
import           Data.Type.Equality ((:~:))
import qualified Data.Type.Equality as TE

-------
-- example from Motivation (call-side safety) slide
-------

infixl 3 !!!

(!!!) :: Vect n a -> SNat m -> MaybeB (m < n) a
VNil !!! SZ = NothingB
VNil !!! (SS k) = NothingB
(VCons x xs) !!! SZ = JustB x
(VCons x xs) !!! (SS k) = xs !!! k

-- check types with ghci
test = 1 -: 2 -: 3 -: VNil !!! s1
test2 = 1 -: 2 -: 3 -: VNil !!! s4

-- Question: How come (!!!) compiles?
-- (_it does not in Idris_)
-- Answ: ghc knows:
th :: (Less m n) :~: (Less (S m) (S n))
th = TE.Refl
