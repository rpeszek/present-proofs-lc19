{-# LANGUAGE 
      KindSignatures
      , DataKinds
      , GADTs
      , TypeOperators
      , TypeInType
      , AllowAmbiguousTypes
#-}

module Motivation.DepTyped where

import           Data.Vect
import           Data.Nat
import           Present.MaybeB
import           Data.Type.Equality ((:~:))
import qualified Data.Type.Equality as TE



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
-- Answ: ghc knows:
th :: (Less m n) :~: (Less (S m) (S n))
th = TE.Refl
