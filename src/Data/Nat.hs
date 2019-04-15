{-# LANGUAGE TemplateHaskell
      , KindSignatures
      , DataKinds
      , GADTs
      , PolyKinds
      , TypeOperators 
      , TypeFamilies
      , StandaloneDeriving
      , UndecidableInstances 
      , ScopedTypeVariables
      , TypeSynonymInstances
      , Rank2Types
#-}

module Data.Nat where
import Data.Singletons.TH
import qualified GHC.TypeLits as TL

$(singletons [d|
  data Nat = Z | S Nat
    deriving (Show, Eq)

  less :: Nat -> Nat -> Bool
  less Z Z = False 
  less Z (S _) = True 
  less (S n) Z = False
  less (S n) (S k) = less n k

  plus :: Nat -> Nat -> Nat
  plus Z m = m
  plus (S n) m = S (plus n m)
  infixl 6 `plus`

  multi :: Nat -> Nat -> Nat
  multi Z m = Z
  multi (S n) m = plus (multi n m) m
  infixl 7 `multi`

  |])


deriving instance Show (SNat n) 

natToInteger :: Nat -> Integer
natToInteger Z = 0
natToInteger (S un) = 1 + (natToInteger un)

someNatToInteger :: SomeSing Nat -> Integer
someNatToInteger (SomeSing n) = natToInteger (fromSing n)

integerToNat :: Integer -> Nat
integerToNat n 
        | n < 0 = integerToNat (negate n) -- kinda quick and dirty to make it total
        | n == 0 = Z
        | otherwise = S (integerToNat (n - 1))

withSomeNat :: Nat -> (forall n . SNat n -> r) -> r
withSomeNat = withSomeSing

-- need Some SNat

-- test method
-- withNat :: Nat -> (forall n . SNat n -> r) -> r
-- withNat Z f = f SZ
-- withNat (S n) f = undefined 

-- Singling of literal patterns not yet supported
type family ToTL (n :: Nat) :: TL.Nat where
    ToTL Z = 0
    ToTL (S n) = 1 TL.+ (ToTL n)

type family FromTL (n :: TL.Nat) :: Nat where
    FromTL 0 = Z
    FromTL n = S (FromTL (n TL.- 1))

type family (m :: Nat) + (n :: Nat) :: Nat where
   left + right = Plus left right 

type family (m :: Nat) < (n :: Nat) :: Bool where
   left < right = Less left right 


s0 :: SNat (FromTL 0) -- 'Z
s0 = SZ
s1 :: SNat (FromTL 1) -- ('S 'Z)
s1 = SS s0
s2 :: SNat (FromTL 2) -- ('S ('S 'Z))
s2 = SS s1
s3 :: SNat (FromTL 3) -- ('S ('S ('S 'Z)))
s3 = SS s2
s4 :: SNat (FromTL 4) 
s4 = SS s3