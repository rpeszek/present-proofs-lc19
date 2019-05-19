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
      , FlexibleContexts
      , AllowAmbiguousTypes
      , TypeApplications
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

type SToTL n = Sing (ToTL n)

sToTL :: forall (n :: Nat). SingI (ToTL n) => Sing (ToTL n) 
sToTL = sing

-- easy TL Sing values
tl0 = sToTL @ Z
tl1 = sToTL @ (S Z)

type family FromTL (n :: TL.Nat) :: Nat where
    FromTL 0 = Z
    FromTL n = S (FromTL (n TL.- 1))

type SFromTL n = Sing (FromTL n)

sFromTL :: forall (n :: TL.Nat). SingI (FromTL n) => Sing (FromTL n) 
sFromTL = sing

-- easy SNat values
s0 = sFromTL @ 0 -- SZ
s1 = sFromTL @ 1 -- ('SS 'SZ)
s2 = sFromTL @ 2 
s3 = sFromTL @ 3
s4 = sFromTL @ 4

type family (m :: Nat) + (n :: Nat) :: Nat where
   left + right = Plus left right 

type family (m :: Nat) < (n :: Nat) :: Bool where
   left < right = Less left right 
