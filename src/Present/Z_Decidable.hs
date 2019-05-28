{-# LANGUAGE
    TypeOperators
  , DataKinds
  , GADTs
  , TypeFamilies
  , KindSignatures
  , PolyKinds
  , ScopedTypeVariables
  , AllowAmbiguousTypes
  , TypeApplications
  , EmptyCase
  , RankNTypes
#-}

--
-- Program like David Hilbert would!
--
module Present.Z_Decidable where

import           Data.Void
import           Data.Nat hiding (Less) -- defined in this package
import           Data.Vect


-- | Gentlemen's agreement: terminating proofs only please.
-- singletons package has a similar @Decision@ type
data Dec prop = Yes prop | No (prop -> Void)

instance Show (Dec a) where
  show (Yes _) = "Yes"
  show (No _) = "No"

-- notice information loss:
trivialize :: Dec prop -> Bool
trivialize (Yes _) = True
trivialize (No _) = False

-------------------------
-- n < m example
-------------------------

-- lack of implementation type safety
wrongLess :: SNat n -> SNat m -> Bool
wrongLess n m = True


data Less (n :: Nat) (m :: Nat) where
  LessZ :: SNat m -> Less Z (S m)
  LessS :: Less n m -> Less (S n) (S m)

-- programming with proofs and contradictions,
-- construct proofs using known proofs
-- contradictions using know contradictions
decideLess :: SNat n -> SNat m -> Dec (Less n m)
decideLess SZ SZ = No $ (\x -> case x of { })
decideLess (SS n) SZ = No $ (\x -> case x of { })
decideLess SZ (SS m) = Yes $ LessZ m
decideLess (SS _n) (SS _m) = case decideLess _n _m of
      Yes prf -> Yes $ LessS prf
      No contra -> No $ (\x {- :: Less (S _n) (S _m) -} -> case x of
                             LessS y -> contra y 
                             -- ^ we need to disprove Less successor expression
                             -- this has to be 'LessS' of predicessor 'y'
                             )

-- decideLess is a better version of (<)
-- check out the information loss:
correctLess :: SNat n -> SNat m -> Bool
correctLess n m = trivialize (decideLess n m)

infix 3 <!
(<!) = correctLess

-- ghci
tst = sFromTL @ 2 <! sFromTL @ 3




----------------------------------------
-- examples using Vector 
----------------------------------------

-- | 'SNat i -> Vect n a -> a' is not precise enough to use with Dec
(!!) :: SNat i -> SNat n -> Dec (SNat i -> Vect n a -> a)
(!!) si sn = case decideLess si sn of 
    Yes prf -> Yes $ lemma1 prf
    No cotr -> undefined -- this will not work!
  where
    lemma1 :: Less i n -> SNat i -> Vect n a -> a    
    lemma1 = undefined


-- | But this is doable!
-- polymorphic pick element from 'Vect n a' is possible iff n > 0
data PickElem n = PickElem (forall a . Vect n a -> a)

pickElem :: forall n a . SNat n ->  Dec (PickElem n)
pickElem sn = case decideLess SZ sn of 
    Yes prf -> Yes $ lemma1 prf
    No cont -> No $ (\vF -> cont . lemma2 sn $ vF ) 
  where
    lemma1 :: Less Z n -> PickElem b   
    lemma1 (LessZ _) = PickElem (\(VCons a vs) -> a)  
                       -- ^ fwarn-incomplete-patterns on
                       -- GHC knows that Vect is not VNil !!

    lemma2 :: SNat n -> PickElem n -> Less Z n
    lemma2 sn (PickElem fn) = fn (exampleVect sn)
                       -- ^ construct example Vect and use 
                       -- polymorphic function to extract evidence from it

    exampleVect :: Sing n -> Vect n (Less Z n)
    exampleVect SZ = VNil
    exampleVect k@(SS n) = repeatV k (LessZ n) 



-- Next: (back to slides)
