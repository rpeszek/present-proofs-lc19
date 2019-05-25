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
#-}

--
-- Program like David Hilbert would!
--
module Present.ProofsDecidable where

import           Data.Void
import           Data.Nat hiding (Less) -- defined in this package


-- | Gentleman's agreement: terminating proofs only please.
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
decideLess (SS n) (SS m) = case decideLess n m of
      Yes prf -> Yes $ LessS prf
      No contra -> No $ (\x -> case x of
                             LessS y -> contra y
                             )

-- decideLess is a better version of (<)
-- check out the information loss:
correctLess :: SNat n -> SNat m -> Bool
correctLess n m = trivialize (decideLess n m)

infix 3 <!
(<!) = correctLess

-- ghci
tst = sFromTL @ 2 <! sFromTL @ 3


-- Next: (back to slides)
