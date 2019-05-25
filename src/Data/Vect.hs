{-# LANGUAGE 
      KindSignatures
      , DataKinds
      , GADTs
      -- , PolyKinds
      , TypeInType 
      , TypeOperators 
      , TypeFamilies
      , StandaloneDeriving
      , ScopedTypeVariables
      -- , TypeSynonymInstances
      , Rank2Types
#-}

module Data.Vect where

-- import           Data.Kind (Type)
-- import           Data.Singletons
-- import qualified GHC.TypeLits as TL
import           Data.Nat

------------------------------------------------------
---- main part                ------------------------
------------------------------------------------------

data Vect (n :: Nat) a where
     VNil :: Vect 'Z a
     VCons :: a -> Vect n a -> Vect ('S n) a

deriving instance Show a => Show (Vect n a)

infixr 5 -:
(-:) :: a -> Vect n a -> Vect ('S n) a
(-:) = VCons

repeatV :: Sing n -> a ->  Vect n a
repeatV SZ _ = VNil
repeatV (SS k) a = a `VCons` (repeatV k a)

-- data instance Sing ( v :: Vect n a) :: Type where
--   SVNil :: Sing VNil
--   SVCons :: Sing a -> Sing v -> Sing (VCons a v)

-- data SomeVect a where
--    SomeVect :: SNat s -> Vect s a -> SomeVect a

-- deriving instance Show a => Show (SomeVect a)

-- {-| CPS style reification -}
-- withSomeVect :: SomeVect a -> (forall n. SNat n -> Vect n a -> r) -> r
-- withSomeVect (SomeVect sn vec) f = f sn vec

-- listToSomeVect :: [a] -> SomeVect a
-- listToSomeVect [] = SomeVect SZ VNil
-- listToSomeVect (x : xs) 
--       = case listToSomeVect xs of SomeVect n rr -> SomeVect (SS n) (x `VCons` rr) 

-- withList :: [a] -> (forall n. SNat n -> Vect n a -> r) -> r
-- withList a = withSomeVect (listToSomeVect a)