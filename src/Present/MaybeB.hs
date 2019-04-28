{-# LANGUAGE GADTs
  , DataKinds
  , KindSignatures
  , TypeOperators
  , StandaloneDeriving
  , TypeFamilies #-}
  

module Present.MaybeB where

import           Data.Singletons.Prelude.Bool
import           Data.Semigroup
  


data MaybeB (b :: Bool) a where
    NothingB :: MaybeB False a
    JustB    :: a -> MaybeB True a

deriving instance Show a => Show (MaybeB b a)


-- Note -fwarn-incomplete-patterns is ON
fromJustB :: MaybeB True a -> a     
fromJustB (JustB a) = a


-- | Information stored in MaybeB can only grow
append :: Semigroup m => 
          MaybeB b1 m -> MaybeB b2 m -> MaybeB (b1 :|| b2) m
append NothingB x = x
append x@(JustB a) NothingB = x
append (JustB a) (JustB b) = JustB (a <> b)
-- append _ _ = NothingB

-- Notice Better Precision (smaller solution space) than Maybe
append' :: Semigroup m => 
           Maybe m -> Maybe m -> Maybe m
append' _ _ = Nothing -- cannot do that with MaybeB!

-- this works
firstKnown :: Semigroup m => MaybeB True m -> MaybeB b m -> MaybeB 'True m
firstKnown = append

-- Problem in paradise!
secondKnown :: Semigroup m => MaybeB b m -> MaybeB 'True m -> MaybeB 'True m
secondKnown = undefined 
-- secondKnown = append
-- secondKnown = flip append  -- brittle!!