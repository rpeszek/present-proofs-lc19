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
      , AllowAmbiguousTypes
#-}

module Extras.TypeLev where

import           Data.Singletons.Prelude.Bool

type family EQ (a :: k) (b :: k) where
  EQ a a = 'True
  EQ a b = 'False
 
-- ghc error, ghc needs value level evidence
-- test0 :: Sing (b :: Bool) -> Sing (EQ True (b :|| True)) -> Sing True
-- test0 _ = id

test1 :: Sing (b :: Bool) -> Sing (EQ True (b :|| True)) -> Sing True
test1 STrue = id
test1 SFalse = id