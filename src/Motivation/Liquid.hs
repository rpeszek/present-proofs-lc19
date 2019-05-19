module Motivation.Liquid where

import         Data.List ((!!))

-- if Liquid is global-cabal installed
-- liquid src/Motivation/Liquid.hs

{-@ (!!!) :: x: [a] -> {i:Nat | i < len x} -> a @-}
(!!!) :: [a] -> Int -> a
(!!!) = (!!)

test :: Int
test = (!!!) [1,2,3] 1

-- Liquid will not allow this:
-- test2 :: Int
-- test2 = (!!!) [1,2,3] 3

-- Liquid comes with predefined refinements 
-- this will not compile:

-- test3 :: Int
-- test3 = (!!) [1,2,3] 3
