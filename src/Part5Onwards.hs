{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Use first" #-}

module Part5Onwards () where 

import Prelude hiding ((**))

---------------------------------------------------s-----------------------------
-- Part 5: Fair Enumeration
--------------------------------------------------------------------------------
-- 5.1 Tiered enumeration

-- | Typeclass of enumerable values, with two functions 
-- defined in terms of each other 
-- (user can define any Listable instance most convenience for them)
class Listable a where 
  -- | `tiers` is a possibly infinite list of finite sublists, 
  -- characterized by some notion of size. Each sublist represents a *tier*. 
  tiers :: [[a]]
  tiers = map (:[]) list 

  -- | To `list` all values of a type, we concatenate `tiers`
  list :: [a]
  list = concat tiers

instance Listable Bool where 
  tiers :: [[Bool]]
  tiers = [[False, True]]

instance Listable Word where 
  -- tiers = [[0], [1], [2], ...]
  tiers :: [[Word]]
  tiers = [[n] | n <- [0..]]

--------------------------------------------------------------------------------
-- 5.2 Manipulating tiers 

-- | Sum of two tier-lists.
-- For tier-lists with the same number of tiers, 
-- ( \/ ) is equivalent to `zipWith (++)` 
( \/ ) :: [[a]] -> [[a]] -> [[a]]
xss \/ [] = xss 
[] \/ yss = yss 
(xs : xss) \/ (ys : yss) = (xs ++ ys) : xss \/ yss 

-- | Same as the old version of ( \/ ) in Parts1To4.hs
interleave :: [a] -> [a] -> [a]
[] `interleave` ys = ys 
(x:xs) `interleave` ys = x : (ys `interleave `xs)

-- | Product of two tier-lists
-- In the 3rd case, as we peel-off one tier to extract `xss`, we need to 
-- `delay` the enumeration of the 2nd arg to `\/`
( >< ) :: [[a]] -> [[b]] -> [[(a, b)]]
_ >< [] = [] 
[] >< _ = [] 
(xs : xss) >< yss =  map (xs ** ) yss \/ delay (xss >< yss)
    where 
      ( ** ) :: [a] -> [b] -> [(a, b)]
      as ** bs = [(a, b) | a <- as, b <- bs]

-- | Prepends an empty list, increasing the size assigned to 
-- elements in a tier enumeration. Note that:
-- > delay [[x, y], [a, b]] = [[], [x, y], [a, b]]
delay :: [[a]] -> [[a]]
delay = ([] :)    

--------------------------------------------------------------------------------
-- 5.2 (continued) Constructing tiers 

-- Wraps the value in a tier-list with a single value of size 0
cons0 :: a -> [[a]]
cons0 x = [[x]]

-- | maps a constructor into a tiered enumeration, delaying it once
cons1 :: Listable a => (a -> b) -> [[b]]
cons1 f = delay (mapT f tiers)

-- | Variant of `map` for tier-lists
mapT :: (a -> b) -> [[a]] -> [[b]]
mapT = map . map

cons2 :: (Listable a, Listable b) => (a -> b -> c) -> [[c]]
cons2 f = delay (mapT (uncurry f) tiers)

cons3 :: (Listable a, Listable b, Listable c) => (a -> b -> c -> d) -> [[d]]
cons3 f = delay (mapT (uncurry3 f) tiers)
  where
    uncurry3 g (x,y,z) = g x y z

--------------------------------------------------------------------------------
-- 5.3 Listable instances using tiers

-- | Listable instance for lists
instance Listable a => Listable [a] where 
  tiers :: [[[a]]]
  tiers = cons0 [] \/ cons2 (:)

-- | Listable instance for pairs
instance (Listable a, Listable b) => Listable (a, b) where 
  tiers :: [[(a, b)]]
  tiers = tiers >< tiers

-- | Listable instance for triples
instance (Listable a, Listable b, Listable c) => Listable (a, b, c) where 
  tiers :: [[(a, b, c)]]
  tiers = mapT (\(x, (y, z)) -> (x, y, z)) tiers

-- | Listable instance for ints
-- 0 has size 0, 1 has size 1, -1 has size 2, 2 has size 3, etc
-- Having one Int per tier works better as it prevents the enumeration 
-- from blowing up (See pg. 6 of paper)
instance Listable Int where 
  list :: [Int]
  list = [0, -1..] `interleave` [1..]

--------------------------------------------------------------------------------
-- 5.4 `Testable` typeclass: tiers of tests

-- | A `Result` is a list of arguments & a boolean test result for those args
type Result = ([String], Bool)

-- | Typeclass of Testable properties
class Testable a where 
  -- | Given a `Testable` property, `resultiers` returns tiers of results 
  resultiers :: a -> [[Result]]

-- Returns a list of `results` by concatenating all the tiered results
results :: Testable a => a -> [Result]
results = concat . resultiers

-- | `Testable` instance for bools
instance Testable Bool where 
  resultiers :: Bool -> [[Result]]
  resultiers p = [[([], p)]]

-- | `Testable` instance for functions
instance (Show a, Listable a, Testable b) => Testable (a -> b) where 
  resultiers :: (a -> b) -> [[Result]]
  resultiers p = concatMapT resultiersFor tiers 
    where 
      resultiersFor x = (\(as, r) -> (show x : as, r)) `mapT` resultiers (p x)  

-- | Tiered equivalent of `concat`
concatT :: [[ [[a]] ]] -> [[a]]
concatT = foldr (\+:/) [] . map (foldr (\/) [])
  where 
    ( \+:/ ) :: [[a]] -> [[a]] -> [[a]]
    xss \+:/ yss = xss \/ delay yss

-- | Tiered equivalent of `concatMap`
concatMapT :: (a -> [[b]]) -> [[a]] -> [[b]]
concatMapT f = concatT . mapT f

--------------------------------------------------------------------------------