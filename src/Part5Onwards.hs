{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Use first" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use if" #-}


module Part5Onwards () where 

import Parts1To4 (Expr (..), check)

import Prelude hiding ((**))


--------------------------------------------------------------------------------
-- Part 5: Fair Enumeration
--------------------------------------------------------------------------------
-- 5.1 Tiered enumeration

-- | Typeclass of enumerable values, with two functions 
-- defined in terms of each other 
-- (user can define any Listable instance most convenient for them)
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
(x:xs) `interleave` ys = x : (ys `interleave` xs)

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
-- 5.5 Finding counter-examples & reporting test results 

-- Example 5.1
rotateL :: Expr -> Expr
rotateL (Add e1 (Add e2 e3)) = Add (Add e1 e2) e3

rotateR :: Expr -> Expr
rotateR (Add (Add e1 e2) e3) = Add e1 (Add e3 e2)

prop_rotRotId :: Expr -> Expr -> Expr -> Bool
prop_rotRotId e1 e2 e3 = rotateR (rotateL e) == e
  where 
    e = Add e1 (Add e2 e3)

-- > *** failed for: (Val 0) (Val 0) (Val 1)
test1 :: IO ()
test1 = check prop_rotRotId

--------------------------------------------------------------------------------
-- Part 6: Conditional Properties & Data Invariants
--------------------------------------------------------------------------------

-- | The logical implication operator 
( ==> ) :: Bool -> Bool -> Bool
False ==> _ = True 
True  ==> p = p 
infixr 0 ==>

-- | Tiered version of `filter`
filterT :: (a -> Bool) -> [[a]] -> [[a]]
filterT = map . filter

-- | Flipped version of `filterT`
suchThat :: [[a]] -> (a -> Bool) -> [[a]]
suchThat = flip filterT

-- | A datatype of non-negative numbers
newtype NonNeg n = NonNeg n 

-- | `Listable` instance for non-negative numbers
instance (Listable n, Num n, Ord n) => Listable (NonNeg n) where 
  tiers :: [[NonNeg n]]
  tiers = cons1 NonNeg `suchThat` nonNegOk 
    where 
      nonNegOk (NonNeg n) = n >= 0

--------------------------------------------------------------------------------
-- Part 7: Functions as Test Values
--------------------------------------------------------------------------------
-- Mutating functions

-- | `mutate f ms` mutates the function `f` given a list `ms` of mutants 
-- (exception pairs)
mutate :: Eq a => (a -> b) -> [(a, b)] -> (a -> b)
mutate f ms = foldr mut f ms 
  where 
    mut :: Eq a => (a, b) -> (a -> b) -> a -> b
    mut (x', fx') f x = if x == x' then fx' else f x 

--------------------------------------------------------------------------------
-- Enumerating exceptions

-- | Takes tiers of arguments and results, and returns tiers of 
--   lists of pairs of arguments and results. For example:
-- > exceptionPairs (tiers :: [[Word]]) (tiers :: [[Word]])
-- > [ [[]]
-- >  , [[(0,0)]]
-- >  , [[(0,1)],[(1,0)]]
-- >  , [[(0,2)],[(1,1)],[(0,0),(1,0)],[(2,0)]]
-- >  , ... ]
exceptionPairs :: [[a]] -> [[b]] -> [[ [(a, b)] ]]
exceptionPairs xss yss = 
  concatMapT (`excep` yss) (properSubsetsOf xss)
    where 
      excep :: [a] -> [[b]] -> [[ [(a, b)] ]]
      excep xs sbs = zip xs `mapT` products (const sbs `map` xs)

-- | Returns tiers of proper sublists from values from a given tier-list
properSubsetsOf :: [[a]] -> [[ [a] ]]
properSubsetsOf = init . setsOf 
  where 
    -- To define `properSubsetsOf`, we need a few helper functions 
    -- (taken from the LeanCheck source code, not in the paper)
    setsOf :: [[a]] -> [[[a]]]
    setsOf = ([[]]:) . concatT . setChoicesWith (\x xss -> mapT (x:) (setsOf xss))

    setChoicesWith :: (a -> [[a]] -> b) -> [[a]] -> [[b]]
    setChoicesWith _ []  =  []
    setChoicesWith _ [[]]  =  []
    setChoicesWith f ([]:xss)
      =  []
      :  setChoicesWith (\y yss -> f y ([]: normalizeT yss)) xss
    setChoicesWith f ((x:xs):xss)
      =  [[f x (xs:xss)]]
      \/ setChoicesWith f (xs:xss)
      
-- | Takes the product of N lists of tiers, producing lists of length N
-- (from LeanCheck source code)
products :: [ [[a]] ] -> [[ [a] ]]
products  =  foldr (productWith (:)) [[[]]]

-- | Take a tiered product of lists of tiers.
-- (taken from LeanCheck source code)
productWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
productWith _ _ []  =  []
productWith _ [] _  =  []
productWith f (xs:xss) yss  =  map (xs **) yss
                            \/ delay (productWith f xss yss)
  where
    xs ** ys  =  [x `f` y | x <- xs, y <- ys]

-- | Normalizes tiers by removing up to 12 empty tiers from the end of a list
--   of tiers. (taken from LeanCheck)
--
-- > normalizeT [xs0,xs1,...,xsN,[]]     =  [xs0,xs1,...,xsN]
-- > normalizeT [xs0,xs1,...,xsN,[],[]]  =  [xs0,xs1,...,xsN]
--
-- The arbitrary limit of 12 tiers is necessary as this function would loop if
-- there is an infinite trail of empty tiers.
normalizeT :: [[a]] -> [[a]]
normalizeT []  =  []
normalizeT [[]]  =  []
normalizeT [[],[]]  =  []
normalizeT [[],[],[]]  =  []
normalizeT [[],[],[],[]]  =  []
normalizeT [[],[],[],[], []]  =  []
normalizeT [[],[],[],[], [],[]]  =  []
normalizeT [[],[],[],[], [],[],[]]  =  []
normalizeT [[],[],[],[], [],[],[],[]]  =  []
normalizeT [[],[],[],[], [],[],[],[], []]  =  []
normalizeT [[],[],[],[], [],[],[],[], [],[]]  =  []
normalizeT [[],[],[],[], [],[],[],[], [],[],[]]  =  []
normalizeT [[],[],[],[], [],[],[],[], [],[],[],[]]  =  []
normalizeT (xs:xss)  =  xs:normalizeT xss

--------------------------------------------------------------------------------
-- Enumerating functions

-- | Takes tiers of arguments & tiers of results, returning tiers of functions
( -->> ) :: Eq a => [[a]] -> [[b]] -> [[ a -> b ]]
xss -->> yss = 
  concatMapT 
    (\(r, yss) -> mapT (const r `mutate`) (exceptionPairs xss yss))
    (choices yss)

-- | `Listable` instance of functions
-- 1. Arguments should be instances of both `Eq` & `Listable`
-- 2. Results should be instances of `Listable`
instance (Eq a, Listable a, Listable b) => Listable (a -> b) where 
  tiers :: [[ a -> b ]]
  tiers = tiers -->> tiers 

-- | Enumerated functions of type `Bool -> Bool` (Example 7.2)
tiersBoolToBool :: [[Bool -> Bool]]
tiersBoolToBool = 
  [ [ const False, 
      const True ],
    [ \case False -> True; True -> False,
      \case False -> False; True -> True ]
  ]

-- | Returns `tiers` of choices for result values
-- Choices are pairs of values and tiers excluding that value.
-- (taken from Leancheck source code)
-- > choices [[False,True]] == [[(False,[[True]]),(True,[[False]])]]
-- > choices [[1],[2],[3]]
-- >   == [ [(1,[[],[2],[3]])]
-- >      , [(2,[[1],[],[3]])]
-- >      , [(3,[[1],[2],[]])] ]
--
-- Each choice is sized by the extracted element.
choices :: [[a]] -> [[(a,[[a]])]]
choices  =  choicesWith (,)

-- | Like 'choices', but allows a custom function. (taken from LeanCheck)
choicesWith :: (a -> [[a]] -> b) -> [[a]] -> [[b]]
choicesWith _ []  =  []
choicesWith _ [[]]  =  []
choicesWith f ([]:xss)
  =  [] : choicesWith (\y yss -> f y ([]:normalizeT yss)) xss
choicesWith f ((x:xs):xss)
  =  [[f x (xs:xss)]]
  \/ choicesWith (\y (ys:yss) -> f y ((x:ys):yss)) (xs:xss)

--------------------------------------------------------------------------------
-- Part 8: LeanCheck
--------------------------------------------------------------------------------  
-- 8.1 Existential properties

-- | Checks if there `exists` an assignemnt of values that satisfy a property 
-- up to `n` values
exists :: Testable a => Int -> a -> Bool 
exists n = or . take n . map snd . results 
