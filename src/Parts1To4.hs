{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Parts1To4 where

--------------------------------------------------------------------------------
-- Part 1: Intro
--------------------------------------------------------------------------------

-- | A faulty sorting function
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = filter (< x) xs ++ [x] ++ filter (> x) xs

-- | Checks if a list is sorted
isSorted :: Ord a => [a] -> Bool 
isSorted []  = True 
isSorted [_] = True  
isSorted (x:y:xs) = x <= y && isSorted (y : xs)

-- | Checks that a sorted list is sorted
prop_sortOrdered :: Ord a => [a] -> Bool 
prop_sortOrdered xs = isSorted (sort xs)

-- | `count x xs` is the no. of times `x` appears in `xs`
count :: Eq a => a -> [a] -> Int 
count x xs = foldr (\y acc -> if x == y then acc + 1 else acc) 0 xs

-- | The counts of elements don't change after sorting
prop_sortCount :: Ord a => a -> [a] -> Bool
prop_sortCount x xs = count x (sort xs) == count x xs

--------------------------------------------------------------------------------
-- Part 2: Mark I: Generate and Test
--------------------------------------------------------------------------------

-- | Listable types are those for which there is a declared 
-- | `list` of all values of that type
class Listable a where 
  list :: [a]

-- Listaable instance for Bools
instance Listable Bool where 
  list :: [Bool]
  list = [False, True]

--------------------------------------------------------------------------------
-- Ints

-- | Lazily interleaves two lists 
( \/ ) :: [a] -> [a] -> [a]
[] \/ ys = ys 
(x:xs) \/ ys = x:(ys \/ xs)

-- | Listable instance for ints
instance Listable Int where 
  list :: [Int]
  list = [0, -1..] \/ [1..]

--------------------------------------------------------------------------------
-- Pairs 

-- | `xs >< ys` is the Cartesian product of xs and ys
-- (with modifications to handle infinite lists)
( >< ) :: [a] -> [b] -> [(a, b)]
[] >< _ = []
_ >< [] = []
(x:xs) >< ys = [(x, y) | y <- ys] \/ (xs >< ys)

-- | Listable instance for Pairs  
instance (Listable a, Listable b) => Listable (a, b) where 
  list :: [(a, b)]
  list = list >< list

--------------------------------------------------------------------------------
-- Lists 

-- | Listable instance for lists
instance Listable a => Listable [a] where 
  -- | Start with the empty list, 
  -- then recursively list all lists of the form `x:xs`
  list :: [[a]]
  list = [] : [ x:xs | (x, xs) <- list ]

--------------------------------------------------------------------------------
-- Searching for counter-examples

-- | `counterExamples0 n p` lists `n` counterexamples that fail the predicate `p` 
counterExamples0 :: Listable a => Int -> (a -> Bool) -> [a]
counterExamples0 n p = 
  [x | x <- take n list, not (p x)]

--------------------------------------------------------------------------------
-- Showing test reuslts

-- | Reports whether a property `p` is true out of `n` test values
checkFor0 :: (Show a, Listable a) => Int -> (a -> Bool) -> IO ()
checkFor0 n p = 
  case counterExamples n p of 
    []    -> putStrLn "+++ OK!"
    (x:_) -> putStrLn $ "*** failed for: " ++ show x 

-- | Checks whether a property is true out of 200 test values
check0 :: (Show a, Listable a) => (a -> Bool) -> IO ()
check0 = checkFor0 200   

--------------------------------------------------------------------------------
-- Part 3: Algebraic data types
--------------------------------------------------------------------------------

-- Shape shifting

-- | Returns a list of values obtained by applying a constructor `c`
cons1 :: Listable a => (a -> b) -> [b]
cons1 c = [c x | x <- list]

-- | Like `cons1`, but for constructors with arity-2
cons2 :: (Listable a, Listable b) => (a -> b -> c) -> [c]
cons2 c = [c x y | (x, y) <- list]

-- | Creates a singleton list containing the nullary onstructor `c`
cons0 :: a -> [a]
cons0 c = [c]

--------------------------------------------------------------------------------
-- | Hutton's Razor (expression language with addition & ints)
data Expr = Val Int
          | Add Expr Expr 
  deriving (Show, Eq)

-- | Listable instance for `Expr`s
instance Listable Expr where 
  list :: [Expr]
  list = cons1 Val \/ cons2 Add

-- | Evaluates an `Expr` to an integer 
eval :: Expr -> Int 
eval (Val i) = i 
eval (Add e1 e2) = eval e1 + eval e2

--------------------------------------------------------------------------------
-- Part 4: Multi-argument properties
--------------------------------------------------------------------------------

-- | A typeclass of multi-argument properties
class Testable a where 
  -- | Takes a `Testable` property and returns a list of `Result`s
  results :: a -> [Result]

-- | A `Result` is a list of string arguments & the result of testing the 
-- property on those arguments
type Result = ([String], Bool)  

--------------------------------------------------------------------------------
-- Testable booleans

-- | A boolean value is a property w/ no arguments, 
-- where the only result is its value
instance Testable Bool where 
  results :: Bool -> [Result]
  results p = [([], p)]

--------------------------------------------------------------------------------
-- Testable functions 

{- `Testable` instance for functions of type `a -> b`

  1. The argument type `a` must have `Show` & `Listable` instances defined,
    so that we can print and generate arguments.
  2. The return type `b` must be `Testable`: note that `p x` is the property `p`
    specialized with the test value `x`

  Since `->` is right associative, `a -> (c -> Bool) == a -> c -> Bool`,
  and we can instantiate `b` at a function type as long 
  as the final return type is Bool. 
-} 
instance (Show a, Listable a, Testable b) => Testable (a -> b) where 
  results :: (a -> b) -> [Result]
  results p = foldr (\/) [] [resultsFor x | x <- list]
    where 
      -- resultsFor :: a -> [Result]
      resultsFor x = [(show x : as, r) | (as, r) <- results (p x)]

--------------------------------------------------------------------------------
-- Finding counterexamples for `Testable` values

-- | `counterExamples n p` lists `n` counterexamples that fail the 
-- `Testable` property `p` 
counterExamples :: Testable a => Int -> a -> [[String]]
counterExamples n p = [ as | (as, False) <- take n (results p) ]

-- | Reports whether a `Testable` property `p` is true out of `n` test values
checkFor :: Testable a => Int -> a -> IO ()
checkFor n p = 
  case counterExamples n p of 
    []       -> putStrLn "+++ OK!"
    (ce : _) -> putStrLn $ "*** failed for: " ++ unwords ce

-- | Checks whether a property is true out of 200 test values
check :: Testable a => a -> IO ()
check = checkFor 200
