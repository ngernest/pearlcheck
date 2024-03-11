{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module PartialFunctions () where

import Data.List (intercalate)
import Control.Monad (mplus)
import Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------
-- 4: A datatype for partial functions
--------------------------------------------------------------------------------

-- | A GADT for partial functions of type `a -> c`
data a :-> c where
  -- | Constant function of type `() -> c`
  Unit :: c -> (() :-> c)

  -- | Uncurries a partial function 
  Pair :: (a :-> (b :-> c)) -> ((a,b) :-> c)

  -- | Construct partial functions from sums 
  -- (yielding a result if their argument uses `Left` or `Right` respectively)
  Lft :: (a :-> c) -> (Either a b :-> c)
  Rgt :: (b :-> c) -> (Either a b :-> c)

  -- | Glues together two disjoint partial functions
  (:+:) :: (a :-> c) -> (a :-> c) -> (a :-> c)

  -- | Never returns anything
  Nil :: a :-> c

  -- | Takes `g, h` such that `h . g == id`, and uses them two convert 
  -- between a new type `c` and an already supported type
  Map :: (a -> b) -> (b -> a)
                      -> (b :-> c) -> (a :-> c)

-- | Functor instance for `a :->`
instance Functor ((:->) a) where
  fmap :: (a2 -> b) -> (a1 :-> a2) -> a1 :-> b
  fmap f (Pair p)    = Pair (fmap (fmap f) p)
  fmap f (p:+:q)     = fmap f p :+: fmap f q
  fmap f (Unit c)    = Unit (f c)
  fmap _ Nil         = Nil
  fmap f (Map g h p) = Map g h (fmap f p)                      

-- | Converts a partial function to a table of entries
table :: (a :-> c) -> [(a, c)]
table (Unit c) = [((), c)]
table (Pair p) = [((x, y), c) | (x, q) <- table p,
                                (y, c) <- table q]
table (Lft p) = [(Left x, c) | (x, c) <- table p]
table (Rgt q) = [(Right y, c) | (y, c) <- table q]
table (p :+: q) = table p ++ table q
table Nil = []
table (Map _ h p) = [(h x, c) | (x, c) <- table p]

-- | `display d xys` pretty-prints a function table `xys`, along with a default 
-- result `d` (since all functions displayed to the user must be total) 
display :: (Show a, Show c) => c -> [(a, c)] -> String
display d xys = "{" ++
  intercalate "," (
    [show x ++ " -> " ++ show y | (x, y) <- xys ]
      ++ ["_ -> " ++ show d]
  ) ++ "}"

-- | Takes a partial function & an argument, and possibly yields a result
papply :: (a :-> c) -> (a -> Maybe c)

-- Always respond the result `c`
papply (Unit c) _ = Just c

-- Lookup `x` in the argument function `p`, and if it exists, lookup `y` in the 
-- resultant function
papply (Pair p) (a, b) = do
  q <- papply p a
  papply q b

-- `Lft` & `Rgt succeed only when their arguments are `Left` & `Right`
papply (Lft p) (Left a) = papply p a
papply (Rgt q) (Right b) = papply q b

-- First try `p`, then try `q`
papply (p :+: q) a = papply p a `mplus` papply q a

-- Use `g` to obtain `g a` (a value of type `b`), then apply `p` to it
papply (Map g _ p) a = papply p (g a)

-- Catches `Nil`, and `Lft / Rgt` if they are used with the wrong constructor
papply _ _ = Nothing


-- | Guarantees that applying `p` succeeds, by providing a default result `c`
apply :: c -> (a :-> c) -> (a -> c)
apply c p = fromMaybe c . papply p


-- | A shrinker for functions that uses the auxiliary shrinker `shr :: c -> [c]`
shrinkFun :: (c -> [c]) -> (a :-> c) -> [a :-> c]
shrinkFun shr (Unit c) =
  Nil : [Unit c' | c' <- shr c]
shrinkFun shr (Pair p) =
  [ Pair p' | p' <- shrinkFun (shrinkFun shr) p ]
shrinkFun shr (Lft p) =
  [ Lft p' | p' <- shrinkFun shr p ]
shrinkFun shr (Rgt q) =
  [ Rgt q' | q' <- shrinkFun shr q ]
shrinkFun shr (p :+: q) =
  [p, q] ++
  [ p :+: q' | q' <- shrinkFun shr q ] ++
  [ p' :+: q | p' <- shrinkFun shr p ]
shrinkFun _ Nil = []
shrinkFun shr (Map g h p) =
  [ Map g h p' | p' <- shrinkFun shr p ]

--------------------------------------------------------------------------------
-- 5: Building partial functions
--------------------------------------------------------------------------------

-- | Typeclass for argument types `a` for which it is possible to take a regular
-- function of type `a -> c` and represent it as a partial function `a :-> c`
class Argument a where 
  build :: (a -> c) -> (a :-> c)

instance Argument () where 
  build :: (() -> c) -> () :-> c
  build f = Unit $ f ()

instance (Argument a, Argument b) => Argument (a, b) where 
  -- | To convert `f` into a partial function, curry `f`, 
  -- build a partial function, and build partial functions for 
  -- all results of the resultant function
  build :: ((a, b) -> c) -> (a, b) :-> c
  build f = Pair $ build `fmap` build (curry f)  

instance (Argument a, Argument b) => Argument (Either a b) where 
  -- | Build two partial functions, one for Left & one for Right, 
  -- then glue them together using `:+:`
  build :: (Either a b -> c) -> Either a b :-> c
  build f = Lft (build (f . Left)) :+: Rgt (build (f . Right))  


-- | Turns a function `f :: a -> c` into a partial function `a :-> c`,
-- as long as we have functions `g` & `h` which allow us to go 
-- from `a` to `b` and back
buildMap :: Argument b => (a -> b) -> (b -> a) -> (a -> c) -> (a :-> c)
buildMap g h f = Map g h (build (f . h))

-- Figure 8: build function for bools
instance Argument Bool where 
  -- Bool is isomorphic to `Either () ()`
  build :: (Bool -> c) -> Bool :-> c
  build = buildMap from to 
    where 
      from :: Bool -> Either () ()
      from False = Left ()
      from True  = Right ()

      to :: Either () () -> Bool 
      to (Left _) = False 
      to (Right _) = True

-- Figure 8: build function for lists
instance Argument a => Argument [a] where 
  -- `[a]` is isormophic to `Either () (a, [a])`
  build :: ([a] -> c) -> [a] :-> c
  build = buildMap from to 
    where
      from :: [a] -> Either () (a, [a]) 
      from [] = Left ()
      from (x:xs) = Right (x, xs)

      to :: Either () (a, [a]) -> [a]
      to (Left _) = [] 
      to (Right (x, xs)) = x:xs

-- Figure 9: build function for integers
instance Argument Int where 
  -- Use  two's complement to go from `Int` to `Either (Bool, Int) Int` & back
  build :: (Int -> c) -> Int :-> c
  build = buildMap from to 
    where 
      from :: Int -> Either (Bool, Int) Bool
      from 0 = Right False 
      from (-1) = Right True 
      from x = Left (odd x, x `div` 2)

      to :: Either (Bool, Int) Bool -> Int
      to (Right False) = 0
      to (Right True) = -1 
      to (Left (b, x)) = bit b + 2 * x
      
      -- Converts a `Bool` to a bit
      bit :: Bool -> Int 
      bit False = 0
      bit True = 1

-- | Provides a `build` function for any type with a `show` & `read` function
-- buildShow :: (Show a, Read a) => (a -> c) -> (a :-> c)
-- buildShow f = buildMap show read f

--------------------------------------------------------------------------------
-- 6: The Fun modifier (TODO)
--------------------------------------------------------------------------------
