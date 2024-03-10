{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PartialFunctions () where 

import Data.List (intercalate)
import Control.Monad (mplus)
import Data.Maybe (fromMaybe)

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
-- shrinkFun :: (c -> [c]) -> (a :-> c) -> [a :-> c]

