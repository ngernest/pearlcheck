{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PartialFunctions () where 

data a :-> c where
  Unit :: c -> (() :-> c)
  
  Pair :: (a :-> (b :-> c)) -> ((a,b) :-> c) 
  
  Lft :: (a :-> c) -> (Either a b :-> c)
  Rgt :: (b :-> c) -> (Either a b :-> c) 
  
  (:+:) :: (a :-> c) -> (a :-> c) -> (a :-> c) 
  
  Nil :: a :-> c
  Map :: (a -> b) -> (b -> a)
                      -> (b :-> c) -> (a :-> c)  