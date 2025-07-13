import Data.Ord

namespace Data.Num

open Data.Ord

class Sign a where
  sign : a -> a

class Num a extends Add a, Sub a, Mul a, Div a, Ord a, LT a, LE a, BEq a where

instance : Num Nat where

instance : Num Int where

instance : Num Float32 where

instance : Num Float where
