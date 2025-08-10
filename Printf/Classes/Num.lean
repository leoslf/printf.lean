import Printf.Extensions

namespace Printf

class Sign a where
  sign : a -> a

class Num a extends Add a, Sub a, Mul a, Div a, Ord a, LT a, LE a, BEq a where

instance : Num Nat where

instance : Num Int where

instance : Ord Float32 where
  compare a b :=
    if a == b then
      .eq
    else if a < b then
      .lt
    else
      .gt

instance : Num Float32 where

instance : Ord Float where
  compare a b :=
    if a == b then
      .eq
    else if a < b then
      .lt
    else
      .gt

instance : Num Float where

