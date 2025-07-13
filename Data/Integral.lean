import Data.Num
import Data.Char

namespace Data.Integral

open Data.Num
open Data.Char

universe u

class OfInt (a : Type u) where
  ofInt : Int -> a

instance : OfInt Nat where
  ofInt x := x.toNat

instance : OfInt Int where
  ofInt := id

instance : OfInt Int8 where
  ofInt := Int8.ofInt

instance : OfInt Int16 where
  ofInt := Int16.ofInt

instance : OfInt Int32 where
  ofInt := Int32.ofInt

instance : OfInt Int64 where
  ofInt := Int64.ofInt

instance : OfInt UInt8 where
  ofInt := UInt8.ofNat ∘ Int.toNat

instance : OfInt UInt16 where
  ofInt := UInt16.ofNat ∘ Int.toNat

instance : OfInt UInt32 where
  ofInt := UInt32.ofNat ∘ Int.toNat

instance : OfInt UInt64 where
  ofInt := UInt64.ofNat ∘ Int.toNat

instance : OfInt Float32 where
  ofInt := Float32.ofInt

instance : OfInt Float where
  ofInt := Float.ofInt

class Integral (a : Type u) extends Num a, Zero a, /- Real a, -/ Div a, Mod a, OfInt a, Ord a, LT a, BEq a, LE a where
  toInt : a -> Int

instance : Integral Nat where
  toInt := Int.ofNat

instance : Integral Int where
  toInt := id

instance : Integral Int8 where
  toInt := Int8.toInt

instance : Integral Int16 where
  toInt := Int16.toInt

instance : Integral Int32 where
  toInt := Int32.toInt

instance : Integral Int64 where
  toInt := Int64.toInt

instance : Integral UInt8 where
  toInt := Int.ofNat ∘ UInt8.toNat

instance : Integral UInt16 where
  toInt := Int.ofNat ∘ UInt16.toNat

instance : Integral UInt32 where
  toInt := Int.ofNat ∘ UInt32.toNat

instance : Integral UInt64 where
  toInt := Int.ofNat ∘ UInt64.toNat

def fromIntegral {a b : Type u} [Integral a] [OfInt b] : a -> b :=
  OfInt.ofInt ∘ Integral.toInt

def divMod {n : Type u} [Div n] [Mod n] (a : n) (b : n) : n × n :=
  (a / b, a % b)

partial def itosb (b : Nat) (n : Int) : String :=
  if n >= 0 ∧ n.toNat < b then
    String.singleton $ natToDigit n.toNat
  else
    let (q, r) := divMod n $ Int.ofNat b
    itosb b q ++ (natToDigit $ r.toNat).toString

