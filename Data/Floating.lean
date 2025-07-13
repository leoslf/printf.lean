import Data.Num
import Data.Ord
import Data.Char
import Data.Integral

namespace Data.Floating

open Data.Num
open Data.Ord
open Data.Char
open Data.Integral

universe u

def Float32.radix  := 2 -- FLT_RADIX
def Float32.digits := 24 -- FLT_MANT_DIG
def Float32.minExp := -125 -- FLT_MIN_EXP
def Float32.maxExp := 128 -- FLT_MAX_EXP

def Float.radix := 2 -- FLT_RADIX
def Float.digits := 53 -- DBL_MANT_DIG
def Float.minExp := -1021 -- DBL_MIN_EXP
def Float.maxExp := 1024 -- DBL_MAX_EXP

class Real (a : Type u) extends OfInt a, Num a, Neg a, Ord a, LT a, BEq a where
  -- toRational : a -> Rat

instance : Real Float32 where
  -- toRational a :=

instance : Real Float where
  -- toRational a :=

class Fractional (a : Type u) extends Num a, OfInt a where
  /-- Reciprocal fraction -/
  recip : a -> a
  -- fromRational : Rational -> a

instance : Fractional Float32 where
  recip x := 1 / x
  -- fromRational : Rational -> a

instance : Fractional Float where
  recip x := 1 / x
  -- fromRational : Rational -> a

inductive FFFormat where
| Exponent : FFFormat
| Fixed : FFFormat
| Generic : FFFormat

partial def Float32.decode (x : Float32) : Int × Int :=
  if x == 0.0 then
    (0, 0)
  else
    let (m, e) := x.frExp -- x := m * 2 ^ e, where 0.5 <= |m| < 1.0
    let rec scale (k : Int) : Int × Int :=
      let scaled := Float32.scaleB m k
      if scaled == scaled.toInt64.toFloat32  then
        (scaled.toInt64.toInt, e - k)
      else
        scale (k + 1)
    let (significand, exponent) := scale 0
    (if x < 0 then -significand else significand, exponent)

def Float32.encode (significand : Int) (exponent : Int) : Float32 :=
  Float32.scaleB (Float32.ofInt significand) exponent

partial def Float.decode (x : Float) : Int × Int :=
  if x == 0.0 then
    (0, 0)
  else
    let (m, e) := x.frExp -- x := m * 2 ^ e, where 0.5 <= |m| < 1.0
    let rec scale (k : Int) : Int × Int :=
      let scaled := Float.scaleB m k
      if scaled == scaled.toInt64.toFloat then
        (scaled.toInt64.toInt, e - k)
      else
        scale (k + 1)
    let (significand, exponent) := scale 0
    (if x < 0 then -significand else significand, exponent)

def Float.encode (significand : Int) (exponent : Int) : Float :=
  Float.scaleB (Float.ofInt significand) exponent

class RealFrac (a : Type u) extends Real a, Fractional a where
  properFraction : {b : Type u} -> [Integral b] -> a -> b × a
  /-- truncate x returns the integer nearest x between zero and x -/
  truncate : {b : Type v} -> [Integral b] -> a -> b
  round : {b : Type v} -> [Integral b] -> a -> b
  ceil : {b : Type v} -> [Integral b] -> a -> b
  floor : {b : Type v} -> [Integral b] -> a -> b

def Float32.properFraction {b : Type u} [Integral b] (x : Float32) : b × Float32 :=
  match Float32.decode x with
  | (m, n) =>
    if n >= 0 then
      (OfInt.ofInt $ m * Float32.radix ^ n.toNat, 0.0)
    else
      match divMod m (Float32.radix ^ (-n).toNat) with
      | (w, r) => (OfInt.ofInt w, Float32.encode r n)

def Float32.truncate {b : Type u} [Integral b] (x : Float32) : b :=
  let (m, _) := Float32.properFraction x
  m

instance : RealFrac Float32 where
  properFraction := Float32.properFraction
  truncate := Float32.truncate
  round := Float32.truncate ∘ Float32.round
  ceil := Float32.truncate ∘ Float32.ceil
  floor := Float32.truncate ∘ Float32.floor

def Float.properFraction {b : Type u} [Integral b] (x : Float) : b × Float :=
  match Float.decode x with
  | (m, n) =>
    if n >= 0 then
      (OfInt.ofInt $ m * Float.radix ^ n.toNat, 0.0)
    else
      match divMod m (Float.radix ^ (-n).toNat) with
      | (w, r) => (OfInt.ofInt w, Float.encode r n)

def Float.truncate {b : Type u} [Integral b] (x : Float) : b :=
  let (m, _) := Float.properFraction x
  m

instance : RealFrac Float where
  properFraction := Float.properFraction
  truncate := Float.truncate
  round := Float.truncate ∘ Float.round
  ceil := Float.truncate ∘ Float.ceil
  floor := Float.truncate ∘ Float.floor

class Floating (a : Type u) extends Fractional a, OfScientific a where
  pi : a := 3.141592653589793238462643383279502884
  exp : a -> a
  /-- Natural Logarithm -/
  log : a -> a
  -- log1p : a -> a
  sqrt : a -> a
  sin : a -> a
  cos : a -> a
  tan : a -> a
  asin : a -> a
  acos : a -> a
  atan : a -> a
  sinh : a -> a
  cosh : a -> a
  tanh : a -> a
  asinh : a -> a
  acosh : a -> a
  atanh : a -> a

def Floating.logBase {f : Type u} [Floating f] (base : f) (x : f) : f :=
  Floating.log x / Floating.log base

instance : Floating Float32 where
  exp := Float32.exp
  log := Float32.log
  sqrt := Float32.sqrt
  sin := Float32.sin
  cos := Float32.cos
  tan := Float32.tan
  asin := Float32.asin
  acos := Float32.acos
  atan := Float32.atan
  sinh := Float32.sinh
  cosh := Float32.cosh
  tanh := Float32.tanh
  asinh := Float32.asinh
  acosh := Float32.acosh
  atanh := Float32.atanh

instance : Floating Float where
  exp := Float.exp
  log := Float.log
  sqrt := Float.sqrt
  sin := Float.sin
  cos := Float.cos
  tan := Float.tan
  asin := Float.asin
  acos := Float.acos
  atan := Float.atan
  sinh := Float.sinh
  cosh := Float.cosh
  tanh := Float.tanh
  asinh := Float.asinh
  acosh := Float.acosh
  atanh := Float.atanh

class RealFloat (a : Type u) extends RealFrac a, Floating a where
  /-- The radix of the representation (often 2) -/
  radix : a -> Nat
  /-- The number of digits of 'radix' in the significand -/
  digits : a -> Nat
  /-- The lowest and highest values the exponent may assume -/
  range : a -> Int × Int
  decode : a -> Int × Int
  encode : Int -> Int -> a
  exponent : a -> Int
  significand : a -> a
  scale : Int -> a -> a
  isNaN : a -> Bool
  isInfinite : a -> Bool
  isFinite : a -> Bool
  -- isDenormalized : a -> Bool
  isNegative : a -> Bool
  isNegativeZero : a -> Bool
  isIEEE : a -> Bool

def clamp : Int -> Int -> Int
| bd, k => Max.max (-bd) (Min.min bd k)

instance instRealFloatFloat32 : RealFloat Float32 where
  radix _ := Float32.radix
  digits _ := Float32.digits
  range _ := (Float32.minExp, Float32.maxExp)
  decode := Float32.decode
  encode := Float32.encode
  exponent x :=
    match Float32.decode x with
    | (m, n) => if m == 0 then 0 else n + Float32.digits
  significand x :=
    match Float32.decode x with
    | (m, _) => Float32.encode m (-Float32.digits)
  scale
  | 0, x => x
  | k, x =>
    if x == 0 ∨ Float32.isFinite x then
      x
    else
      match Float32.decode x with
      | (m, n) =>
        let bf := Float32.maxExp - Float32.minExp + 4 * Float32.digits
        Float32.encode m (n + clamp bf k)
  isNaN := Float32.isNaN
  isInfinite := Float32.isInf
  isFinite := Float32.isFinite
  -- isDenormalized
  isNegative := (. < 0.0)
  isNegativeZero := fun x => x == -0.0 ∧ x < 0.0
  isIEEE _ := true

instance instRealFloatFloat : RealFloat Float where
  radix _ := Float.radix
  digits _ := Float.digits
  range _ := (Float.minExp, Float.maxExp)
  decode := Float.decode
  encode := Float.encode
  exponent x :=
    match Float.decode x with
    | (m, n) => if m == 0 then 0 else n + Float.digits
  significand x :=
    match Float.decode x with
    | (m, _) => Float.encode m (-Float.digits)
  scale
  | 0, x => x
  | k, x =>
    if x == 0 ∨ Float.isFinite x then
      x
    else
      match Float.decode x with
      | (m, n) =>
        let bf := Float.maxExp - Float.minExp + 4 * Float.digits
        Float.encode m (n + clamp bf k)
  isNaN := Float.isNaN
  isInfinite := Float.isInf
  isFinite := Float.isFinite
  -- isDenormalized
  isNegative := (. < 0.0)
  isNegativeZero := fun x => x == -0.0 ∧ x < 0.0
  isIEEE _ := true

@[inline]
def expt {b e p: Type u} [HPow b e p] (base : b) (n : e) : p :=
  base ^ n

partial def floatToDigits {a : Type u} [RealFloat a] (base : Nat) (x : a) : List Int × Int :=
  if x == 0.0 then
    ([0], 0)
  else
    let (f0, e0) := RealFloat.decode x
    let (minExp0, _) := RealFloat.range x
    let p := RealFloat.digits x
    let b := RealFloat.radix x
    let minExp := minExp0 - p -- the real minimum exponent
    let (f, e) : Int × Int :=
      let n := minExp - e0
      if n > 0 then
        (f0 / (expt b n.toNat), e0 + n)
      else
        (f0, e0)
    let (r, s, mUp, mDn) :=
      if e >= 0 then
        let be := expt b e.toNat
        if expt b (p - 1) == f then
          (f * be * b * 2, 2 * b, be * b, be) -- according to Burger and Dybvig
        else
          (f * be * 2, 2, be, be)
      else if e > minExp ∧ expt b (p - 1) == f then
        (f * b * 2, expt b (-e + 1).toNat * 2, b, 1)
      else
        (f * 2, expt b (-e).toNat * 2, 1, 1)
    let k : Int :=
      let k0 : Int :=
        if b == 2 && base == 10 then
          -- logBase 10 2 is very slightly larger than 8651/28738
          -- (about 5.3558e-10), so if log x >= 0, the approximation
          -- k1 is too small, hence we add one and need one fixup step less.
          -- If log x < 0, the approximation errs rather on the high side.
          -- That is usually more than compensated for by ignoring the
          -- fractional part of logBase 2 x, but when x is a power of 1/2
          -- or slightly larger and the exponent is a multiple of the
          -- denominator of the rational approximation to logBase 10 2,
          -- k1 is larger than logBase 10 x. If k1 > 1 + logBase 10 x,
          -- we get a leading zero-digit we don't want.
          -- With the approximation 3/10, this happened for
          -- 0.5^1030, 0.5^1040, ..., 0.5^1070 and values close above.
          -- The approximation 8651/28738 guarantees k1 < 1 + logBase 10 x
          -- for IEEE-ish floating point types with exponent fields
          -- <= 17 bits and mantissae of several thousand bits, earlier
          -- convergents to logBase 10 2 would fail for long double.
          -- Using quot instead of div is a little faster and requires
          -- fewer fixup steps for negative lx.
          let lx := p - 1 + e0
          let k1 := (lx * 8651) / 28738
          if lx >= 0 then
            k1 + 1
          else
            k1
        else
          let f_plus_1 : a := OfInt.ofInt $ f + 1
          let e' : a := OfInt.ofInt e
          let b' : a := OfInt.ofInt b
          let base' : a := OfInt.ofInt $ Int.ofNat base
          @RealFrac.ceil a _ Int _ $ (Floating.log f_plus_1 + e' * Floating.log b') / Floating.log base'
      let rec fixup : Int -> Int := fun n =>
        if n >= 0 then
          if r + mUp <= expt base n.toNat * s then
            n
          else
            fixup (n + 1)
        else
          if expt base (-n).toNat * (r + mUp) <= s then
            n
          else
            fixup (n + 1)
      fixup k0
    let rec gen : List Int -> Int -> Int -> Int -> Int -> List Int := fun ds rn sN mUpN mDnN =>
      let (dn, rn') := divMod (rn * base) sN
      let mUpN' := mUpN * base
      let mDnN' := mDnN * base
      let go : Bool -> Bool -> List Int
      | true, false => dn :: ds
      | false, true => (dn + 1) :: ds
      | true, true =>
        if rn' * 2 < sN then
          dn :: ds
        else
          (dn + 1) :: ds
      | false, false => gen (dn :: ds) rn' sN mUpN' mDnN'
      go (rn' < mDnN') (rn' + mUpN' > sN)
    let rds :=
      if k >= 0 then
        gen [] r.toNat (s * expt base k.toNat) mUp mDn
      else
        let bk := expt base (-k).toNat
        gen [] (r.toNat * bk) s (mUp * bk) (mDn * bk)
    (rds.reverse, k)

def roundTo (base : Int) (d : Int) (is : List Int) : Int × List Int :=
  let b2 := base / 2
  let rec go : Int -> Bool -> List Int -> Int × List Int := fun
  | n, _, [] => (0, List.replicate n.toNat 0)
  | 0, e, x :: xs =>
    if x == b2 ∧ e ∧ xs.all (. == 0) then
      (0, [])
    else
      (if x >= b2 then 1 else 0, [])
  | n, _, i :: xs =>
    let (c, ds) := go (n - 1) (i % 2 == 0) xs
    let i' := c + i
    if i' == base then
      (1, 0 :: ds)
    else
      (0, i' :: ds)
  match go d true is with
  | (0, xs) => (0, xs)
  | (1, xs) => (1, 1 :: xs)
  | _ => panic "roundTo: bad value"

abbrev ShowS := String -> String

def showString : String -> ShowS :=
  String.append

partial def RealFloat.formatAlt {f : Type u} [RealFloat f] (alt : Bool) (fmt : FFFormat) (decs : Option Int) (x : f) : String :=
  let base := 10
  let rec doFmt : FFFormat -> List Int × Int -> String := fun fmt (is, e) =>
    let ds : List Char := is.map (natToDigit ∘ Int.toNat)
    match fmt with
    | .Generic => doFmt (if e < 0 ∨ e > 7 then .Exponent else .Fixed) (is, e)
    | .Exponent =>
      match decs with
      | .none =>
        let show_e' := s!"{e - 1}"
        match ds with
        | '0' :: [] => "0.0e0"
        | d :: [] => String.singleton d ++ ".0e" ++ show_e'
        | d :: ds' =>  (d :: '.' :: ds').asString ++ "e" ++ show_e'
        | [] => panic "formatRealFloat/doFmt/FFExponent: []"
      | .some dec =>
        if dec <= 0 then
          match is with
          | [0] => "0e0"
          | _ =>
            let (ei, is') := roundTo base 1 is
            let n := ((if ei > 0 then is'.dropLast else is').map (natToDigit ∘ Int.toNat))[0]!
            String.singleton n ++ s!"e{e - 1 + ei}"
        else
          let dec' := Max.max dec 1
          match is with
          | [0] => "0." ++ ("".pushn '0' $ dec'.toNat) ++ "e0"
          | _ =>
            let (ei, is') := roundTo base 1 is
            match (if ei > 0 then is'.dropLast else is').map (natToDigit ∘ Int.toNat) with
            | d :: ds' => (d :: '.' :: ds').asString ++ s!"e{e - 1 + ei}"
            | [] => panic "should not happen"
    | .Fixed =>
      let mk0 := fun ls =>
        match ls with
        | "" => "0"
        | _ => ls
      match decs with
      | .none =>
        if e <= 0 then
          "0." ++ ("".pushn '0' $ (-e).toNat) ++ ds.asString
        else
          let rec go := fun
            | 0, s, rs => mk0 (s.toList.reverse.asString) ++ "." ++ mk0 rs.asString
            | n, s, [] => go (n - 1) ("0" ++ s) []
            | n, s, r :: rs => go (n - 1) (String.singleton r ++ s) rs
          go e "" ds
      | .some dec =>
        let dec' := Max.max dec 0
        if e >= 0 then
          let (ei, is') := roundTo base (dec' + e) is
          let (ls, rs) := (is'.map (natToDigit ∘ Int.toNat)).splitAt (e + ei).toNat
          mk0 ls.asString ++ (if rs.isEmpty ∧ not alt then "" else "." ++ rs.asString)
        else
          let (ei, is') := roundTo base dec' (List.replicate (-e).toNat 0 ++ is)
          match (if ei > 0 then is'.dropLast else is').map (natToDigit ∘ Int.toNat) with
          | d :: ds' => String.singleton d ++ (if ds'.isEmpty ∧ not alt then "" else "." ++ ds'.asString)
          | [] => panic "should not happen"

  if RealFloat.isNaN x then
    "NaN"
  else if RealFloat.isInfinite x then
    if RealFloat.isNegative x then
      "-Infinity"
    else
      "Infinity"
  else if RealFloat.isNegative x ∨ RealFloat.isNegativeZero x then
    "-" ++ doFmt fmt (floatToDigits base (-x))
  else
    doFmt fmt (floatToDigits base x)

def RealFloat.format {f : Type u} [RealFloat f] : FFFormat -> Option Int -> f -> String :=
  RealFloat.formatAlt false

def showEFloat {f : Type u} [RealFloat f] : Option Int -> f -> ShowS :=
  (showString ∘ .) ∘ RealFloat.format .Exponent
def showFFloat {f : Type u} [RealFloat f] : Option Int -> f -> ShowS :=
  (showString ∘ .) ∘ RealFloat.format .Fixed
def showFFloatAlt {f : Type u} [RealFloat f] : Option Int -> f -> ShowS :=
  (showString ∘ .) ∘ RealFloat.formatAlt true .Fixed
def showGFloat {f : Type u} [RealFloat f] : Option Int -> f -> ShowS :=
  (showString ∘ .) ∘ RealFloat.format .Generic
def showGFloatAlt {f : Type u} [RealFloat f] : Option Int -> f -> ShowS :=
  (showString ∘ .) ∘ RealFloat.formatAlt true .Generic
