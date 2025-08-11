import Printf.Extensions

import Printf.Classes.Num
import Printf.Classes.IsChar
import Printf.Classes.Integral

open Printf

def Float32.radix  := 2 -- FLT_RADIX
def Float32.digits := 24 -- FLT_MANT_DIG
def Float32.minExp := -125 -- FLT_MIN_EXP
def Float32.maxExp := 128 -- FLT_MAX_EXP

def Float32.exponent (x : Float32) : Int :=
  Float32.frExp x |>.snd

def Float32.significand (x : Float32) : Float32 :=
  Float32.frExp x |>.fst

def Float32.scale (i : Int) (x : Float32) : Float32 :=
  x.scaleB i

def Float32.decode (x : Float32) : Int × Int := Id.run do
  if x == 0.0 then
    return (0, 0)

  let (m, e) := x.frExp -- x := m * 2 ^ e, where 0.5 <= |m| < 1.0
  let digits := Float32.digits - m.abs.log2.neg.ceil.toInt64.toInt + 1
  return (m.scaleB digits |>.toInt64.toInt, e - digits)

def Float32.encode : Int × Int -> Float32
| (m, e) =>
  (if m < 0 then neg else id) $ Float32.ofBinaryScientific m.natAbs e

def Float32.properFraction {b : Type u} [Integral b] (x : Float32) : b × Float32 :=
  match Float32.decode x with
  | (m, n) =>
    if n >= 0 then
      (OfInt.ofInt $ m * Float32.radix ^ n.toNat, 0.0)
    else
      match divMod m (Float32.radix ^ (-n).toNat) with
      | (w, r) => (OfInt.ofInt w, Float32.encode (r, n))

def Float32.truncate {b : Type u} [Integral b] (x : Float32) : b :=
  Float32.properFraction x |>.fst

def Float32.isNegativeZero (x : Float32) : Bool :=
  x.toBits = (-0.0 : Float32).toBits

def Float.radix := 2 -- FLT_RADIX
def Float.digits := 53 -- DBL_MANT_DIG
def Float.minExp := -1021 -- DBL_MIN_EXP
def Float.maxExp := 1024 -- DBL_MAX_EXP

def Float.significand (x : Float) : Float :=
  x.frExp.fst

def Float.exponent (x : Float) : Int :=
  x.frExp.snd

def Float.scale (i : Int) (x : Float) : Float :=
  x.scaleB i

def Float.decode (x : Float) : Int × Int := Id.run do
  if x == 0.0 then
    return (0, 0)

  let (m, e) := x.frExp -- x := m * 2 ^ e, where 0.5 <= |m| < 1.0
  let digits := Float.digits - m.abs.log2.neg.ceil.toInt64.toInt + 1
  return (m.scaleB digits |>.toInt64.toInt, e - digits)

def Float.encode : Int × Int -> Float
| (m, e) =>
  (if m < 0 then neg else id) $ Float.ofBinaryScientific m.natAbs e

def Float.properFraction {b : Type u} [Integral b] (x : Float) : b × Float :=
  match Float.decode x with
  | (m, n) =>
    if n >= 0 then
      (OfInt.ofInt $ m * Float.radix ^ n.toNat, 0.0)
    else
      match divMod m (Float.radix ^ (-n).toNat) with
      | (w, r) => (OfInt.ofInt w, Float.encode (r, n))

def Float.truncate {b : Type u} [Integral b] (x : Float) : b :=
  Float.properFraction x |>.fst

def Float.isNegativeZero (x : Float) : Bool :=
  x.toBits = (-0.0 : Float).toBits

namespace Printf

open _root_ (Float32 Float)

universe u

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
deriving Repr, DecidableEq, BEq, Inhabited, Nonempty, TypeName

class RealFrac (a : Type u) extends Real a, Fractional a where
  properFraction : {b : Type u} -> [Integral b] -> a -> b × a
  /-- truncate x returns the integer nearest x between zero and x -/
  truncate : {b : Type v} -> [Integral b] -> a -> b
  round : {b : Type v} -> [Integral b] -> a -> b
  ceil : {b : Type v} -> [Integral b] -> a -> b
  floor : {b : Type v} -> [Integral b] -> a -> b

instance : RealFrac Float32 where
  properFraction := Float32.properFraction
  truncate := Float32.truncate
  round := Float32.truncate ∘ Float32.round
  ceil := Float32.truncate ∘ Float32.ceil
  floor := Float32.truncate ∘ Float32.floor

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

class RealFloat (a : Type u) extends RealFrac a, Floating a, Repr a, ToString a where
  /-- The radix of the representation (often 2) -/
  radix : Nat
  /-- The number of digits of 'radix' in the significand -/
  digits : Nat
  /-- The lowest and highest values the exponent may assume -/
  range : Int × Int
  /--
  The function decodeFloat applied to a real floating-point number returns the significand expressed as an Integer and an appropriately scaled exponent (an Int).
  If decodeFloat x yields (m,n), then x is equal in value to m*b^n, where b is the floating-point radix, and furthermore, either m and n are both zero or else b^(d-1) <= abs m < b^d, where d is the value of RealFloat.digits x.
  In particular, decodeFloat 0 = (0,0). If the type contains a negative zero, also decodeFloat (-0.0) = (0,0)
  NOTE: The result of decodeFloat x is unspecified if either of isNaN x or isInfinite x is True.
  -/
  decode : a -> Int × Int

  /--
  encodeFloat performs the inverse of decodeFloat in the sense that for finite x with the exception of -0.0, uncurry encodeFloat (decodeFloat x) = x. encodeFloat m n is one of the two closest representable floating-point numbers to m*b^n (or ±Infinity if overflow occurs); usually the closer, but if m contains too many bits, the result may be rounded in the wrong direction.
  -/
  encode : Int × Int -> a

  exponent : a -> Int
  significand : a -> a
  scale : Int -> a -> a

  isNaN : a -> Bool
  isInfinite : a -> Bool
  isFinite : a -> Bool
  -- isDenormalized : a -> Bool
  isNegative : a -> Bool
  isNegativeZero : a -> Bool
  isIEEE : Bool

def clamp : Int -> Int -> Int
| bd, k => Max.max (-bd) (Min.min bd k)

instance instRealFloatFloat32 : RealFloat Float32 where
  radix := Float32.radix
  digits := Float32.digits
  range := (Float32.minExp, Float32.maxExp)
  decode := Float32.decode
  encode := Float32.encode
  exponent := Float32.exponent
  significand := Float32.significand
  scale := Float32.scale
  isNaN := Float32.isNaN
  isInfinite := Float32.isInf
  isFinite := Float32.isFinite
  -- isDenormalized
  isNegative := (. < 0.0)
  isNegativeZero := Float32.isNegativeZero
  isIEEE := true

instance instRealFloatFloat : RealFloat Float where
  radix := Float.radix
  digits := Float.digits
  range := (Float.minExp, Float.maxExp)
  decode := Float.decode
  encode := Float.encode
  exponent := Float.exponent
  significand := Float.significand
  scale := Float.scale
  isNaN := Float.isNaN
  isInfinite := Float.isInf
  isFinite := Float.isFinite
  -- isDenormalized
  isNegative := (. < 0.0)
  isNegativeZero := Float.isNegativeZero
  isIEEE := true

@[inline]
def expt {b e p: Type u} [HPow b e p] (base : b) (n : e) : p :=
  base ^ n

/--
Based on "Printing Floating-Point Numbers Quickly and Accurately"
by R.G. Burger and R.K. Dybvig in PLDI 96.
This version uses a much slower logarithm estimator. It should be improved.

'floatToDigits' takes a base and a non-negative 'RealFloat' number, and returns a list of digits and an exponent.

In particular, if x >= 0, and floatToDigits base x = ([d1,d2,...,dn], e), then
  (1) @n >= 1@

  (2) @x = 0.d1d2...dn * (base**e)@

  (3) @0 <= di <= base-1@
-/
partial def RealFloat.toDigits {a : Type u} [RealFloat a] (base : Nat) (x : a) : List Int × Int := Id.run do
  if x == 0.0 then
    return ([0], 0)

  let (f0, e0) := RealFloat.decode x
  let (minExp0, _) := RealFloat.range a
  let p := RealFloat.digits a
  let b := RealFloat.radix a
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
    else if e > minExp && expt b (p - 1) == f then
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

    let rec fixup (n : Int) : Int :=
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

  let rec gen (ds : List Int) (rn : Int) (sN : Int) (mUpN : Int) (mDnN : Int) : List Int :=
    let (dn, rn') := divMod (rn * base) sN
    let mUpN' := mUpN * base
    let mDnN' := mDnN * base

    match (rn' < mDnN' : Bool), (rn' + mUpN' > sN : Bool) with
    | true, false => dn :: ds
    | false, true => (dn + 1) :: ds
    | true, true =>
      if rn' * 2 < sN then
        dn :: ds
      else
        (dn + 1) :: ds
    | false, false => gen (dn :: ds) rn' sN mUpN' mDnN'

  let rds :=
    if k >= 0 then
      gen [] r.toNat (s * expt base k.toNat) mUp mDn
    else
      let bk := expt base (-k).toNat
      gen [] (r.toNat * bk) s (mUp * bk) (mDn * bk)
  return (rds.reverse, k)

def roundTo (base : Int) (d : Int) (is : List Int) : Int × List Int :=
  let b2 := base / 2

  let rec go : Int -> Bool -> List Int -> Int × List Int
    | n, _, [] => (0, List.replicate n.toNat 0)
    | 0, e, x :: xs =>
      if x == b2 && e && xs.all (. == 0) then
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

open String (replicate) in
partial def RealFloat.format {f : Type u} [RealFloat f] (fmt : FFFormat) (dec? : Option Int) (self : f) (alt : Bool := false) : String :=
  let rec doFmt (fmt : FFFormat) : List Int × Int -> String
  | (is, e) => Id.run do
    let ds := is.map Int.toDigit |>.asString
    match fmt with
    | .Generic => doFmt (if e < 0 || e > 7 then .Exponent else .Fixed) (is, e)
    | .Exponent =>
      match dec? with
      | .none =>
        match ds with
        | ⟨'0' :: []⟩ => return "0.0e0"
        | ⟨d :: []⟩ => return s!"{d}.0e{e - 1}"
        | ⟨d :: ds'⟩ => return (d :: '.' :: ds').asString ++ s!"e{e - 1}"
        | ⟨[]⟩ => panic "RealFloat.format/doFmt/Exponent: []"
      | .some dec =>
        if dec <= 0 then
          if let [0] := is then
            return "0e0"

          let (ei, is') := roundTo base 1 is
          let n := ((if ei > 0 then is'.dropLast else is').map (Nat.toDigit ∘ Int.toNat))[0]!
          return s!"{n}e{e - 1 + ei}"
        else
          let dec' := Max.max dec 1 |>.toNat
          if let [0] := is then
            return s!"0.{replicate dec' '0'}e0"

          let (ei, is') := roundTo base (dec' + 1) is
          let (d :: ds') := (if ei > 0 then is'.dropLast else is').map Int.toDigit
            | unreachable!
          return (d :: '.' :: ds').asString ++ s!"e{e - 1 + ei}"
    | .Fixed =>
      match dec? with
      | .none =>
        if e <= 0 then
          return s!"0.{replicate (-e).toNat '0'}{ds}"
        else
          let rec f : Nat -> String -> ShowS
            | 0, s, rs => mk0 s.reverse ++ "." ++ mk0 rs
            | n, s, ⟨[]⟩ => f (n - 1) ("0" ++ s) ""
            | n, s, ⟨r :: rs⟩ => f (n - 1) ⟨r :: s.data⟩ ⟨rs⟩
          return f e.toNat "" ds
      | .some dec =>
        let dec' := Max.max dec 0
        if e >= 0 then
          let (ei, is') := roundTo base (dec' + e) is
          let (ls, rs) := is'.map Int.toDigit |>.asString.splitAt (e + ei).toNat
          return mk0 ls ++ (if rs.isEmpty && not alt then "" else "." ++ rs)
        else
          let (ei, is') := roundTo base dec' (List.replicate (-e).toNat 0 ++ is)
          let (d :: ds') := (if ei > 0 then is' else (0 :: is')).map Int.toDigit
            | unreachable!
          return ⟨d :: (if ds'.isEmpty && not alt then [] else ('.' :: ds'))⟩

  if isNaN self then
    "NaN"
  else if isInfinite self then
    if isNegative self then
      "-Infinity"
    else
      "Infinity"
  else if isNegative self || isNegativeZero self then
    "-" ++ doFmt fmt (toDigits base (-self))
  else
    doFmt fmt (toDigits base self)
 where
  base : Nat := 10

  mk0 : String -> String
  | "" => "0"
  | ls => ls

def showEFloat {f : Type u} [RealFloat f] : Option Int -> f -> ShowS :=
  (showString ∘ .) ∘ RealFloat.format (alt := false) .Exponent
def showFFloat {f : Type u} [RealFloat f] (alt : Bool := false) : Option Int -> f -> ShowS :=
  (showString ∘ .) ∘ RealFloat.format .Fixed (alt := alt)
def showGFloat {f : Type u} [RealFloat f] (alt : Bool := false) : Option Int -> f -> ShowS :=
  (showString ∘ .) ∘ RealFloat.format .Generic (alt := alt)
