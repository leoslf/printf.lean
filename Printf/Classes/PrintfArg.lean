import Printf.Extensions

import Printf.Classes.IsChar
import Printf.Classes.Integral
import Printf.Classes.Bounded
import Printf.Classes.Floating

namespace Printf

universe u

def perror [Inhabited a] (message : String) : a :=
  panic s!"Printf.printf: {message}"

def errorBadFormat (c : Char) : String := s!"bad formatting char {c}"
def errorShortFormat : String := "formatting string ended prematurely"
def errorMissingArgument : String := "argument list ended prematurely"
def errorBadArgument : String := "bad argument"

def dfmt {f : Type u} [RealFloat f] (c : Char) (p : Option Int) (a : Bool) (d : f) : String × String :=
  let caseConvert := if c.isUpper then String.toUpper else id
  let showFunction : Option Int -> f -> String -> String :=
    match c.toLower with
    | 'e' => showEFloat
    | 'f' => if a then showFFloatAlt else showFFloat
    | 'g' => if a then showGFloatAlt else showGFloat
    | _ => perror "internal error: impossible dfmt"
  match (caseConvert $ showFunction p d "").toList with
  | '-' :: cs => ("-", cs.asString)
  | cs => ("", cs.asString)


inductive FormatAdjustment where
| LeftAdjust : FormatAdjustment
| ZeroPad : FormatAdjustment
deriving Repr, Inhabited, DecidableEq

inductive FormatSign where
| Plus : FormatSign
| Space : FormatSign
deriving Repr, Inhabited, DecidableEq

structure FieldFormat where
  mk ::
  width : Option Nat
  precision : Option Int
  adjust : Option FormatAdjustment
  sign : Option FormatSign
  alternate : Bool
  modifiers : String
  char : Char
deriving Repr, Inhabited, DecidableEq

abbrev FieldFormatter := FieldFormat -> ShowS

/-- Walks over argument-type-specific modifier characters to find primary format character. -/
structure FormatParse where
  mk ::
  /-- Modifiers found -/
  modifiers : String
  /-- Primary format character -/
  char : Char
  /-- Rest of the format string -/
  rest : String
deriving Repr, Inhabited, DecidableEq

abbrev ModifierParser := String -> FormatParse

def intModifierMap : List (String × Int) :=
  [
    ("hh", Int8.minValue.toInt),
    ("h", Int16.minValue.toInt),
    ("l", Int32.minValue.toInt),
    ("ll", Int64.minValue.toInt),
    ("L", Int64.minValue.toInt),
  ]

def parseIntFormat (_ : a) (s : String) : FormatParse :=
  let getFormat (p : String) :=
    let fp (cs : Substring) : Option FormatParse :=
      match cs.toString.toList with
      | c :: cs' => .some $ FormatParse.mk p c cs'.asString
      | [] => perror errorShortFormat
    (s.dropPrefix? p) >>= fp
  let matchPrefix : String × Int -> Option FormatParse -> Option FormatParse
  | (p, _), m@(.some { modifiers := p0, .. }) =>
    if p0.length >= p.length then
      m
    else
      match getFormat p with
      | .none => m
      | .some fp => .some fp
  | (p, _), .none => getFormat p
  match intModifierMap.foldr matchPrefix .none with
  | .some m => m
  | .none =>
    match s.toList with
    | c :: cs => { modifiers := "", char := c, rest := cs.asString }
    | [] => perror errorShortFormat

def vFmt (c : Char) (ufmt : FieldFormat) : FieldFormat :=
  match ufmt with
  | { char := 'v', .. } => { ufmt with char := c }
  | _ => ufmt

def adjust : FieldFormat -> String × String -> String
| ufmt, (pre, str) =>
  let naturalWidth := pre.length + str.length
  let zero : Bool :=
    match ufmt.adjust with
    | .some .ZeroPad => True
    | _ => False
  let left : Bool :=
    match ufmt.adjust with
    | .some .LeftAdjust => True
    | _ => False
  let fill : String :=
    match ufmt.width with
    | .some width =>
      if width > naturalWidth then
        let fillChar := if zero then '0' else ' '
        "".pushn fillChar (width - naturalWidth)
      else
        ""
    | _ => ""
  if left then
    pre ++ str ++ fill
  else if zero then
    pre ++ fill ++ str
  else
    fill ++ pre ++ str

def adjustSigned : FieldFormat -> String × String -> String
| ufmt@{ sign := .some .Plus, .. }, ("", str) => adjust ufmt ("+", str)
| ufmt@{ sign := .some .Space, .. }, ("", str) => adjust ufmt (" ", str)
| ufmt, ps => adjust ufmt ps

def integral_prec : Option Int -> String -> String
| .none, integral => integral
| .some 0, "0" => ""
| .some prec, integral => "".pushn '0' (prec.toNat - integral.length) ++ integral

def fmti : Option Int -> Int -> String × String
| prec, i =>
  if i < 0 then
    ("-", integral_prec prec s!"{-i}")
  else
    ("", integral_prec prec s!"{i}")

partial def showIntAtBase [OfNat a 0] [OfNat a 1] [Integral a] [DecidableLT a] [DecidableLE a] [DecidableEq a] : a -> (Int -> Char) -> a -> ShowS
| base, toChr, n0, r0 =>
  if base <= 1 then
    panic s!"showIntAtBase: unsupported base: {Integral.toInt base}"
  else if n0 < 0 then
    panic s!"showIntAtBase: applied to negative number: {Integral.toInt n0}"
  else
    let rec showIt : a × a -> ShowS
    | (n, d), r =>
      let r' := String.singleton (toChr $ fromIntegral d) ++ r
      if n == 0 then
        r'
      else
        showIt (divMod n base) r'
    showIt (divMod n0 base) r0

partial def fmtu : Int -> Option String -> Option Int -> Option Int -> Int -> String × String
| b, .some pre, prec, m, i =>
  let (_, s) := fmtu b .none prec m i
  match pre with
  | "0" =>
    match s.toList with
    | '0' :: _ => ("", s)
    | _ => (pre, s)
  | _ => (pre, s)
| b, .none, prec0, m0, i0 =>
  let rec fmtu' : Option Int -> Option Int -> Bool -> Int -> Option String
  | prec, .some m, true, i => fmtu' prec .none false $ -2 * m + i
  | .some prec, _, false, i => integral_prec (.some prec) <$> fmtu' .none .none false i
  | .none, _, false, i => .some $ showIntAtBase b (Nat.toDigit ∘ Int.toNat) i ""
  | _, _, _, _ => .none
  match fmtu' prec0 m0 (i0 < 0) i0 with
  | .some s => ("", s)
  | .none => perror errorBadArgument

def formatString [IsChar c] : List c -> FieldFormatter
| x, ufmt =>
  match (vFmt 's' ufmt).char with
  | 's' =>
    let trunc : Option Int -> List c
    | .none => x
    | .some n => x.take n.toNat
    let ts := (trunc ufmt.precision).map IsChar.toChar
    (adjust ufmt ("", ts.asString) ++ .)
  | c => perror $ errorBadFormat c

def formatIntegral : Option Int -> Int -> FieldFormatter
| m, x, ufmt0 =>
  let prec := ufmt0.precision
  let ufmt := vFmt 'd' $
    match ufmt0 with
    | { precision := .some _, adjust := .some .ZeroPad, .. } => { ufmt0 with adjust := .none }
    | _ => ufmt0
  let alt := fun
  | _, 0 => .none
  | p, _ => if ufmt.alternate then .some p else .none
  let upcase := fun (a, b) => (a, b.toUpper)
  match ufmt.char with
  | 'd' | 'i' => (adjustSigned ufmt (fmti prec x) ++ .)
  | 'x' => (adjust ufmt (fmtu 16 (alt "0x" x) prec m x) ++ .)
  | 'X' => (adjust ufmt (upcase $ fmtu 16 (alt "0X" x) prec m x) ++ .)
  | 'b' => (adjust ufmt (fmtu 2 (alt "0b" x) prec m x) ++ .)
  | 'o' => (adjust ufmt (fmtu 8 (alt "0" x) prec m x) ++ .)
  | 'u' => (adjust ufmt (fmtu 10 .none prec m x) ++ .)
  | 'c' =>
    if Char.isValidCharNat x.toNat ∧ ufmt.precision == .none ∧ ufmt.modifiers == "" then
      formatString [Char.ofNat x.toNat] { ufmt with char := 's' }
    else
      perror "illegal char conversion"
  | c => perror s!"bad formatting char: '{c}' for {x}"

def fixupMods (ufmt : FieldFormat) (m : Option Int) : Option Int :=
  match ufmt.modifiers with
  | "" => m
  | _ =>
    match intModifierMap.lookup ufmt.modifiers with
    | .some m0 => .some m0
    | .none => perror "unknown format modifier"

def formatBoundedInt [Integral a] [Bounded a] : a -> FieldFormat -> ShowS
| x, ufmt =>
  let minBound : a := Bounded.minBound
  let lb := Integral.toInt minBound
  let m := fixupMods ufmt $ .some lb
  let ufmt' :=
    match lb with
    | 0 => vFmt 'u' ufmt
    | _ => ufmt
  formatIntegral m (Integral.toInt x) ufmt'

def formatInt [Integral a] : a -> FieldFormat -> ShowS
| x, ufmt =>
  let m := fixupMods ufmt .none
  formatIntegral m (Integral.toInt x) ufmt

def formatChar : Char -> FieldFormat -> ShowS
| x, ufmt => formatIntegral (.some 0) (Integral.toInt $ x.toNat) $ vFmt 'c' ufmt

def formatRealFloat {f : Type u} [RealFloat f] : f -> FieldFormatter
| x, ufmt =>
  let c := (vFmt 'g' ufmt).char
  let prec := ufmt.precision
  let alt := ufmt.alternate
  match c with
  | 'e' | 'E' | 'f' | 'F' | 'g' | 'G' =>
    showString $ adjustSigned ufmt $ dfmt c prec alt x
  | _ =>
    perror $ errorBadFormat c

class PrintfArg a where
  formatArg : a -> FieldFormatter
  parseFormat : a -> ModifierParser := fun
  | _, format =>
    match format.toList with
    | c :: cs => { modifiers := "", char := c, rest := cs.asString }
    | [] => perror $ errorShortFormat

instance : PrintfArg Char where
  formatArg := formatChar
  parseFormat _ cf := parseIntFormat /- undefined -/ 0 cf

instance [IsChar c]: PrintfArg (List c) where
  formatArg := formatString

instance : PrintfArg String where
  formatArg := formatString ∘ String.toList

instance : PrintfArg Nat where
  formatArg := formatInt
  parseFormat := parseIntFormat

instance : PrintfArg Int where
  formatArg := formatInt
  parseFormat := parseIntFormat

instance [Integral a] [Bounded a] : PrintfArg a where
  formatArg := formatBoundedInt
  parseFormat := parseIntFormat

instance {a : Type u} [RealFloat a] : PrintfArg a where
  formatArg := @formatRealFloat a _

abbrev UPrintf := ModifierParser × FieldFormatter

def getStar : List UPrintf -> List UPrintf × Nat
| [] => perror errorMissingArgument
| (_, nu) :: us' =>
  let ufmt : FieldFormat := {
    width := .none,
    precision := .none
    adjust := .none
    sign := .none
    alternate := false
    modifiers := "",
    char := 'd',
  }
  (us', (nu ufmt "").toNat!)

def stoi (cs : String) : Int × String :=
  let (as, cs') := cs.toList.span Char.isDigit
  match as with
  | [] => (0, cs'.asString)
  | _ => (as.asString.toNat!, cs'.asString)

def adjustment (w : Option Int) (p : Option Int) (l : Bool) (z : Bool) : Option FormatAdjustment :=
  let adjl : Option Int -> Bool -> Bool -> Option FormatAdjustment := fun
    | _, true, _ => .some .LeftAdjust
    | _, false, true => .some .ZeroPad
    | _, _, _ => .none
  match w with
  | .some n =>
    if n < 0 then
      adjl p true z
    else
      adjl p l z
  | .none =>
    adjl p l z

partial def getSpecs : Bool -> Bool -> Option FormatSign -> Bool -> List Char -> List UPrintf -> FieldFormat × List Char × List UPrintf
| _, z, s, a, '-' :: cs0, us => getSpecs true z s a cs0 us
| l, z, _, a, '+' :: cs0, us => getSpecs l z (.some .Plus) a cs0 us
| l, z, s, a, ' ' :: cs0, us =>
  let ss :=
    match s with
    | .some .Plus => .some .Plus
    | _ => .some .Space
  getSpecs l z ss a cs0 us
| l, _, s, a, '0' :: cs0, us => getSpecs l true s a cs0 us
| l, z, s, _, '#' :: cs0, us => getSpecs l z s true cs0 us
| l, z, s, a, '*' :: cs0, us =>
  let (us', n) : List UPrintf × Nat := getStar us
  let ((p, cs''), us'') : (Option Int × String) × List UPrintf :=
    match cs0 with
    | '.' :: '*' :: r =>
      let (us''', p') := getStar us'
      ((.some $ Int.ofNat p', r.asString), us''')
    | '.' :: r =>
      let (p', r') := stoi r.asString
      ((.some p', r'), us')
    | _ =>
      ((.none, cs0.asString), us')
  let { modifiers := ms, char := c, rest := cs } :=
    match us'' with
    | (ufmt, _) :: _ => ufmt cs''
    | [] => perror errorMissingArgument
  (
    {
      width := .some $ Int.natAbs n,
      precision := p,
      adjust := adjustment (.some n) p l z,
      sign := s,
      alternate := a,
      modifiers := ms,
      char := c,
    },
    cs.toList,
    us''
  )
| l, z, s, a, '.' :: cs0, us =>
  let ((p, cs'), us') : (Int × String) × List UPrintf :=
    match cs0 with
    | '*' :: cs'' =>
      let (us'', p') := getStar us
      ((Int.ofNat p', cs''.asString), us'')
    | _ => (stoi cs0.asString, us)
  let { modifiers := ms, char := c, rest := cs } :=
    match us' with
    | (ufmt, _) :: _ => ufmt cs'
    | [] => perror errorMissingArgument
  (
    {
      width := .none,
      precision := .some p,
      adjust := adjustment .none (.some p) l z,
      sign := s,
      alternate := a,
      modifiers := ms,
      char := c,
    },
    cs.toList,
    us'
  )
| l, z, s, a, cs0@(c0 :: _), us =>
  if Char.isDigit c0 then
    let (n, cs') := stoi cs0.asString
    let ((p, cs''), us') : (Option Int × String) × List UPrintf :=
      match cs'.toList with
      | '.' :: '*' :: r =>
        let (us'', p') := getStar us
        ((.some p', r.asString), us'')
      | '.' :: r =>
        let (p', r') := stoi r.asString
        ((.some p', r'), us)
      | _ =>
        ((.none, cs'), us)
    let { modifiers := ms, char := c, rest := cs } :=
      match us' with
      | (ufmt, _) :: _ => ufmt cs''
      | _ => perror errorMissingArgument
    (
      {
        width := .some $ n.natAbs,
        precision := p,
        adjust := adjustment (.some n) p l z,
        sign := s,
        alternate := a,
        modifiers := ms,
        char := c,
      },
      cs.toList,
      us'
    )
  else
    let { modifiers := ms, char := c, rest := cs } :=
      match us with
      | (ufmt, _) :: _ => ufmt cs0.asString
      | _ => perror errorMissingArgument
    (
      {
        width := .none,
        precision := .none,
        adjust := adjustment .none .none l z,
        sign := s,
        alternate := a,
        modifiers := ms,
        char := c,
      },
      cs.toList,
      us
    )
| _, _, _, _, [], _ => perror errorShortFormat

partial def uprintfs : List Char -> List UPrintf -> ShowS
| [], [] => id
| [], _ :: _ => perror errorShortFormat
| '%' :: '%' :: cs, us => ("%" ++ .) ∘ uprintfs cs us
| '%' :: _, [] => perror errorMissingArgument
| '%' :: cs, us =>
  let fmt (cs0 : List Char) (us0 : List UPrintf) : ShowS :=
    match getSpecs false false .none false cs0 us0 with
    | (_, _, []) => perror errorMissingArgument
    | (ufmt, cs, (_, u) :: us) => u ufmt ∘ uprintfs cs us
  fmt cs us
| c :: cs, us => (String.singleton c ++ .) ∘ uprintfs cs us

def uprintf (format : String) (us : List UPrintf) : String :=
  uprintfs format.toList us ""

def PrintfArg.of [PrintfArg a] (arg : a) : UPrintf :=
  (PrintfArg.parseFormat arg, PrintfArg.formatArg arg)

