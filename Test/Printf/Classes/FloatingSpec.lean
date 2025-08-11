import Printf.Classes.Floating
import Printf

open Printf

namespace Test.Printf.Classes.FloatingSpec

section Float32

open Float32

def test_Float32_isNegativeZero : IO Unit := do
  assert! isNegativeZero (-0.0) = true
  assert! isNegativeZero 0.0 = false

def test_Float32_decode : IO Unit := do
  assert! decode 0.0 == (0, 0)
  assert! decode (-0.0) == (0, 0)
  assert! decode 0.0010 == (8589935, -33)
  assert! decode (0.357000 - 0.356000) == (8589824, -33)

/-- encode (decode x)) = x -/
def test_Float32_encode : IO Unit := do
  for x in [0.0, -0.0, 0.0010, 0.357000 - 0.356000] do
    assert! encode (decode x) == x

def test_Float32 : IO Unit := do
  test_Float32_isNegativeZero
  test_Float32_decode
  test_Float32_encode

end Float32

section Float

open Float

def test_Float_isNegativeZero : IO Unit := do
  assert! isNegativeZero (-0.0) = true
  assert! isNegativeZero 0.0 = false

def test_Float_decode : IO Unit := do
  assert! decode 0.0 == (0, 0)
  assert! decode (-0.0) == (0, 0)
  assert! decode 0.0010 == (4611686018427388, -62)
  assert! decode (1932267.357000 - 1932267.356000) == (4611686774341632, -62)

/-- encode (decode x)) = x -/
def test_Float_encode : IO Unit := do
  for x in [0.0, -0.0, 0.0010, 1932267.357000 - 1932267.356000] do
    assert! encode (decode x) == x

def test_Float : IO Unit := do
  test_Float_isNegativeZero
  test_Float_decode
  test_Float_encode

end Float

section RealFloat

open RealFloat

def test_RealFloat_toDigits : IO Unit := do
  assert! toDigits (base := 10) 0.0 = ([0], 0)
  assert! toDigits (base := 10) 0.0010 = ([1], -2)
  assert! toDigits (base := 10) (1932267.357000 - 1932267.356000 : Float) = ([1, 0, 0, 0, 0, 0, 0, 1, 6, 3, 9, 1, 2, 7, 7, 3, 1], -2)

def test_RealFloat_format : IO Unit := do
  assert! format .Fixed (1.0 / 0.0) (dec? := .none) = "Infinity"
  assert! format .Fixed (-1.0 / 0.0) (dec? := .none) = "-Infinity"
  assert! format .Fixed (0.0 / 0.0) (dec? := .none) = "NaN"
  assert! format .Fixed (0.0) (dec? := .none) = "0.0"
  assert! format .Fixed (-0.0) (dec? := .none) = "-0.0"
  assert! format .Fixed 0.0010 (dec? := .some 4) = "0.0010"
  assert! format .Fixed (1932267.357000 - 1932267.356000) (dec? := .some 4) = "0.0010"

def test_RealFloat : IO Unit := do
  test_RealFloat_toDigits
  test_RealFloat_format

end RealFloat

def test_roundTo : IO Unit := do
  let dec' := 2
  let e := 0
  let is := [0]
  assert! roundTo 10 (dec' + e) is = (0, [0, 0])

def spec : IO Unit := do
  test_Float32
  test_Float
  test_RealFloat
  test_roundTo

