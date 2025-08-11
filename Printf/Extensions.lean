def Nat.toDigit (n : Nat) : Char :=
  if n < 10 then
    Char.ofNat $ '0'.toNat + n
  else if n < 16 then
    Char.ofNat $ 'a'.toNat + (n - 10)
  else
    panic $ s!"Nat.toDigit: '{n}' is not a digit"

def Int.toDigit (n : Int) : Char :=
  n.toNat.toDigit

