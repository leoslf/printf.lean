namespace Data.Char

class IsChar (c : Type u) where
  toChar : c -> Char
  fromChar : Char -> c

instance : IsChar Char where
  toChar := id
  fromChar := id

def natToDigit (n : Nat) : Char :=
  if n < 10 then
    Char.ofNat $ '0'.toNat + n
  else if n < 16 then
    Char.ofNat $ 'a'.toNat + (n - 10)
  else
    panic $ s!"natToDigit: '{n}' is not a digit"
