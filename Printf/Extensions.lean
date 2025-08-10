def Nat.toDigit (n : Nat) : Char :=
  if n < 10 then
    Char.ofNat $ '0'.toNat + n
  else if n < 16 then
    Char.ofNat $ 'a'.toNat + (n - 10)
  else
    panic $ s!"Nat.toDigit: '{n}' is not a digit"

def Int.toDigit (n : Int) : Char :=
  n.toNat.toDigit

def String.with (f : List Char -> List Char) : String -> String :=
  List.asString ∘ f ∘ String.toList

def String.reverse (self : String) : String :=
  self.with $ List.reverse

def String.replicate (n : Nat) (c : Char) : String :=
  List.replicate n c |>.asString
