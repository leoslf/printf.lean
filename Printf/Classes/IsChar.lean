namespace Printf

class IsChar (c : Type u) where
  toChar : c -> Char
  fromChar : Char -> c

instance : IsChar Char where
  toChar := id
  fromChar := id

