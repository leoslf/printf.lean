import Data.Char
import Printf.Classes.PrintfArg

namespace Printf.Classes.PrintfType

open Data.Char
open Printf.Classes.PrintfArg

class PrintfType t where
  spr : String -> List UPrintf -> t

instance : PrintfType (IO Unit) where
  spr fmt args :=
    IO.print $ uprintf fmt $ args.reverse

@[default_instance]
instance : PrintfType String where
  spr fmt args := uprintf fmt $ args.reverse

instance [IsChar c] : PrintfType (List c) where
  spr fmt args :=
    let result : String := PrintfType.spr fmt args
    result.toList.map IsChar.fromChar

instance [PrintfArg a] [PrintfType r] : PrintfType (a -> r) where
  spr fmt args := fun arg =>
    PrintfType.spr fmt $ PrintfArg.of arg :: args

