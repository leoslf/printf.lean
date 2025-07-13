import Printf.Classes.PrintfArg

namespace Printf.Classes.HPrintfType

open Printf.Classes.PrintfArg

class HPrintfType t where
  hspr : IO.FS.Handle -> String -> List UPrintf -> t

instance : HPrintfType (IO Unit) where
  hspr handle fmt args :=
    handle.putStr $ uprintf fmt $ args.reverse

instance [PrintfArg a] [HPrintfType r] : HPrintfType (a -> r) where
  hspr handle fmt args := fun arg =>
    HPrintfType.hspr handle fmt $ PrintfArg.of arg :: args

