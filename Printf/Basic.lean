import Printf.Classes.PrintfArg
import Printf.Classes.PrintfType
import Printf.Classes.HPrintfType

namespace Printf

open Printf.Classes.PrintfArg
open Printf.Classes.PrintfType
open Printf.Classes.HPrintfType

open Lean Macro

-- -- NOTE: seems varargs via typeclass polymorphism lean4 does not work
-- def printf [PrintfType r] (format : String) : r :=
--   PrintfType.spr format []

syntax (name := printf_macro) "printf " term:arg (term:arg)* : term

macro_rules
| `(printf $format:term $[$args:term]*) => do
  let mut arguments <- `([])
  for arg in args do
    arguments <- `((PrintfArg.of $arg) :: $arguments)
  `(PrintfType.spr $format $arguments)


-- -- NOTE: seems varargs via typeclass polymorphism lean4 does not work
-- def hPrintf [HPrintfType r] (handle : IO.FS.Handle) (format : String) : r :=
--   HPrintfType.hspr handle format []

syntax (name := hPrintf_macro) "hPrintf " term:arg term:arg (ppSpace term:arg)* : term

macro_rules
| `(hPrintf $handle:term $format:term $[$args:term]*) => do
  let mut arguments <- `([])
  for arg in args do
    arguments <- `((PrintfArg.of $arg) :: $arguments)
  `(HPrintfType.hspr $handle $format $arguments)

