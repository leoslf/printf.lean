import Printf

open Printf

def main : IO Unit := do
  printf "Hello World!\n"
  printf "Hello %s!\n" "World"
  printf "%s %s!\n" "Hello" "World"
  printf "%02d\n" 0
  printf "%0.2f\n" 0.0
  printf "%e\n" 0.0
  printf "%g\n" 0.0
  let zero : Int8 := 0
  printf "%hhd\n" zero
  let result : String := printf "%s %s\n" "Hello" "World"
  printf "%s" result
