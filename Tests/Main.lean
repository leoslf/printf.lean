import Printf

open Printf

#eval printf "%s: %d + %d = %d\n" "Sum" 2 7 9
-- Output: "Sum: 2 + 7 = 9\n"
#eval printf "Pi ≈ %.3f" 3.1415926
-- Output: "Pi ≈ 3.142"

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
