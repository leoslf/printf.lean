import Printf

open Printf

#eval printf "%s: %d + %d = %d\n" "Sum" 2 7 9
-- Output: "Sum: 2 + 7 = 9\n"
#eval printf "Pi ≈ %.3f" 3.1415926
-- Output: "Pi ≈ 3.142"

#eval printf "Finished in %1.4f seconds" (1932267.357000 - 1932267.356000 : Float)

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
  printf "%s" result;

  let result2 : String := printf "%1.4f" (1932267.357000 - 1932267.356000 : Float);
  dbgTrace result2 pure
  assert! result2 == "0.0010"
