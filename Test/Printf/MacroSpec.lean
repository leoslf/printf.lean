import Printf

namespace Test.Printf.MacroSpec

def spec : IO Unit := do
  assert! dbgTraceVal (printf "%s: %d + %d = %d" "Sum" 2 7 9) = "Sum: 2 + 7 = 9"
  assert! dbgTraceVal (printf "Pi ≈ %.3f" 3.1415926) = "Pi ≈ 3.142"

  assert! dbgTraceVal (printf "Hello World!\n") = "Hello World!\n"
  assert! dbgTraceVal (printf "Hello %s!\n" "World") = "Hello World!\n"
  assert! dbgTraceVal (printf "%s %s!\n" "Hello" "World") = "Hello World!\n"
  assert! dbgTraceVal (printf "%02d" 0) = "00"
  assert! dbgTraceVal (printf "%0.2f" 0.0) = "0.00"
  assert! dbgTraceVal (printf "%e" 0.0) = "0.0e0"
  assert! dbgTraceVal (printf "%g" 0.0) = "0.0"
  assert! dbgTraceVal (printf "%hhd" (0 : Int8)) = "0"
