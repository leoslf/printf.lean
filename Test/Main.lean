import Printf
import Test.Printf.Classes.FloatingSpec
import Test.Printf.MacroSpec

def main : IO Unit := do
  Test.Printf.Classes.FloatingSpec.spec
  Test.Printf.MacroSpec.spec
