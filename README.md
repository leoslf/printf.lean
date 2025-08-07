# 📣 Lean 4 `printf` Implementation

A lightweight Lean 4 implementation of the C‑style `printf` format and output functionality — inspired by `printf`, `sprintf`, and `snprintf`.
This lets you format strings directly in Lean, without resorting to FFI.

## Features

- C-style format specifiers: `%d`, `%f`, `%s`, `%x`, etc.
- Variadic formatting interface in Lean 4
- Pure Lean (no libc or FFI dependencies)
- Extensible format specifier support

## Usage Examples

```lean
import Printf

open Printf

#eval printf "%s: %d + %d = %d\n" "Sum" 2 7 9
-- Output: Sum: 2 + 7 = 9

#eval printf "Pi ≈ %.3f" 3.1415926
-- Returns: "Pi ≈ 3.142"
```

