# tinyc - A Small C Compiler

`tinyc` is a small C compiler written in Rust. It uses LLVM 21 for code generation and optimization.

## Project Structure

- **`src/lexer`** — Lexical analysis: breaks source code into tokens.
- **`src/parser`** — Parsing: constructs an AST from tokens.
- **`src/codegen`** — LLVM IR code generation, optimization, and AOT compilation.

## Current Status

The compiler supports:
- **Lexer** — Keywords, identifiers, integer/float/bool/char literals, all standard operators, and accurate line/column tracking.
- **Parser** — Recursive-descent parser with correct operator precedence. Supports variable/function declarations, if/else, while loops, return, blocks, and nested expressions.
- **Codegen** — Full LLVM IR generation for all AST nodes including arithmetic, comparisons, assignments, if/else, while loops, and function definitions.
- **Optimization** — Runs the LLVM O3 pass pipeline (mem2reg, inlining, GVN, instcombine, loop optimizations, etc.).
- **AOT Compilation** — Compiles source to a native executable via LLVM object file emission and system linker.

## Building

Requires LLVM 21 (`llvm21` on Arch, or set `LLVM_SYS_211_PREFIX`).

```bash
cargo build
```

## Usage

```bash
# Compile to executable (output name = input without extension)
cargo run -- program.tinyc

# Compile to a specific output path
cargo run -- program.tinyc -o myprogram

# Dump unoptimized LLVM IR to stderr
cargo run -- program.tinyc --emit-ir
```

## Testing

```bash
cargo test
```

## Example

```c
int main() {
    int sum = 0;
    int i = 0;
    while (i < 10) {
        sum = sum + i;
        i = i + 1;
    }
    return sum;
}
```
