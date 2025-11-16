# tinyc - A Small C Compiler

`tinyc` is a small C compiler written in Rust. The goal of this project is to build a complete compiler toolchain, starting from lexical analysis and parsing, and eventually incorporating advanced optimizations and code generation using LLVM and potentially MLIR.

## Project Structure

The project is organized into the following main components:

-   **`src/lexer`**: Handles the lexical analysis phase, breaking down source code into tokens.
-   **`src/parser`**: Implements the parsing phase, constructing an Abstract Syntax Tree (AST) from the tokens.
-   **`src/codegen`**: (Future) Will contain the code generation logic.

## Current Status

Currently, the project includes a functional lexer and parser capable of processing a subset of the C language.

## Future Plans

The roadmap for `tinyc` includes:

-   **LLVM Code Generation**: Generating LLVM Intermediate Representation (IR) from the AST.
-   **LLVM Passes**: Utilizing LLVM's optimization passes to improve the generated code.
-   **LLVM Execution Engine**: Integrating an LLVM execution engine for Just-In-Time (JIT) compilation and execution.
-   **Ahead-of-Time (AOT) Compilation**: Implementing functionality to compile `tinyc` programs into standalone executables.
-   **MLIR Integration**: Exploring the use of MLIR (Multi-Level IR) for advanced compiler optimizations, especially for domain-specific or hardware-specific optimizations.

## Building and Running

To build the `tinyc` compiler:

```bash
cargo build
```

To run the compiler (e.g., with a test file):

```bash
cargo run -- <input_file.c>
```

To run the tests:

```bash
cargo test
```
