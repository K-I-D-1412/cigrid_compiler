# Cigrid Compiler

A robust, educational compiler for **Cigrid** (a subset of C), implemented in **OCaml**. This project compiles high-level C-like source code into **x86-64 assembly** (NASM syntax), featuring a complete frontend, semantic analysis system, and a backend with liveness analysis optimization.

> **Note**: This project was developed as part of the *Compilers and Execution Environments* course (ID2202) at KTH Royal Institute of Technology.
> 
> **Code of Honour**: If there are similar questions or labs or projects in the future, it is the responsibility of KTH students not to copy or modify these codes, or other files because it is against the [KTH EECS Code of Honour](https://www.kth.se/en/eecs/utbildning/hederskodex). The owner of this repository doesn't take any commitment for other's faults.

## Features

### Frontend
- **Lexical Analysis**: Tokenization handles keywords, identifiers, literals (int/char/string), and operators using `ocamllex`.
- **Syntax Analysis**: Context-free grammar parsing using `ocamlyacc` to generate an Abstract Syntax Tree (AST).
- **Semantic Analysis**:
  - **Scope Resolution**: Ensures variables are defined before use in local/global scopes.
  - **Type Checking**: Static type system supporting integers, chars, pointers, arrays, and structs.
  - **Safety Checks**: Validates return types, function signatures, and control flow integrity.

### Backend
- **Code Generation**: Compiles AST directly to **x86-64 assembly** (NASM).
- **Runtime Environment**:
  - **AMD64 ABI Compliance**: Implements standard calling conventions (first 6 args in registers, rest on stack) to support external C library calls (e.g., `printf`, `malloc`).
  - **Memory Management**: Supports dynamic memory allocation via `new`/`delete` (mapped to `malloc`/`free`).
  - **Stack Frame Management**: Handles local variable storage, parameter passing, and stack alignment (16-byte).
- **Optimization**:
  - **Liveness Analysis**: Implements data-flow analysis to compute live-in/live-out variable sets, visualizing register interference.

## Technical Challenges & Solutions

### 1. Implementing the AMD64 ABI
**Challenge**: To allow the Cigrid language to interact with the standard C library (like `printf`), the compiler must strictly adhere to the System V AMD64 ABI.
**Solution**:
- **Register Passing**: Implemented logic to pass the first six arguments via `RDI`, `RSI`, `RDX`, `RCX`, `R8`, `R9`.
- **Stack Alignment**: Ensured the stack pointer (`RSP`) is 16-byte aligned before any `call` instruction to avoid segmentation faults in external libraries.
- **Callee-Saved Registers**: Managed `RBP` preservation in function prologues and epilogues.

### 2. Variable Scope & Stack Layout
**Challenge**: Handling local variables, shadowing, and recursion efficiently.
**Solution**:
- Designed an **Environment** data structure passed through the recursive compilation steps.
- Maps variable names to stack offsets relative to `RBP` (e.g., `[rbp - 8]`).
- Differentiates between **Local** (stack-based) and **Global** (data section `.data` or `.bss` with `[rel label]`) access patterns.

### 3. Liveness Analysis
**Challenge**: Analyzing variable lifespan to support potential register allocation optimization.
**Solution**:
- Constructed a control-flow representation of the assembly instructions.
- Implemented a **Fixed-Point Iteration Algorithm** to calculate `LiveIn` and `LiveOut` sets based on `Use` and `Def` sets.
- Handles control flow branches (jumps/labels) to correctly propagate liveness data across basic blocks.

## Project Structure

```text
.
├── src/
│   ├── ast.ml          # Abstract Syntax Tree type definitions
│   ├── lexer.mll       # Lexer specification (ocamllex)
│   ├── parser.mly      # Grammar specification (ocamlyacc)
│   ├── semantic.ml     # Type checking and name analysis logic
│   ├── compile.ml      # Core compiler: AST -> x86-64 Assembly
│   ├── asm.ml          # x86 Instruction definitions
│   ├── liveness.ml     # Data-flow analysis implementation
│   └── main.ml         # Entry point and CLI argument parsing
├── tests/              # Cigrid source files for testing
├── Makefile            # Build configuration
└── README.md
