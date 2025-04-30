# **Lyra: Rethinking SystemVerilog Simulation**

[![Build and Test](https://github.com/hankhsu1996/lyra/actions/workflows/build.yml/badge.svg?event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/build.yml)

**Lyra** is a clean, structured, and extensible SystemVerilog compiler and simulator.
It features a modern multi-stage compilation pipeline, enabling accurate semantic analysis, flexible intermediate representations, and native execution through interpretation or LLVM.

> ⚠️ **Development Status**: This project is under active development. Features are incomplete, APIs are unstable, and rapid changes should be expected.

## 🛠️ Build Instructions

Build the entire project:

```
bazel build //...
```

Run all tests:

```
bazel test //...
```

Generate compile commands for IDE integration:

```
bazel run @hedron_compile_commands//:refresh_all
```

## 📦 Project Structure

- `frontend/`: Wrapper for Slang SystemVerilog parser
- `mir/`: Middle-level IR preserving high-level language structure
- `lir/`: Low-level IR with SSA-style instructions for simulation
- `lowering/`: Transformations between AST, MIR, and LIR
- `core/`: Simulation environment (symbol tables, scheduler, event queue)
- `common/`: Shared utilities and data structures
- `runtime/`: Runtime support for simulation (typing, system functions)
- `simulation/`: Integration of the full compiler and simulator pipeline
- `codegen/`: LLVM code generation (planned)

## 🔍 Compilation Pipeline

```
SystemVerilog → AST → MIR → LIR → [ LLVM IR | Interpreter ] → Results
```

- **AST**: Generated using [Slang](https://github.com/MikePopoloski/slang)
- **MIR**: High-level, structure-preserving intermediate representation
- **LIR**: Linear SSA-style IR for simulation and codegen
- **Backend**: Interpreter (available) or LLVM native compilation (planned)

## ✅ Current Features

- SystemVerilog `module` support with variable declarations
- `initial` blocks and sequential execution
- Arithmetic operations (`+`) and assignments
- Signal management and runtime context
- LIR-based interpreter for simulation

## 🚧 Roadmap

- Event-driven scheduling system
- `always_ff` support with clock/event semantics
- Native binary generation via LLVM
- Broader SystemVerilog language coverage

## 💬 Get Involved

Got feedback or ideas? We're building Lyra to make SystemVerilog simulation **cleaner**, **clearer**, and **more scalable** — contributions are welcome!
