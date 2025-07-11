# **Lyra: Rethinking SystemVerilog Simulation**

[![Bazel Build and Test](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml/badge.svg?event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml)
[![Bazel File Lint](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-lint.yml/badge.svg?event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-lint.yml)
[![C++ Lint](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-lint.yml/badge.svg?event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-lint.yml)

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
- `common/`: Shared utilities and data structures
- `interpreter/`: Execution engine for running LIR instructions
- `driver/`: Integration of the full compiler and simulator pipeline

## 🔍 Compilation Pipeline

```
SystemVerilog → AST → MIR → LIR → [ LLVM IR | Interpreter ] → Results
```

- **AST**: Generated using [Slang](https://github.com/MikePopoloski/slang)
- **MIR**: High-level, structure-preserving intermediate representation
- **LIR**: Linear SSA-style IR for simulation and codegen
- **Backend**: Interpreter execution engine (current) with LLVM compilation planned

## 🚧 Roadmap

- Complete SystemVerilog language coverage
- Performance optimizations for large-scale designs
- Native code generation via LLVM
- Multi-threading support for parallel simulation
- Advanced debugging and visualization tools

## 💬 Get Involved

Got feedback or ideas? We're building Lyra to make SystemVerilog simulation **cleaner**, **clearer**, and **more scalable** — contributions are welcome!
