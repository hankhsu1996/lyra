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
- `runtime/`: Simulation environment (symbol tables, scheduler, event queue) and runtime support
- `common/`: Shared utilities and data structures
- `interpreter/`: Execution engine for running LIR instructions
- `driver/`: Integration of the full compiler and simulator pipeline

## 🔍 Compilation Pipeline

```
SystemVerilog → AST → MIR → LIR → Interpreter → Results
```

- **AST**: Generated using [Slang](https://github.com/MikePopoloski/slang)
- **MIR**: High-level, structure-preserving intermediate representation
- **LIR**: Linear SSA-style IR for simulation and codegen
- **Backend**: Interpreter execution engine


## 🚧 Roadmap

- Complete SystemVerilog language coverage
- Performance optimizations for large-scale designs
- Native code generation capabilities
- Multi-threading support for parallel simulation
- Advanced debugging and visualization tools

## 💬 Get Involved

Got feedback or ideas? We're building Lyra to make SystemVerilog simulation **cleaner**, **clearer**, and **more scalable** — contributions are welcome!
