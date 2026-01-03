# Lyra: Rethinking SystemVerilog Simulation

[![Bazel Build and Test](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml/badge.svg?event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml)
[![Bazel File Lint](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-lint.yml/badge.svg?event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-lint.yml)
[![C++ Lint](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-lint.yml/badge.svg?event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-lint.yml)

**Lyra** is a SystemVerilog compiler that generates readable C++ for simulation.
It prioritizes fast iteration (compile + run + debug) over peak simulation speed.

> **Development Status**: This project is under active development. Features are incomplete, APIs are unstable, and rapid changes should be expected.

## Quick Start

```bash
# Build
bazel build //:lyra

# Run simulation
./bazel-bin/lyra run examples/tiny_cpu.sv

# Generate standalone C++ project
./bazel-bin/lyra emit examples/tiny_cpu.sv
cd out && cmake --preset default && cmake --build build && ./build/sim
```

## CLI Commands

```bash
lyra run <file.sv>                 # Compile and run (codegen)
lyra run --interpret <file.sv>     # Run with interpreter
lyra emit <file.sv>                # Generate buildable C++ project (to out/)
lyra emit --out-dir <dir> <file>   # Specify output directory
lyra check <file.sv>               # Parse and validate only
```

## Build Instructions

```bash
bazel build //...                              # Build all
bazel test //...                               # Run tests
bazel run @hedron_compile_commands//:refresh_all  # IDE integration
```

## Project Structure

- `frontend/`: Wrapper for Slang SystemVerilog parser
- `mir/`: Middle-level IR preserving high-level language structure
- `lir/`: Low-level IR with SSA-style instructions for simulation
- `lowering/`: Transformations between AST, MIR, and LIR
- `interpreter/`: LIR execution engine
- `compiler/`: C++ code generation and compilation
- `sdk/`: Runtime headers for generated code

## Compilation Pipeline

```
SystemVerilog → AST → MIR → LIR → Interpreter
                    ↘ MIR → C++ → Binary (codegen)
```

- **AST**: Generated using [Slang](https://github.com/MikePopoloski/slang)
- **MIR**: High-level, structure-preserving intermediate representation
- **LIR**: Linear SSA-style IR for simulation
- **Backends**: Interpreter (dev) and Codegen (production)

## Roadmap

- Complete SystemVerilog language coverage
- Performance optimizations for large-scale designs
- Native code generation via LLVM
- Multi-threading support for parallel simulation

## Get Involved

Got feedback or ideas? We're building Lyra to make SystemVerilog simulation **cleaner**, **clearer**, and **more scalable** — contributions are welcome!
