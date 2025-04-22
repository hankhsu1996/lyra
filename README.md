# Volans: A Modern SystemVerilog Simulator

Volans is an event-driven SystemVerilog simulator that compiles directly to native code using LLVM, with no intermediate artifacts.

## Features

- Compiles SystemVerilog source directly to standalone binary
- Event-driven simulation with accurate process scheduling
- Uses Slang for parsing and elaboration
- Uses LLVM as backend for optimized native code generation

## Build Instructions

Build the entire project:
```bash
bazel build //...
```

Generate compile commands for IDE integration:
```bash
bazel run @hedron_compile_commands//:refresh_all
```

## Current Support

Initial version supports:
- `int`, `bit`, and `string` types
- `initial` blocks
- Basic expressions and assignments
