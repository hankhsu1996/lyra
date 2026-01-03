# Examples

Sample SystemVerilog files for testing Lyra.

## Files

| File          | Description                                |
| ------------- | ------------------------------------------ |
| `hello.sv`    | Minimal example (variable init and finish) |
| `tiny_cpu.sv` | Comprehensive example with all SV features |

## Running Examples

### Using Bazel

```bash
# Build and run with codegen (default)
bazel run //:lyra -- run examples/tiny_cpu.sv

# Run with interpreter
bazel run //:lyra -- run --interpret examples/tiny_cpu.sv

# Generate standalone C++ project (to out/)
bazel run //:lyra -- emit examples/tiny_cpu.sv

# Parse and validate only
bazel run //:lyra -- check examples/tiny_cpu.sv
```

### Using the Binary Directly

After building:

```bash
bazel build //:lyra

./bazel-bin/lyra run examples/tiny_cpu.sv
./bazel-bin/lyra emit examples/tiny_cpu.sv
./bazel-bin/lyra check examples/tiny_cpu.sv
```

## Generating a Standalone C++ Project

`lyra emit` generates a complete, buildable C++ project to `out/` by default:

```bash
lyra emit examples/tiny_cpu.sv              # Creates out/
lyra emit --out-dir gen examples/tiny_cpu.sv  # Creates gen/
```

This creates:

```
out/
├── CMakeLists.txt        # Build configuration
├── CMakePresets.json     # Uses clang by default
├── main.cpp              # Entry point
├── include/
│   ├── design/
│   │   └── tiny_cpu.hpp  # Generated simulation code
│   └── lyra/sdk/         # SDK headers
```

Build and run:

```bash
cd out
cmake --preset default
cmake --build build
./build/sim
```

## Supported SystemVerilog Features

See `tiny_cpu.sv` for examples of:

- Data types: `bit`, `int`
- Time delays: `#5`, `#7`
- Clock generation: `forever` loops
- Always blocks: `always_comb`, `always_ff`
- Wait events: `@(posedge ...)`
- Arithmetic and conditionals
- Initial blocks with `$finish`
