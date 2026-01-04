# Examples

Sample Lyra projects demonstrating SystemVerilog simulation.

## Projects

| Project     | Description                                |
| ----------- | ------------------------------------------ |
| `hello/`    | Minimal example (variable init and finish) |
| `tiny_cpu/` | Comprehensive example with all SV features |

Each project contains a `lyra.toml` configuration file and source files.

## Running Examples

```bash
# Navigate to an example project
cd examples/hello

# Run simulation
lyra run

# Or build without running
lyra build

# Parse and validate only
lyra check

# Generate C++ project only
lyra emit
```

## Project Structure

Each example follows this structure:

```
hello/
├── lyra.toml       # Project configuration
├── hello.sv        # SystemVerilog source
└── out/            # Generated after build (gitignored)
    ├── build/sim   # Compiled binary
    └── ...
```

## Dump Command (Debug)

The `dump` command is for debugging and requires explicit file paths:

```bash
lyra dump cpp examples/hello/hello.sv   # Generated C++ code
lyra dump mir examples/hello/hello.sv   # MIR (high-level, structured)
lyra dump lir examples/hello/hello.sv   # LIR (low-level, linearized)
```

## Supported SystemVerilog Features

See `tiny_cpu/tiny_cpu.sv` for examples of:

- Data types: `bit`, `int`
- Time delays: `#5`, `#7`
- Clock generation: `forever` loops
- Always blocks: `always_comb`, `always_ff`
- Wait events: `@(posedge ...)`
- Arithmetic and conditionals
- Initial blocks with `$finish`
