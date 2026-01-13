# Lyra CLI Design

Design document for the `lyra` command-line tool.

## Workflow

Lyra uses a project-based workflow with `lyra.toml` configuration files, similar to Cargo for Rust.

### Quick Start

```bash
# Create a new project
lyra init my_project
cd my_project

# Run simulation
lyra run

# Or build without running
lyra build
```

### Project Structure

```
my_project/
├── lyra.toml       # Project configuration
├── my_project.sv   # SystemVerilog source
└── out/            # Generated C++ project (after build)
    ├── build/sim   # Compiled binary
    └── ...
```

## Configuration (`lyra.toml`)

```toml
[package]
name = "my_design"
top = "Top"                     # Required: top module name

[sources]
files = ["src/top.sv"]          # Required: source files
incdir = ["include/"]           # Optional: include directories
defines = ["DEBUG", "WIDTH=32"] # Optional: preprocessor defines

[build]
out_dir = "out"                # Optional: default "out"

[diagnostics]
warnings = ["no-unused"]       # Optional: warning control
```

## Commands

| Command                  | Description                       |
| ------------------------ | --------------------------------- |
| `lyra run`               | Build and run simulation          |
| `lyra run -i`            | Run with interpreter backend      |
| `lyra build`             | Generate C++ and compile (no run) |
| `lyra emit`              | Generate C++ project only         |
| `lyra check`             | Parse and validate                |
| `lyra init <name>`       | Create new project                |
| `lyra dump <fmt> <file>` | Debug: dump IR (cpp, mir, lir)    |

Commands can use either `lyra.toml` or CLI arguments (or both).

### Global Options

| Option     | Description                           |
| ---------- | ------------------------------------- |
| `-C <dir>` | Run as if lyra was started in `<dir>` |

Example: `lyra -C path/to/project run`

### Project Command Options

The `run`, `build`, `emit`, and `check` commands accept these options:

| Option                    | Description                                  |
| ------------------------- | -------------------------------------------- |
| `--top <module>`          | Top module name                              |
| `-I, --include-directory` | Add include search path                      |
| `-D,--define-macro`       | Define preprocessor macro (e.g., `-DDEBUG`)  |
| `-W<warning>`             | Warning control (e.g., `-Wno-unused`)        |
| `-f <file>`               | Command file (paths relative to CWD)         |
| `-F <file>`               | Command file (paths relative to file itself) |
| `<files...>`              | Source files (positional)                    |

### CLI Mode

Commands work without `lyra.toml` when source files are provided:

```bash
# Check syntax without lyra.toml
lyra check --top Top file.sv

# Run simulation with interpreter
lyra run -i --top Testbench pkg.sv dut.sv tb.sv

# Include directories
lyra check --top Top -I include/ src/*.sv

# Define macros
lyra run --top Top -DDEBUG -DWIDTH=32 design.sv

# Command file with paths relative to CWD
lyra run --top Top -f sources.f

# Command file with paths relative to file itself (like slang -F)
lyra run --top Top -F lib/sources.f
```

### Command File Format

Command files (`.f` files) list source files and compilation options:

```
# Comment lines start with # or //
// This is also a comment

# Include directories
+incdir+vendor/ip/include
+incdir+../common/include

# Preprocessor defines
+define+DEBUG
+define+WIDTH=32

# Source files (one per line)
rtl/top.sv
rtl/submodule.sv
```

The `-f` flag resolves relative paths from the current working directory.
The `-F` flag resolves relative paths from the command file's directory.

### Precedence Rules

When both `lyra.toml` and CLI arguments are provided:

| Setting   | Behavior                             |
| --------- | ------------------------------------ |
| `--top`   | CLI overrides lyra.toml              |
| `-I`      | CLI merges with lyra.toml (additive) |
| `-D`      | CLI merges with lyra.toml (additive) |
| `-W`      | CLI merges with lyra.toml (additive) |
| `<files>` | CLI replaces lyra.toml files         |

### Output Structure (`lyra emit` -> `out/`)

```
out/
├── CMakeLists.txt        # Build configuration
├── CMakePresets.json     # Uses clang by default
├── main.cpp              # Entry point
├── include/
│   ├── design/
│   │   └── <module>.hpp  # Generated simulation code
│   └── lyra/sdk/         # SDK headers (copied)
```

Build the generated project manually:

```bash
cd out
cmake --preset default
cmake --build build
./build/sim
```

### Dump Command

Debug command to inspect internal representations:

```bash
lyra dump cpp file.sv   # Generated C++ code
lyra dump mir file.sv   # MIR (high-level, structured)
lyra dump lir file.sv   # LIR (low-level, linearized)
```

This is the only command that accepts file arguments directly.

## Backends

Two execution backends:

| Backend     | Path                             | Use Case                    |
| ----------- | -------------------------------- | --------------------------- |
| Interpreter | AST -> MIR -> LIR -> Interpreter | Development, debugging      |
| Codegen     | AST -> MIR -> C++ -> Binary      | Performance, production use |

Codegen is the default backend. Interpreter is useful for development (no compile step).

## Internal Architecture

### Library Structure

```
include/lyra/
├── config/                # Project configuration
│   └── project_config.hpp # lyra.toml parsing
│
├── interpreter/           # Interpreter backend
│   └── interpreter.hpp    # Entry point
│
├── compiler/              # Compiler backend
│   ├── compiler.hpp       # Entry point (codegen + compile + run)
│   └── codegen.hpp        # Generates C++ from MIR
│
└── sdk/                   # Runtime for generated code
    ├── sdk.hpp            # Umbrella header
    ├── module.hpp         # Base class for modules
    ├── scheduler.hpp      # Coroutine scheduler
    └── ...
```

### Key Classes

| Class           | Location       | Role                                 |
| --------------- | -------------- | ------------------------------------ |
| `ProjectConfig` | `config/`      | lyra.toml parsing                    |
| `Interpreter`   | `interpreter/` | MIR -> LIR -> run                    |
| `Compiler`      | `compiler/`    | MIR -> C++ -> compile -> run         |
| `Codegen`       | `compiler/`    | Generates C++ source from MIR        |
| `Module`        | `sdk/`         | Base class for generated modules     |
| `Scheduler`     | `sdk/`         | Coroutine-based simulation scheduler |

## Design Decisions

- **Default backend**: Codegen (production path)
- **C++ standard**: C++23 (for coroutines, `<print>`)
- **Preferred compiler**: Clang (better C++23 support than GCC)
- **Build system for output**: CMake with presets
- **Dual mode**: Commands work with lyra.toml, CLI arguments, or both
