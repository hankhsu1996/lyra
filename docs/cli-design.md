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
top = "Top"                    # Required: top module name

[sources]
files = ["src/top.sv"]         # Required: source files
incdir = ["include/"]          # Optional: include directories

[build]
out_dir = "out"                # Optional: default "out"
```

## Commands

| Command                  | Description                                   |
| ------------------------ | --------------------------------------------- |
| `lyra run`               | Build and run simulation (requires lyra.toml) |
| `lyra run -i`            | Run with interpreter backend                  |
| `lyra build`             | Generate C++ and compile (no run)             |
| `lyra emit`              | Generate C++ project only                     |
| `lyra check`             | Parse and validate                            |
| `lyra init <name>`       | Create new project                            |
| `lyra dump <fmt> <file>` | Debug: dump IR (cpp, mir, lir)                |

All commands except `dump` require a `lyra.toml` file. The CLI searches the current directory and parent directories for the config file.

### Output Structure (`lyra emit` → `out/`)

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

| Backend     | Path                          | Use Case                    |
| ----------- | ----------------------------- | --------------------------- |
| Interpreter | AST → MIR → LIR → Interpreter | Development, debugging      |
| Codegen     | AST → MIR → C++ → Binary      | Performance, production use |

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
| `Interpreter`   | `interpreter/` | MIR → LIR → run                      |
| `Compiler`      | `compiler/`    | MIR → C++ → compile → run            |
| `Codegen`       | `compiler/`    | Generates C++ source from MIR        |
| `Module`        | `sdk/`         | Base class for generated modules     |
| `Scheduler`     | `sdk/`         | Coroutine-based simulation scheduler |

## Design Decisions

- **Default backend**: Codegen (production path)
- **C++ standard**: C++23 (for coroutines, `<print>`)
- **Preferred compiler**: Clang (better C++23 support than GCC)
- **Build system for output**: CMake with presets
- **Config-first**: Commands use lyra.toml by default (no dual mode)
