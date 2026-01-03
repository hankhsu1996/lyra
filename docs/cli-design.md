# Lyra CLI Design

Design document for the `lyra` command-line tool.

## Current Implementation

### Subcommands

| Command                | Status | Description                    |
| ---------------------- | ------ | ------------------------------ |
| `lyra run <file.sv>`   | Done   | Compile and run (codegen)      |
| `lyra run --interpret` | Done   | Run with interpreter           |
| `lyra emit <file.sv>`  | Done   | Emit C++ to stdout             |
| `lyra emit -o <dir>`   | Done   | Generate buildable C++ project |
| `lyra check <file.sv>` | Done   | Parse and validate only        |
| `lyra build`           | —      | Generate binary only (no run)  |
| `lyra init`            | —      | Initialize new project         |

### Output Structure (`lyra emit -o out/`)

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

Build the generated project:

```bash
cd out
cmake --preset default
cmake --build build
./build/sim
```

## Backends

Two execution backends:

| Backend     | Path                          | Use Case                    |
| ----------- | ----------------------------- | --------------------------- |
| Interpreter | AST → MIR → LIR → Interpreter | Development, debugging      |
| Codegen     | AST → MIR → C++ → Binary      | Performance, production use |

Codegen is the default backend. Interpreter is useful for development (no compile step).

## Planned Features

### Project Configuration (`lyra.toml`)

```toml
[package]
name = "my_design"
top = "Top"

[sources]
files = ["src/top.sv", "src/sub.sv"]
incdir = ["src/", "include/"]
```

With a config file, commands become simpler:

```bash
lyra run       # Uses lyra.toml
lyra emit      # Uses lyra.toml
lyra check     # Uses lyra.toml
```

### Additional Commands

```bash
lyra init my_project   # Create new project with lyra.toml
lyra build             # Emit + compile (don't run)
```

## Internal Architecture

### Library Structure

```
include/lyra/
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

| Class         | Location       | Role                                 |
| ------------- | -------------- | ------------------------------------ |
| `Interpreter` | `interpreter/` | MIR → LIR → run                      |
| `Compiler`    | `compiler/`    | MIR → C++ → compile → run            |
| `Codegen`     | `compiler/`    | Generates C++ source from MIR        |
| `Module`      | `sdk/`         | Base class for generated modules     |
| `Scheduler`   | `sdk/`         | Coroutine-based simulation scheduler |

## Design Decisions

- **Default backend**: Codegen (production path)
- **C++ standard**: C++23 (for coroutines)
- **Preferred compiler**: Clang (better coroutine support than GCC)
- **Build system for output**: CMake with presets
- **One module per file**: Scales to 1000+ modules
