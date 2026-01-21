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

| Command                   | Description                           |
| ------------------------- | ------------------------------------- |
| `lyra run [files]`        | Run simulation (MIR backend, default) |
| `lyra run --backend=llvm` | Run simulation via LLVM lli           |
| `lyra dump hir <file>`    | Dump HIR representation               |
| `lyra dump mir <file>`    | Dump MIR representation               |
| `lyra dump llvm <file>`   | Dump LLVM IR                          |

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
lyra dump hir file.sv   # HIR (high-level IR, close to AST)
lyra dump mir file.sv   # MIR (mid-level IR, execution semantics)
lyra dump llvm file.sv  # LLVM IR (for LLVM backend)
```

This is the only command that accepts file arguments directly.

## Backends

Two execution backends:

| Backend       | Path                        | Use Case              |
| ------------- | --------------------------- | --------------------- |
| MIR (default) | AST -> HIR -> MIR -> Interp | Full feature support  |
| LLVM          | AST -> HIR -> MIR -> LLVM   | Experimental, limited |

The MIR interpreter is the default backend and supports the full feature set.
The LLVM backend (`--backend=llvm`) is experimental and has limited feature support.
It requires `lli` (LLVM interpreter) to be installed.

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
