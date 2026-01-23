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
```

### Project Structure

```
my_project/
├── lyra.toml       # Project configuration
└── my_project.sv   # SystemVerilog source
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

[diagnostics]
warnings = ["no-unused"]       # Optional: warning control
```

## Commands

| Command                  | Description                            |
| ------------------------ | -------------------------------------- |
| `lyra init [name]`       | Create a new project                   |
| `lyra run [files]`       | Run simulation (LLVM backend, default) |
| `lyra run --backend=mir` | Run simulation via MIR interpreter     |
| `lyra check [files]`     | Check source files for errors          |
| `lyra dump hir [files]`  | Dump HIR representation                |
| `lyra dump mir [files]`  | Dump MIR representation                |
| `lyra dump llvm [files]` | Dump LLVM IR                           |

Commands can use either `lyra.toml` or CLI arguments (or both).

### Global Options

| Option     | Description                           |
| ---------- | ------------------------------------- |
| `-C <dir>` | Run as if lyra was started in `<dir>` |

Example: `lyra -C path/to/project run`

### Project Command Options

The `run`, `check`, and `dump` commands accept these options:

| Option                    | Description                                  |
| ------------------------- | -------------------------------------------- |
| `--top <module>`          | Top module name                              |
| `-I, --include-directory` | Add include search path (repeatable)         |
| `-D, --define-macro`      | Define preprocessor macro (e.g., `-DDEBUG`)  |
| `-W <warn>`               | Warning control (e.g., `-Wno-unused`)        |
| `-f <file>`               | Command file (paths relative to CWD)         |
| `-F <file>`               | Command file (paths relative to file itself) |
| `<files...>`              | Source files (positional)                    |

### CLI Mode

Commands work without `lyra.toml` when source files are provided:

```bash
# Check syntax without lyra.toml
lyra check --top Top file.sv

# Run simulation with MIR interpreter
lyra run --backend=mir --top Testbench pkg.sv dut.sv tb.sv

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

### Dump Command

Debug command to inspect internal representations:

```bash
lyra dump hir file.sv   # HIR (high-level IR, close to AST)
lyra dump mir file.sv   # MIR (mid-level IR, execution semantics)
lyra dump llvm file.sv  # LLVM IR (for LLVM backend)
```

Like `run`, accepts `--top`, `-I`, `-D`, `-W`, and positional files.

## Backends

Two execution backends:

| Backend        | Path                        | Use Case                      |
| -------------- | --------------------------- | ----------------------------- |
| LLVM (default) | AST -> HIR -> MIR -> LLVM   | Production path               |
| MIR            | AST -> HIR -> MIR -> Interp | Development, full feature set |

The LLVM backend is the default. The MIR interpreter (`--backend=mir`) supports the full feature set and is useful for development and testing.

## Design Decisions

- **Dual mode**: Commands work with lyra.toml, CLI arguments, or both
- **Default backend**: LLVM (production path)
- **MIR backend**: Full feature support, accessed via `--backend=mir`
