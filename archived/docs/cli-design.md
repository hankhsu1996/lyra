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
+-- lyra.toml       # Project configuration
+-- my_project.sv   # SystemVerilog source
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

| Command                  | Description                       |
| ------------------------ | --------------------------------- |
| `lyra init [name]`       | Create a new project              |
| `lyra run [files]`       | Run simulation (AOT, default)     |
| `lyra run --backend=jit` | Run simulation via JIT (dev-only) |
| `lyra compile [files]`   | Compile to native executable      |
| `lyra check [files]`     | Check source files for errors     |
| `lyra dump hir [files]`  | Dump HIR representation           |
| `lyra dump mir [files]`  | Dump MIR representation           |
| `lyra dump llvm [files]` | Dump LLVM IR                      |

Commands can use either `lyra.toml` or CLI arguments (or both).

### Global Options

| Option     | Description                           |
| ---------- | ------------------------------------- |
| `-C <dir>` | Run as if lyra was started in `<dir>` |

Example: `lyra -C path/to/project run`

### Project Command Options

The `run`, `check`, and `dump` commands accept these options:

| Option                    | Description                                                                  |
| ------------------------- | ---------------------------------------------------------------------------- |
| `--no-project`            | Skip lyra.toml, use CWD-relative paths                                       |
| `--top <module>`          | Top module name                                                              |
| `-I, --include-directory` | Add include search path (repeatable)                                         |
| `-D, --define-macro`      | Define preprocessor macro (e.g., `-DDEBUG`)                                  |
| `-G<name>=<value>`        | Override top-level module parameter (repeatable, applies to all top modules) |
| `-W <warn>`               | Warning control (e.g., `-Wno-unused`)                                        |
| `-O <level>`              | Optimization level (0-3, default 2)                                          |
| `--pedantic`              | Enable strict LRM compliance (disallow implicit enum conversions)            |
| `-v, --verbose`           | Verbosity level (-v, -vv, -vvv or --verbose=0-3)                             |
| `--stats[=N]`             | Show LLVM IR statistics (top N functions, default 10; 0 for summary only)    |
| `--trace-summary`         | Enable trace summary output (event counts at end of simulation)              |
| `--trace-signals[=FILE]`  | Enable text signal trace (to stdout or file)                                 |
| `--trace-activations`     | Enable activation trace (live per-process wake/run log to stderr)            |
| `-f <file>`               | Command file (paths relative to CWD)                                         |
| `-F <file>`               | Command file (paths relative to file itself)                                 |
| `<files...>`              | Source files (positional)                                                    |

### CLI Arguments

CLI arguments override or merge with lyra.toml settings:

```bash
# Override top module from lyra.toml
lyra run --top Testbench

# Add include directories (merges with lyra.toml)
lyra run -I extra/include/

# Define macros (merges with lyra.toml)
lyra run -DDEBUG -DWIDTH=32

# Use command file
lyra run -f sources.f
```

For ad-hoc runs without a project, use `--no-project`:

```bash
lyra run --no-project --top Top file.sv
```

### Plusargs

Runtime plusargs for `$test$plusargs` and `$value$plusargs` are passed after `--`:

```bash
lyra run [options] [files...] -- +FOO +BAR=1
```

Plusargs are passed verbatim to the simulation runtime. The `--` separator distinguishes Lyra CLI options from simulation plusargs.

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

Three execution backends. See [execution-modes.md](execution-modes.md) for
architectural details.

| Backend       | Flag            | Use Case                      |
| ------------- | --------------- | ----------------------------- |
| AOT (default) | `--backend=aot` | Production path               |
| JIT           | `--backend=jit` | Developer iteration, dev-only |

AOT and JIT share the same LLVM codegen pipeline (`LowerMirToLlvm`). AOT
compiles to a native executable; JIT compiles and runs in-process.

## Strictness Modes

By default, Lyra relaxes certain LRM rules for compatibility with common industry practices. The `--pedantic` flag enforces strict LRM-style behavior.

| Behavior                             | Default (compatibility) | `--pedantic` (strict) |
| ------------------------------------ | ----------------------- | --------------------- |
| Implicit integral-to-enum conversion | Allowed                 | Error                 |

In `lyra.toml`:

```toml
[compilation]
pedantic = true
```

## Design Decisions

- **Dual mode**: Commands work with lyra.toml, CLI arguments, or both
- **Default backend**: AOT (production path, compile to native executable)
- **JIT backend**: Dev-only, accessed via `--backend=jit`
- **`run` semantics**: Always means "execute", backend is an implementation detail
