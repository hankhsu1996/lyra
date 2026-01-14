#CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Documentation

| Document                           | Purpose                                              |
| ---------------------------------- | ---------------------------------------------------- |
| `docs/philosophy.md`               | North star, priorities, tradeoffs                    |
| `docs/design-principles.md`        | Implementation guidelines, coding patterns           |
| `docs/architecture.md`             | Component relationships, data flow                   |
| `docs/hir-design.md`               | HIR layer design                                     |
| `docs/mir-design.md`               | MIR layer design (Rust-style)                        |
| `docs/type-system.md`              | Type interning, type kinds                           |
| `docs/cpp-codegen.md`              | SV to C++ mapping, coroutine model                   |
| `docs/parameterized-modules.md`    | Module parameters, template specialization, strings  |
| `docs/module-hierarchy.md`         | Module hierarchy support (MIR, codegen, interpreter) |
| `docs/cli-design.md`               | CLI tool design, lyra.toml config, commands          |
| `docs/limitations.md`              | Unsupported SystemVerilog features                   |
| `docs/string-types.md`             | String type handling, slang interaction              |
| `docs/error-handling.md`           | Error types, when to use each                        |
| `docs/documentation-guidelines.md` | Documentation guidelines                             |

**When writing or editing documentation, follow `docs/documentation-guidelines.md`.** Key rules: concise over complete, capture decisions not implementation, target 200-400 lines, integrate don't append.

## Commands

```bash
bazel build //...                              # Build
bazel test //...                               # Test
clang-format -i <files>                        # Format C++
npx prettier --write <files>                   # Format docs
buildifier -r .                                # Format Bazel files
```

## Linting

Run clang-tidy for static analysis. First generate `compile_commands.json`:

```bash
bazel run //:refresh_compile_commands
```

Then run clang-tidy on specific files:

```bash
clang-tidy -p . <files>                # Single file (~20s each)
run-clang-tidy -p . -j 20 <files>      # Multiple files in parallel
```

Or run on all files (update path when new code is added):

```bash
run-clang-tidy -p . -header-filter='^.*(src|include)/lyra/.*' -j 20 src/lyra/
```

**Note:** When using Bash tool, avoid `$(...)` subshell substitution (it gets escaped incorrectly). Use hardcoded values instead of `$(nproc)`.

## Lyra CLI

**Note:** CLI commands are not functional until the new pipeline is built. Documentation kept for reference.

Lyra has two modes: **project-based commands** and **standalone dump commands**.

### Project-Based Commands

These commands require `lyra.toml` in the current directory. They take **NO positional arguments**.

```bash
lyra run                    # Build and run simulation
lyra build                  # Generate and compile (no run)
lyra emit                   # Generate output to out/
lyra check                  # Parse and validate only
```

**Important:** Must run from directory containing `lyra.toml`. To run from elsewhere:

```bash
bash -c 'cd /path/to/project && lyra run'
```

### Standalone Commands

These commands take source files directly (no `lyra.toml` needed):

```bash
lyra init <name>            # Create new project in <name>/
lyra init                   # Create new project in current directory
lyra dump hir <file.sv>     # Dump HIR
lyra dump mir <file.sv>     # Dump MIR
```

### Project Configuration

The `lyra.toml` format:

```toml
[package]
name = "project_name"
top = "TopModule"

[sources]
files = ["file1.sv", "file2.sv"]
```

## SystemVerilog Version

Lyra targets **IEEE 1800-2023** (SystemVerilog 2023). The slang frontend is configured with `languageVersion = v1800_2023`. When testing SV 2023 features directly with slang, use `--std 1800-2023`.

## Architecture

SystemVerilog compiler:

```
SV ---> AST ---> HIR ---> MIR ---> LLVM IR ---> Binary
                  |
                  +---> C++ (secondary)
```

Headers in `include/lyra/`, implementations in `src/lyra/`.

## Testing

**Note:** Tests are currently disabled (legacy code moved). Commands below will not work until new pipeline is built. Documentation kept for reference.

YAML-based tests in `tests/sv_features/`.

```bash
bazel test //tests:sv_feature_tests              # Run ALL tests (sharded)
bazel test //tests:control_flow_loops_tests      # Run single category
bazel test //tests:datatypes_arrays_tests        # Run single category
```

## Code Style

- C++23, Google style, clang-tidy warning-free
- Naming: `CamelCase` classes/functions, `lower_case_` members, `kCamelCase` enums
- Use IEEE 1800 LRM terminology for SystemVerilog features
- Prefer modern C++ idioms:
  - `std::format` over string concatenation
  - `std::span`, `std::string_view` for non-owning references
  - `std::array` over C arrays
  - `std::optional`, `std::expected` for error handling
  - Structured bindings, range-based for loops

## Error Handling

See `docs/error-handling.md` for full details.

| Error Type            | When to Use                            |
| --------------------- | -------------------------------------- |
| `DiagnosticException` | AST→HIR lowering only (has source loc) |
| `InternalError`       | Compiler bugs (invariant violations)   |
| `std::runtime_error`  | Runtime errors                         |

For shared code, return `std::expected<T, std::string>` and let callers convert to `std::runtime_error`.

## Approach to Changes

For full design rationale, see `docs/design-principles.md`.

### Adding Features

Before implementing a new feature directly:

1. **Explore existing structure** - Understand how similar things work
2. **Look for generalization** - Can an existing abstraction be extended?
3. **Find the right level** - The best change is often minimal when placed correctly
4. **Prefer extending over adding** - Modify existing infrastructure rather than creating parallel structures

The goal: make the new requirement feel like a natural extension, not a bolt-on.

### Fixing Bugs

After debugging and finding the immediate cause:

1. **Step back** - Why does this bug exist? What allowed it?
2. **Look for design issues** - Bugs often indicate deeper problems
3. **Fix the root cause** - Not just the symptom
4. **Avoid band-aids** - Don't just add a control branch; address the fundamental issue

The goal: leave the codebase stronger, not just patched.
