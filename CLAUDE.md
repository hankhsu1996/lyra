# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Documentation

| Document                           | Purpose                                     |
| ---------------------------------- | ------------------------------------------- |
| `docs/philosophy.md`               | North star, priorities, tradeoffs           |
| `docs/design-principles.md`        | Implementation guidelines, coding patterns  |
| `docs/architecture.md`             | Component relationships, data flow          |
| `docs/cpp-codegen.md`              | SV to C++ mapping, coroutine model          |
| `docs/cli-design.md`               | CLI tool design, lyra.toml config, commands |
| `docs/documentation-guidelines.md` | Documentation guidelines                    |

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
bazel run @hedron_compile_commands//:refresh_all
```

Then run clang-tidy on specific files (fast):

```bash
clang-tidy -p . <files>
```

Or run on all files (slower):

```bash
run-clang-tidy -p . -header-filter='^.*(src|include)/lyra/.*' -j 20 src/lyra/
```

**Note:** When using Bash tool, avoid `$(...)` subshell substitution (it gets escaped incorrectly). Use hardcoded values instead of `$(nproc)`.

## Lyra CLI

Lyra has two modes: **project-based commands** and **standalone dump commands**.

### Project-Based Commands

These commands require `lyra.toml` in the current directory. They take **NO positional arguments**.

```bash
lyra run                    # Build C++ and run simulation
lyra run --interpret        # Run with interpreter (no C++ compilation)
lyra build                  # Generate and compile C++ (no run)
lyra emit                   # Generate C++ project to out/
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
lyra dump cpp <file.sv>     # Dump generated C++ code
lyra dump mir <file.sv>     # Dump MIR
lyra dump lir <file.sv>     # Dump LIR
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

## Architecture

SystemVerilog compiler and simulator:

```
                    +---> LIR ---> Interpreter
                    |
SV ---> AST ---> MIR +
                    |
                    +---> C++ ---> Binary
```

Headers in `include/lyra/`, implementations in `src/lyra/`.

## Testing

YAML-based tests in `tests/sv_features/`. Each test runs both interpreter and codegen backends.

Test targets follow the pattern `//tests:<category>_<feature>_tests`:

```bash
bazel test //tests:control_flow_loops_tests      # Run loop tests
bazel test //tests:datatypes_arrays_tests        # Run array tests
bazel query '//tests/...:*' --output=label       # List all test targets
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

**User-facing errors** - Use `DiagnosticException` (`lyra/common/diagnostic.hpp`):

```cpp
throw DiagnosticException(
    Diagnostic::Error(source_range, "descriptive message"));
```

- Provides consistent error formatting with source locations
- Caught and printed by CLI commands via `PrintDiagnostic()`
- Use `slang::SourceRange{}` when no source location is available

**Internal errors** - Use `InternalError` (`lyra/common/internal_error.hpp`):

```cpp
throw common::InternalError("context", "detail about the bug");
```

- For compiler bugs, not user errors
- Includes message asking users to report the issue

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
