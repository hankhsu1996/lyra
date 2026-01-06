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

Lyra uses a project-based workflow with `lyra.toml` configuration files.

```bash
lyra init <name>                               # Create new project
lyra run                                       # Build and run (requires lyra.toml)
lyra run --interpret                           # Run with interpreter
lyra build                                     # Generate C++ and compile (no run)
lyra emit                                      # Generate C++ project to out/
lyra check                                     # Parse and validate only
lyra dump cpp <file.sv>                        # Dump generated C++ code
lyra dump mir <file.sv>                        # Dump MIR
lyra dump lir <file.sv>                        # Dump LIR
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
