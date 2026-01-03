# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Documentation

| Document                           | Purpose                                    |
| ---------------------------------- | ------------------------------------------ |
| `docs/philosophy.md`               | North star, priorities, tradeoffs          |
| `docs/design-principles.md`        | Implementation guidelines, coding patterns |
| `docs/architecture.md`             | Component relationships, data flow         |
| `docs/cpp-codegen.md`              | SV to C++ mapping, coroutine model         |
| `docs/cli-design.md`               | CLI tool design, config format, commands   |
| `docs/documentation-guidelines.md` | Documentation guidelines                   |

## Commands

```bash
bazel build //...                              # Build
bazel test //...                               # Test
clang-format -i <files>                        # Format C++
npx prettier --write <files>                   # Format docs
buildifier -r .                                # Format Bazel files
```

## Architecture

SystemVerilog compiler and simulator:

```
SV → AST → MIR → LIR → Interpreter
         ↘ MIR → C++ → Binary (codegen path)
```

Headers in `include/lyra/`, implementations in `src/lyra/`.

## Testing

YAML-based tests in `tests/sv_features/`. Each test runs both interpreter and codegen backends.

## Code Style

- C++23, Google style, clangd warning-free
- Naming: `CamelCase` classes/functions, `lower_case_` members, `kCamelCase` enums
- Use IEEE 1800 LRM terminology for SystemVerilog features
- Prefer modern C++ idioms:
  - `std::format` over string concatenation
  - `std::span`, `std::string_view` for non-owning references
  - `std::array` over C arrays
  - `std::optional`, `std::expected` for error handling
  - Structured bindings, range-based for loops
