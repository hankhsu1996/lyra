# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this
repository.

Layer contracts live under `docs/architecture/`. Tracked gaps relative to those contracts live under
`docs/progress/`. The `/archived` directory holds reference material from earlier iterations of the
project; treat it as read-only and do not import from it.

## Documentation

See `docs/README.md` for the documentation index. When writing or editing documentation, follow
`docs/style.md`.

## Commands

```bash
npm ci
bazel build //...
bazel test //...
clang-format -i <files>
npm run format
buildifier -r .
```

## Lyra CLI

```bash
lyra dump hir [files...]              # Dump HIR (looks up lyra.toml by default)
lyra dump mir [files...]              # Dump MIR
lyra emit cpp -o <dir> [files...]     # Write a self-contained C++ project (sources + build.sh + runtime)
lyra compile -o <dir> [files...]      # Emit that project and build it -> <dir>/program
lyra run [files...]                   # Emit, build, and execute, streaming the simulation output
```

Common flags (apply to all subcommands):

```bash
--no-project                     # Bypass lyra.toml lookup; operate on bare files
--top <name>                     # Top module name
-I <dir>                         # Include search directory
-D NAME[=VALUE]                  # Preprocessor macro
-G NAME=VALUE                    # Override a module parameter
--single-unit                    # Compile all files as one compilation unit
-o <dir>                         # Output directory (required for emit cpp and compile)
```

## SystemVerilog Version

Lyra targets **IEEE 1800-2023** (SystemVerilog 2023). The slang frontend is configured with
`languageVersion = v1800_2023`. When testing SV 2023 features directly with slang, use
`--std 1800-2023`.

## Architecture

The pipeline is HIR -> MIR -> LIR -> LLVM IR. HIR, MIR, and `backend::cpp` exist in source; LIR is
defined as a contract in `docs/architecture/lir.md`. See `docs/architecture/compiler_overview.md`
for the binding contract.

```
SV ---> slang AST ---> HIR ---> MIR ---> backend::cpp ---> C++ source + runtime
```

- Semantic modeling lives in HIR and MIR.
- Execution modeling lives in LIR and below.
- A compilation unit is the top-level semantic boundary (module, package, interface).
- Compile-time produces class-level artifacts; runtime constructs objects and installs relations.

Headers in `include/lyra/`, implementations in `src/lyra/`.

## Testing

YAML-based tests in `tests/cases/`. See `tests/suites.yaml` for suite definitions.

```bash
bazel test //... --test_output=errors    # Same target set CI runs
```

## Benchmarks

The benchmark runner under `tools/bench/` and the corresponding CI jobs depend on the `lyra run`
subcommand and the runtime static library. Both now exist, but the runner has not been re-validated
against them.

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
- No block-comment parameter labels at call sites (`/*param=*/value`). IDE inlay hints make them
  redundant.

## Error Handling

The active error policy:

| Error Type           | When to Use                                                   |
| -------------------- | ------------------------------------------------------------- |
| `diag::Result<T>`    | Recoverable lowering / backend failures with structured codes |
| `InternalError`      | Compiler bugs (invariant violations)                          |
| `std::runtime_error` | Runtime errors                                                |

Avoid `assert()` and `<cassert>` (use `InternalError` instead). `catch(...)` is allowed only in
`src/lyra/driver/`. Throw `InternalError` only for compiler-bug invariants, not for user-facing
diagnostics.

## Approach to Changes

### Adding Features

Before implementing a new feature directly:

1. **Explore existing structure** - Understand how similar things work
2. **Look for generalization** - Can an existing abstraction be extended?
3. **Find the right level** - The best change is often minimal when placed correctly
4. **Prefer extending over adding** - Modify existing infrastructure rather than creating parallel
   structures

The goal: make the new requirement feel like a natural extension, not a bolt-on.

### Fixing Bugs

After debugging and finding the immediate cause:

1. **Step back** - Why does this bug exist? What allowed it?
2. **Look for design issues** - Bugs often indicate deeper problems
3. **Fix the root cause** - Not just the symptom
4. **Avoid band-aids** - Don't just add a control branch; address the fundamental issue

The goal: leave the codebase stronger, not just patched.
