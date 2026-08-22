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
lyra check [files...]                 # Elaborate and report diagnostics; no lowering
lyra dump hir|mir|lir|llvm [files...] # Dump the named intermediate form
lyra emit cpp -o <dir> [files...]     # Write a self-contained C++ project (sources + build.sh + runtime)
lyra compile -o <dir> [files...]      # Emit that project and build it -> <dir>/program
lyra run [files...]                   # Emit, build, and execute, streaming the simulation output
lyra cache clear                      # Drop the precompiled-header cache
lyra --help                           # Commands plus the whole merged option list
```

The command words are positional; everything after them is one command line shared with the slang
driver, so every front-end option slang accepts -- `--top`, `-I`, `-D`, `-G`, `--single-unit`, `-y`,
`--libext`, `-f` / `-F` filelists, the `-W` warning options -- reaches Lyra unchanged. `lyra --help`
prints the authoritative list. Lyra's own options:

```bash
--no-project                     # Bypass lyra.toml lookup; operate on bare files
-o <dir>                         # Output directory (required for emit cpp and compile)
--disable-assertions             # Skip assertion constructs during lowering
--backend cpp|jit|aot|lli        # How `run` executes the design
--cxx <program>                  # Host C++ compiler for the C++ backend
--dpi-link <file>                # Native source providing DPI-C foreign symbols
--format, --color / --no-color, --no-pch, --pch-cache-dir <dir>
```

A standalone `--` ends Lyra's command line: everything after it is the simulation's own argv, which
is where LRM 21.6 plusargs go.

## SystemVerilog Version

Lyra targets **IEEE 1800-2023** (SystemVerilog 2023), and defaults the front end to
`--std 1800-2023` and to slang's VCS compatibility mode; both are defaults, so a caller may override
either on the command line. When testing SV 2023 features directly with slang, use
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
- Comments: follow `docs/code-comments.md` (read pre-plan, pre-write, post-edit).

## Error Handling

The active error policy:

| Error Type        | When to Use                                                   |
| ----------------- | ------------------------------------------------------------- |
| `diag::Result<T>` | Recoverable lowering / backend failures with structured codes |
| `InternalError`   | Compiler bugs (invariant violations)                          |
| `SimulationError` | Failures of the simulated design at run time                  |

`InternalError` and `SimulationError` are the only exception types anyone may throw; `std::`
exception types are banned outside their own definitions. The dividing question is whether the
condition depends on a value the simulated program computes: a negative `new[N]` size, a
tagged-union access inconsistent with the tag, or a malformed run-time format string is the design's
failure and gets `SimulationError`, while a width, plane, or arena invariant the compiler itself
established gets `InternalError` and tells the reader to report a bug. An operation a legal program
requests that Lyra does not yet carry out is also `SimulationError` -- the reader's next step is to
ask for support, not to report a bug.

Avoid `assert()` and `<cassert>` (use `InternalError` instead). `catch(...)` is allowed only in
`src/lyra/driver/`.

This table governs the error channel. A control effect -- leaving a disabled scope (LRM 9.6.2) -- is
not an error and is thrown by the runtime that defines it; nothing else may add a thrown type.

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
