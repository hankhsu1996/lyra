#CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Documentation

| Document                           | Purpose                                             |
| ---------------------------------- | --------------------------------------------------- |
| `docs/philosophy.md`               | North star, priorities, tradeoffs                   |
| `docs/architecture-principles.md`  | Architectural north stars, structural rules         |
| `docs/compilation-model.md`        | Specialization-based compilation data model         |
| `docs/design-principles.md`        | Implementation guidelines, coding patterns          |
| `docs/architecture.md`             | Component relationships, data flow                  |
| `docs/pipeline-contract.md`        | Layer responsibilities, boundaries, correctness     |
| `docs/hir-design.md`               | HIR layer design (language semantics)               |
| `docs/mir-design.md`               | MIR layer design (execution semantics)              |
| `docs/llvm-backend.md`             | MIR -> LLVM lowering strategy                       |
| `docs/runtime.md`                  | Simulation engine, scheduling, backend-agnostic API |
| `docs/change-propagation.md`       | Dirty tracking, subscriptions, wakeup filtering     |
| `docs/type-system.md`              | Type interning, type kinds, 4-state representation  |
| `docs/parameterized-modules.md`    | Module parameters, specialization, param classes    |
| `docs/module-hierarchy.md`         | Module hierarchy, port connections, realization     |
| `docs/cli-design.md`               | CLI tool design, lyra.toml config, commands         |
| `docs/state-layout.md`             | Arena state layout, container semantics             |
| `docs/limitations.md`              | Unsupported SystemVerilog features                  |
| `docs/string-types.md`             | String type handling, slang interaction             |
| `docs/error-handling.md`           | Error types, when to use each                       |
| `docs/execution-modes.md`          | AOT vs JIT, shared pipeline, CLI semantics          |
| `docs/profiling.md`                | Callgrind profiling workflow, methodology           |
| `docs/documentation-guidelines.md` | Documentation guidelines                            |

**When writing or editing documentation, follow `docs/documentation-guidelines.md`.** Key rules: concise over complete, capture decisions not implementation, ASCII only, integrate don't append.

## Commands

```bash
bazel build //...                              # Build
bazel test //...                               # Test
clang-format -i <files>                        # Format C++
npx prettier --write <files>                   # Format docs
buildifier -r .                                # Format Bazel files
```

## Lyra CLI

```bash
lyra init [name]                 # Create a new project
lyra compile [files...]          # Compile to native executable
lyra run [files...]              # Run simulation (AOT, default)
lyra run --backend=jit [files]   # Run via ORC JIT (in-process)
lyra check [files...]            # Check source files for errors
lyra dump hir [files...]         # Dump HIR representation
lyra dump mir [files...]         # Dump MIR representation
lyra dump llvm [files...]        # Dump LLVM IR
```

### Debugging Flags

```bash
lyra run -v [files...]           # Show phase timing (-v, -vv, -vvv)
lyra run --stats [files...]      # Show LLVM IR statistics (top 10 functions)
lyra run --stats=N [files...]    # Show top N functions (0 = summary only)
```

## SystemVerilog Version

Lyra targets **IEEE 1800-2023** (SystemVerilog 2023). The slang frontend is configured with `languageVersion = v1800_2023`. When testing SV 2023 features directly with slang, use `--std 1800-2023`.

## Architecture

SystemVerilog compiler with specialization-based compilation:

```
SV -> slang -> Elaboration Discovery
                    |
            Specialization Compilation (parallel per spec)
              AST -> HIR -> MIR -> LLVM IR
                    |
            Design Realization (design-wide)
                    |
            Execution
```

See `docs/compilation-model.md` for the data model.

Headers in `include/lyra/`, implementations in `src/lyra/`.

## Testing

YAML-based tests in `tests/sv_features/`. See `tests/suites.yaml` for suite definitions.

```bash
bazel test //tests:jit_dev_tests --test_output=errors    # In-process ORC JIT tests
```

### Ad-Hoc Testing

Run a specific test file without modifying suite definitions:

```bash
bazel test //tests:jit_dev_tests \
  --test_arg=--test_file=operators/binary/two_state.yaml \
  --test_arg=--backend=jit \
  --test_output=errors
```

Path is relative to `tests/sv_features/`. `--backend` is required with `--test_file`.

## Benchmarks

Benchmark runner: `tools/bench/run_benchmarks.py`. Fixtures in `tools/bench/fixtures/`.

```bash
python3 tools/bench/run_benchmarks.py --tier=nightly    # Full nightly run
python3 tools/bench/run_benchmarks.py --tier=pr          # Quick PR check
```

New benchmark designs go in `tools/bench/fixtures/<name>/` (with `lyra.toml` + `.sv` files) and must be registered in `DESIGNS` and `TIER_CONFIG` in the runner script.

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

| Error Type            | When to Use                             |
| --------------------- | --------------------------------------- |
| `DiagnosticException` | AST->HIR lowering only (has source loc) |
| `InternalError`       | Compiler bugs (invariant violations)    |
| `std::runtime_error`  | Runtime errors                          |

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
