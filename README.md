# Lyra: A Modern SystemVerilog Simulation Toolchain

[![Bazel Build and Test](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml)
[![C++ Style](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-style.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-style.yml)
[![Bazel File Lint](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-lint.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-lint.yml)

**Lyra** is a SystemVerilog compiler and simulator built around a multi-stage IR pipeline,
prioritizing fast iteration (compile + run + debug) over peak simulation speed.

## Prerequisites

- [Bazel](https://bazel.build/) (via [bazelisk](https://github.com/bazelbuild/bazelisk))
- A C++23 compiler. The build defaults to Clang; GCC 13, GCC 14, and Clang 20 are exercised.

`lyra compile` and `lyra run` link the emitted program against a prebuilt runtime library, so the
compiler used for a design must be ABI-compatible with the one that built Lyra -- in practice, the
same standard library implementation.

## Quick Start

```bash
bazel build //:lyra

# Run a design end to end, streaming its output
./bazel-bin/lyra run --no-project --top Top path/to/file.sv

# Inspect any stage of the pipeline
./bazel-bin/lyra dump ast|hir|mir|lir|llvm --no-project --top Top path/to/file.sv

# Write a self-contained C++ project, optionally building it
./bazel-bin/lyra emit cpp --no-project --top Top -o out path/to/file.sv
./bazel-bin/lyra compile --no-project --top Top -o out path/to/file.sv
```

`lyra --help` lists every command and option, including the slang front-end options Lyra passes
through unchanged.

## Tests

```bash
bazel test //...
```

## Examples

See [examples/README.md](examples/README.md) for sample projects.

## Architecture

```
SV ---> slang AST ---> HIR ---> MIR ---> LIR ---> LLVM IR
                                 |
                                 +---> backend::cpp ---> C++ source + runtime
```

- **AST**: Parsed using [Slang](https://github.com/MikePopoloski/slang).
- **HIR**: Source-near semantic IR preserving SystemVerilog constructs.
- **MIR**: Object-oriented semantic IR (objects, members, callables, actions).
- **LIR**: Execution-oriented IR -- control-flow graphs, basic blocks, storage.
- **backend::cpp**: Emits C++ source linked against the coroutine-based runtime in
  `include/lyra/runtime/`.

See [docs/architecture/README.md](docs/architecture/README.md) for layer contracts.

## Documentation

See [docs/](docs/) for the documentation index.
