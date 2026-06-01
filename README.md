# Lyra: A Modern SystemVerilog Simulation Toolchain

[![Bazel Build and Test](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml)
[![Bazel File Lint](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-lint.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-lint.yml)
[![C++ Format](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-format.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-format.yml)
[![C++ Tidy](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-tidy.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-tidy.yml)

**Lyra** is a SystemVerilog compiler and simulator built around a multi-stage IR pipeline,
prioritizing fast iteration (compile + run + debug) over peak simulation speed.

## Prerequisites

- [Bazel](https://bazel.build/) (via [bazelisk](https://github.com/bazelbuild/bazelisk))
- Clang 20+
- C++23-compliant standard library (GCC 13+ libstdc++ or libc++)

## Quick Start

```bash
# Build the compiler
bazel build //:lyra

# Dump HIR or MIR for a source file
./bazel-bin/lyra dump hir --no-project --top Top path/to/file.sv
./bazel-bin/lyra dump mir --no-project --top Top path/to/file.sv

# Run a source file end to end (builds and executes, streaming output)
./bazel-bin/lyra run --no-project --top Top path/to/file.sv

# Or write a self-contained C++ project, optionally building it
./bazel-bin/lyra emit cpp --no-project --top Top -o out path/to/file.sv
./bazel-bin/lyra compile --no-project --top Top -o out path/to/file.sv
```

The CLI exposes `dump {hir,mir}`, `emit cpp`, `compile`, and `run`.

## Tests

```bash
bazel test //...    # Run tests
```

## Examples

See [examples/README.md](examples/README.md) for sample projects.

## Architecture

```
SV ---> slang AST ---> HIR ---> MIR ---> backend::cpp ---> C++ source + runtime
```

- **AST**: Parsed using [Slang](https://github.com/MikePopoloski/slang).
- **HIR**: Source-near semantic IR preserving SystemVerilog constructs.
- **MIR**: Object-oriented semantic IR (objects, members, callables, actions).
- **LIR**: Execution-oriented IR (contract in `docs/architecture/lir.md`).
- **backend::cpp**: Emits C++ source linked against the coroutine-based runtime in
  `include/lyra/runtime/`.

See [docs/architecture/README.md](docs/architecture/README.md) for layer contracts.

## Documentation

See [docs/](docs/) for the documentation index.
