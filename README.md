# Lyra: A Modern SystemVerilog Simulation Toolchain

[![Bazel Build and Test](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml)
[![Bazel File Lint](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-lint.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-lint.yml)
[![C++ Format](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-format.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-format.yml)
[![C++ Tidy](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-tidy.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-tidy.yml)

**Lyra** is a SystemVerilog compiler and simulator built around a multi-stage IR pipeline.
It compiles to LLVM IR for execution, prioritizing fast iteration (compile + run + debug) over peak simulation speed.

## Prerequisites

- [Bazel](https://bazel.build/) (via [bazelisk](https://github.com/bazelbuild/bazelisk))
- Clang 20+
- C++23-compliant standard library (GCC 13+ libstdc++ or libc++)
- LLVM tools with `lli` available in PATH

## Quick Start

```bash
# Build lyra
bazel build //:lyra

# Create a new project
lyra init my_project    # creates lyra.toml and my_project.sv
cd my_project

# Run simulation
lyra run
```

Ensure the built `lyra` binary is on your PATH.
`lyra run` requires `lli` in PATH.

Lyra expects a project workflow with `lyra.toml`. CLI arguments work for quick runs. See [docs/cli-design.md](docs/cli-design.md).

## Tests

```bash
bazel test //...    # Run tests
```

## Examples

See [examples/README.md](examples/README.md) for sample projects and commands.

## Architecture

```
SV ---> AST ---> HIR ---> MIR ---> LLVM IR ---> executable
```

- **AST**: Parsed using [Slang](https://github.com/MikePopoloski/slang)
- **HIR**: High-level IR preserving SystemVerilog semantics
- **MIR**: Mid-level IR for execution (Place/Value model, basic blocks)
- **LLVM IR**: Generated for execution via lli (interpreter) or native compilation

See [docs/architecture.md](docs/architecture.md) for detailed design.

## Philosophy

Minimize iteration time (compile + run + debug). Peak simulation speed is secondary.

See [docs/philosophy.md](docs/philosophy.md) for design rationale.

## Limitations

Lyra supports a focused subset of SystemVerilog. See [docs/limitations.md](docs/limitations.md).

## Documentation

- [docs/architecture.md](docs/architecture.md) - component boundaries and data flow
- [docs/cli-design.md](docs/cli-design.md) - CLI behavior and configuration
- [docs/limitations.md](docs/limitations.md) - unsupported language features
- [docs/design-principles.md](docs/design-principles.md) - implementation guidelines
- [docs/philosophy.md](docs/philosophy.md) - product priorities and tradeoffs
