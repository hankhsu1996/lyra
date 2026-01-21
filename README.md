# Lyra: A Modern SystemVerilog Simulation Toolchain

[![Bazel Build and Test](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml)
[![Bazel File Lint](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-lint.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-lint.yml)
[![C++ Format](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-format.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-format.yml)
[![C++ Tidy](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-tidy.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-tidy.yml)

**Lyra** is a SystemVerilog compiler and simulator built around a multi-stage IR pipeline.
It compiles to LLVM IR for execution, prioritizing fast iteration (compile + run + debug) over peak simulation speed.

## Build

```bash
bazel build //...   # Build all targets
bazel test //...    # Run tests
```

The `lyra` binary is at `bazel-bin/lyra`.

## Usage

```bash
lyra run [files...]   # Run simulation
```

Lyra can also use `lyra.toml` for project configuration. See [docs/cli-design.md](docs/cli-design.md) for details.

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
