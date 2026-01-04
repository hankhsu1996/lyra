# Lyra: Rethinking SystemVerilog Simulation

[![Bazel Build and Test](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-build.yml)
[![Bazel File Lint](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-lint.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/bazel-lint.yml)
[![C++ Lint](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-lint.yml/badge.svg?branch=main&event=push)](https://github.com/hankhsu1996/lyra/actions/workflows/cpp-lint.yml)

**Lyra** is a SystemVerilog compiler that generates readable C++ for simulation.
It prioritizes fast iteration (compile + run + debug) over peak simulation speed.

## Build

```bash
bazel build //...   # Build all targets
bazel test //...    # Run tests
```

The `lyra` binary is at `bazel-bin/lyra`.

## Usage

Lyra uses a project-based workflow with `lyra.toml` configuration files.

```bash
lyra init my_project   # Create a new project
cd my_project
lyra emit              # Generate C++ files to out/
lyra build             # Compile to binary (implies emit)
lyra run               # Run simulation (implies build)
```

See [docs/cli-design.md](docs/cli-design.md) for configuration options and detailed CLI documentation.

## Architecture

```
                        +---> LIR ---> Interpreter
                        |
SV ---> AST ---> MIR ---+
                        |
                        +---> C++ ---> Binary
```

- **AST**: Parsed using [Slang](https://github.com/MikePopoloski/slang)
- **MIR**: High-level, structure-preserving intermediate representation
- **LIR**: Linear SSA-style IR for interpreter backend
- **Codegen**: C++ generation for production use

## Philosophy

Minimize iteration time (compile + run + debug). Peak simulation speed is secondary.

See [docs/philosophy.md](docs/philosophy.md) for design rationale.
