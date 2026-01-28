# Documentation Index

Start here for an overview of Lyra's design and constraints.

## Getting Oriented

- [philosophy.md](philosophy.md) - product priorities and tradeoffs
- [architecture.md](architecture.md) - component boundaries and data flow
- [design-principles.md](design-principles.md) - implementation guidelines
- [pipeline-contract.md](pipeline-contract.md) - layer responsibilities and boundaries

## Language and Semantics

- [hir-design.md](hir-design.md) - SystemVerilog semantic model
- [mir-design.md](mir-design.md) - execution semantics and control flow
- [type-system.md](type-system.md) - type representation and interning
- [string-types.md](string-types.md) - string handling
- [parameterized-modules.md](parameterized-modules.md) - parameters and specialization
- [module-hierarchy.md](module-hierarchy.md) - hierarchy support

## Backend and Runtime

- [llvm-backend.md](llvm-backend.md) - MIR to LLVM lowering
- [runtime.md](runtime.md) - simulation engine
- [scheduling.md](scheduling.md) - scheduling regions

## CLI and Testing

- [cli-design.md](cli-design.md) - CLI behavior and configuration
- [test-framework.md](test-framework.md) - test harness

## Reference

- [limitations.md](limitations.md) - unsupported SystemVerilog features
- [error-handling.md](error-handling.md) - error types and usage
