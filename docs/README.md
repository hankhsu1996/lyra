# Documentation Index

Start here for an overview of Lyra's design and constraints.

## Getting Oriented

- [philosophy.md](philosophy.md) - product priorities and tradeoffs
- [architecture-principles.md](architecture-principles.md) - architectural north stars and structural rules
- [compilation-model.md](compilation-model.md) - specialization-based compilation data model
- [design-principles.md](design-principles.md) - implementation guidelines and coding patterns
- [architecture.md](architecture.md) - component boundaries and data flow
- [pipeline-contract.md](pipeline-contract.md) - layer responsibilities and boundaries

## Language and Semantics

- [hir-design.md](hir-design.md) - SystemVerilog semantic model
- [mir-design.md](mir-design.md) - execution semantics and control flow
- [type-system.md](type-system.md) - type representation and interning
- [string-types.md](string-types.md) - string handling
- [parameterized-modules.md](parameterized-modules.md) - parameters and specialization
- [module-hierarchy.md](module-hierarchy.md) - hierarchy support and assembly binding

## Backend and Runtime

- [llvm-backend.md](llvm-backend.md) - MIR to LLVM lowering
- [runtime.md](runtime.md) - simulation engine
- [scheduling.md](scheduling.md) - scheduling regions
- [change-propagation.md](change-propagation.md) - dirty tracking and wakeup filtering

## CLI and Testing

- [cli-design.md](cli-design.md) - CLI behavior and configuration
- [test-framework.md](test-framework.md) - test harness
- [execution-modes.md](execution-modes.md) - AOT vs JIT vs MIR

## Reference

- [limitations.md](limitations.md) - unsupported SystemVerilog features
- [error-handling.md](error-handling.md) - error types and usage
- [aot-readiness.md](aot-readiness.md) - AOT milestones and gates
- [documentation-guidelines.md](documentation-guidelines.md) - documentation conventions
