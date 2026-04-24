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
- [xir-design.md](xir-design.md) - execution-model IR (closures, observers, wait models, object composition)
- [mir-design.md](mir-design.md) - control-flow plumbing (basic blocks, terminators, Place/Operand)
- [type-system.md](type-system.md) - type representation and interning
- [string-types.md](string-types.md) - string handling
- [assertions.md](assertions.md) - assertion architecture
- [parameterized-modules.md](parameterized-modules.md) - parameters and specialization

## Backend and Runtime

- [llvm-backend.md](llvm-backend.md) - MIR to LLVM lowering
- [runtime.md](runtime.md) - simulation engine
- [runtime-object-model.md](runtime-object-model.md) - runtime object model, constructor assembly, hierarchy
- [scheduling.md](scheduling.md) - scheduling regions
- [change-propagation.md](change-propagation.md) - dirty tracking and wakeup filtering

## CLI and Testing

- [cli-design.md](cli-design.md) - CLI behavior and configuration
- [test-framework.md](test-framework.md) - test harness
- [execution-modes.md](execution-modes.md) - AOT vs JIT vs MIR

## Reference

- [error-handling.md](error-handling.md) - error types and usage
- [documentation-guidelines.md](documentation-guidelines.md) - documentation conventions
