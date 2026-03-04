# Philosophy

## North Star

**Minimize iteration time = compile time + runtime + debug time.**

Peak simulation speed is secondary. Anything that significantly increases compile time at the cost of runtime speed is not acceptable by default.

This reflects real pain with tools like Verilator: compile times reaching hours, unreadable output, poor developer feedback.

## Priority Ordering

1. Iteration speed (build + run + debug)
2. Correctness (for supported subset)
3. Debuggability
4. Readable generated artifacts
5. Runtime performance
6. Peak optimization / research features

## Key Architectural Decisions

These follow from the architectural north stars; see [architecture-principles.md](architecture-principles.md) for the reasoning behind each.

### LLVM IR Backend

Primary backend generates LLVM IR. Native codegen with full optimization pipeline, no dependency on external C++ compiler.

### Elaboration Model

Compile-time elaboration via slang: hierarchy resolution, legality checks, name resolution, type checking.

### HIR/MIR as Templates

HIR/MIR represent module templates (compile per shape), not elaborated instance graphs. See [architecture-principles.md](architecture-principles.md) for the template-vs-instance design.

### Event-Driven Simulation (Default)

Event-driven / process-driven simulation. No global flattening, no global topo sort, no whole-design static scheduling.

## Constants and Parameters

- Compile-time constants must be resolved before simulation runtime
- Evaluation can occur in Slang, HIR, or MIR
- Never during simulation event loop

Loss of human semantics (e.g., `2 ** addr_width` becomes `4096`) is acceptable. Execution semantics take priority over preserving design intent.

## Type Parameters

`parameter type` requires code specialization. This is acceptable only when required. Default strategy: runtime parameters for values, specialization only for type parameters.
