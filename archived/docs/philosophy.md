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

## Elaboration-Time vs Execution-Time

Many simulators treat all parameter effects as compile-time specialization triggers. This leads to specialization explosion and long compile times.

Lyra observes that most parameter effects are **elaboration-time**: they build the design graph (instances, processes, containers, connectivity) without changing compiled code. Only a smaller set of effects are **execution-time**: they change the compiled artifact itself (packed widths, arithmetic representation).

Lyra exploits this distinction to minimize the specialization space. See [compilation-model.md](compilation-model.md) for the full definition.

## Key Architectural Decisions

These follow from the architectural north stars; see [architecture-principles.md](architecture-principles.md) for the reasoning behind each.

### LLVM IR Backend

Primary backend generates LLVM IR. Native codegen with full optimization pipeline, no dependency on external C++ compiler.

### Specialization-Based Compilation

Compilation unit is the module specialization (`ModuleSpecId`), not the elaborated design. Compile per specialization, realize per design, run per instance. See [compilation-model.md](compilation-model.md) for the data model and [architecture-principles.md](architecture-principles.md) for the reasoning.

### Elaboration Model

Compile-time elaboration via slang: hierarchy resolution, legality checks, name resolution, type checking. Elaboration determines which specializations are needed and the instance graph.

### HIR/XIR/MIR as Specialization-Scoped IR

HIR, XIR, and MIR are internal to specialization compilation. They contain no instance paths, no design-global slot IDs. See [architecture-principles.md](architecture-principles.md).

### Event-Driven Simulation (Default)

Event-driven / process-driven simulation. No global flattening, no global topo sort, no whole-design static scheduling.

## Constants and Parameters

- Compile-time constants must be resolved before simulation runtime
- Evaluation can occur in Slang, HIR, or MIR
- Never during simulation event loop

Loss of human semantics (e.g., `2 ** addr_width` becomes `4096`) is acceptable. Execution semantics take priority over preserving design intent.

## Type Parameters

`parameter type` requires code specialization. This is acceptable only when required. Default strategy: runtime parameters for values, specialization only for type parameters.
