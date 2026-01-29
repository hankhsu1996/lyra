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

### LLVM IR Backend

Primary backend generates LLVM IR.

Reasons:

- Native codegen with full optimization pipeline
- Direct control over machine code generation
- No dependency on external C++ compiler
- IR designed for LLVM lowering (no function pointers, no VM bytecode)

### Elaboration Model

Lyra uses compile-time elaboration via slang:

- Slang elaborates hierarchy at compile time
- Each variable gets a unique symbol pointer
- Variable access: flat lookup by symbol (no instance traversal)

Slang performs legality checks, name resolution, and validation.

### HIR/MIR Role

HIR/MIR are module templates, not elaborated instance graphs.

They encode:

- Parameter interfaces
- Behavioral semantics (always/assign)
- Scheduling semantics (comb/ff/NBA)

They do not encode:

- Fully elaborated instance graphs
- Static per-instance specialization

HIR is the high-level IR close to SystemVerilog semantics. MIR is the mid-level IR suitable for LLVM lowering.

### Event-Driven Simulation (Default)

Default mode:

- Event-driven / process-driven simulation
- No global flattening
- No global topo sort
- No whole-design static scheduling

Optional fast modes (future):

- Local comb sorting
- Partial specialization
- Hot-path optimization

## Constants and Parameters

- Compile-time constants must be resolved before simulation runtime
- Evaluation can occur in Slang, HIR, or MIR
- Never during simulation event loop

Loss of human semantics (e.g., `2 ** addr_width` becomes `4096`) is acceptable. Execution semantics take priority over preserving design intent.

## Type Parameters

`parameter type` requires code specialization. This is acceptable only when required. Default strategy: runtime parameters for values, specialization only for type parameters.
