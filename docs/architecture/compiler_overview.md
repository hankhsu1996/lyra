# Compiler Overview

## Purpose

Define the compiler's worldview: the pipeline, what each stage produces, and the boundary between
compile-time and runtime.

## Owns

- The identity of the pipeline layers: HIR, MIR, LIR, LLVM IR.
- The definition of a compilation unit as the primary semantic boundary of the compiler.
- The chosen semantic model: object-oriented. Hierarchy is an object graph; construction is
  constructor-like logic; navigation is object-graph traversal. The model is not C++; C++ is one of
  the backend targets but does not define MIR's semantics.
- The contract that separates semantic modeling (HIR, MIR) from execution modeling (LIR, LLVM IR).
- The contract that separates compile-time artifacts (class-level) from runtime artifacts
  (object-level).
- The positioning of the C++ backend: a transitional realization of the MIR consumer. The
  architectural target is HIR -> MIR -> LIR -> LLVM IR; the C++ backend exists today because LIR and
  the LLVM IR backend are not yet built. The C++ backend's output serves two roles -- an executable
  artifact, and the human-readable surface against which MIR's shape is validated from SystemVerilog
  source through MIR's semantic model. This transitional status does **not** loosen the
  mechanical-translation discipline: the C++ backend's render must remain mechanical at the LLVM-IR
  level (no decision logic in render, every entry a fixed function of one MIR node), so the same MIR
  feeds an eventual LLVM IR backend without rework. `backend_contract.md` owns this discipline; this
  doc owns its position in the pipeline. Debug inspection of HIR and MIR is separate: dumpers
  produce textual traversals for reading and golden testing, while `backend::cpp` is the real
  emitter. Dumpers and backends are both pure over their input IR and must not introduce or
  reinterpret semantics.

## Does Not Own

- The internal contract of any single IR layer (see `hir.md`, `mir.md`, `lir.md`).
- The details of LLVM lowering and codegen.
- Runtime scheduling and simulation-engine design.

## Core Invariants

1. The compiler pipeline is HIR -> MIR -> LIR -> LLVM IR. No stage skips a layer.
2. Each IR layer has a distinct role. Semantic modeling lives in HIR and MIR. Execution modeling
   lives in LIR and below.
3. A compilation unit is the top-level semantic boundary. Kinds of compilation unit include module,
   package, and interface.
4. Compile-time produces class-level artifacts: shape, code, and metadata. Runtime constructs
   objects and installs relations.
5. MIR remains human-readable through its dumper; LIR does not reintroduce high-level semantics lost
   from MIR.
6. Lowering is one-way. A later stage does not write information back into an earlier stage.
7. The semantic model is object-oriented. Hierarchy is represented and navigated as an object graph;
   construction is constructor-like logic. The model is not C++ semantics; C++ is one backend target
   among possibly several.
8. HIR and MIR dumpers are debug-facing textual serialization. They are not compilation paths and
   are not consumed by any lowering step. They must remain semantically faithful to their input IR.
9. Backend emitters (today: `backend::cpp`) consume MIR and produce executable artifacts. A backend
   is a first-class compilation stage, not a debug view. Every backend render entry is a fixed
   function of one MIR node; decision logic in render is a MIR design failure (see
   `backend_contract.md`). The C++ backend's transitional status as the current observation surface
   for MIR sharpens this discipline rather than relaxing it: a place where the C++ render would need
   decision logic is a place where the eventual LLVM IR backend would too.

## Boundary to Adjacent Layers

- Upstream: the slang frontend produces elaborated-unit shape from SystemVerilog source. The
  compiler consumes slang's output and begins at HIR.
- Downstream: LLVM IR is emitted for execution backends (AOT, JIT). The runtime owns object
  construction and scheduling once LLVM IR is linked.

## Forbidden Shapes

- A pipeline stage that consumes and produces the same IR layer.
- Skipping an IR layer (for example, HIR to LIR directly).
- Embedding runtime execution logic in HIR.
- Reconstructing high-level semantics in LIR.
- Treating the elaborated design as a compilation unit.
- Compile-time stages that depend on per-instance identity.
- Framing the semantic model as "C++ semantics" rather than "object-oriented semantics emitted by
  one or more backends".
- A dumper that diverges semantically from its input IR or introduces meaning not present in the IR.
- Treating a dumper's text as a backend artifact, or treating a backend's output as a debug view.
- Trading the C++ backend's emitted readability for its compile time. Emit readability is how MIR's
  semantic correctness is validated; compile-time wins must come from the runtime library, build
  infrastructure (precompiled headers, parallel compilation), or backend-internal organization that
  does not change the emitted form.
- Treating the C++ backend as a permanent realization that justifies non-mechanical render. The C++
  backend is transitional; the MIR shape it consumes must be the same MIR an eventual LLVM IR
  backend consumes. A render entry that would need extra logic in LLVM IR is a render entry that is
  already wrong today; the MIR is the suspect, not the render.

## Notes / Examples

A compilation unit is a class. Compile-time produces the class definition -- its members, its
parameter signature, its constructor logic, and its callable bodies. Runtime executes the
constructor to instantiate objects and runs the callables under scheduler control to drive behavior.
The constructor expands generate `if` / `for` / `case` to build the object graph; processes
(`initial`, `always*`) are the class's runtime callables.

Concretely, `module Foo #(parameter N = 4) (...);` defines a class with an `N` constructor
parameter. `for (genvar i = 0; i < N; ...) ...` is a `for` loop in that class's constructor.
`if (N > 0) ...` is an `if` in that constructor. `initial begin ... end` is a class method
dispatched by the scheduler at simulation time zero. See `runtime_model.md` for the contract that
keeps the constructor context and the simulation context distinct.

Parameters on a compilation unit are constructor or config inputs. Per-instance data (wiring,
hierarchical position, parameter values) flows in at runtime construction. The compile-time model
remains class-level.
