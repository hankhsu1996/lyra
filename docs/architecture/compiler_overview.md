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
- The positioning of the C++ backend: a first-class MIR consumer that emits C++ code. Debug
  inspection of HIR and MIR is separate: dumpers produce textual traversals for reading and golden
  testing, while `backend::cpp` is the real emitter. Dumpers and backends are both pure over their
  input IR and must not introduce or reinterpret semantics.

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
   is a first-class compilation stage, not a debug view.

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

## Notes / Examples

Parameters on a compilation unit are constructor or config inputs. Per-instance data (wiring,
hierarchical position, parameter values) flows in at runtime construction. The compile-time model
remains class-level.
