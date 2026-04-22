# Compiler Overview

## Purpose

Define the compiler's worldview: the pipeline, what each stage produces, and the boundary between
compile-time and runtime.

## Owns

- The identity of the pipeline layers: HIR, MIR, LIR, LLVM IR.
- The definition of a compilation unit as the primary semantic boundary of the compiler.
- The chosen semantic model: object-oriented. Hierarchy is an object graph; construction is
  constructor-like logic; navigation is object-graph traversal. The model is not C++; C++ is used
  only as a projection and reference language.
- The contract that separates semantic modeling (HIR, MIR) from execution modeling (LIR, LLVM IR).
- The contract that separates compile-time artifacts (class-level) from runtime artifacts
  (object-level).
- The positioning of MIR's projection: a readable view of MIR used for inspection, validation, and
  golden testing. Projection is a view, not a compilation path; it is not part of the lowering
  pipeline and does not produce executable artifacts. Projection must be semantically faithful to
  MIR and must not introduce or reinterpret semantics.

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
5. MIR remains human-readable and structurally meaningful through its projection. LIR does not
   reintroduce high-level semantics lost from MIR.
6. Lowering is one-way. A later stage does not write information back into an earlier stage.
7. The semantic model is object-oriented. Hierarchy is represented and navigated as an object graph;
   construction is constructor-like logic. The model is not C++ semantics; C++ is only a
   projection/reference language.
8. MIR's projection is a view, not a compilation path. It exists for inspection, debugging, and
   golden testing only. It is not consumed by any lowering step, and it must remain semantically
   faithful to MIR.

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
- Framing the semantic model as "C++ semantics" rather than "object-oriented semantics with a
  C++-like projection".
- Using MIR's projection as a lowering target, a backend input, or any step in the execution
  pipeline.
- A projection that diverges semantically from MIR or introduces meaning not present in MIR.

## Notes / Examples

Parameters on a compilation unit are constructor or config inputs. Per-instance data (wiring,
hierarchical position, parameter values) flows in at runtime construction. The compile-time model
remains class-level.
