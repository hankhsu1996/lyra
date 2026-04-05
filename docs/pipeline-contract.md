# Pipeline Contract

> Before editing, see [documentation-guidelines.md](documentation-guidelines.md). Architecture docs describe the target, not history. No "current state," migration plans, or queue references.

This document defines the behavioral boundaries between pipeline stages. It is not an architecture doc (which describes structure) - it enforces rules and guardrails.

## Correctness Authority

**MIR is the semantic truth.**

If MIR is correct, the language semantics are correct. LLVM IR and other backends are translators, not interpreters of SystemVerilog.

## Scope Authority

**All IR is specialization-scoped.**

HIR and MIR represent a single module specialization. No design-global slot IDs, no instance paths, no BFS instance ordering. See [compilation-model.md](compilation-model.md).

## Layer Responsibilities

### AST -> HIR

| Must Resolve           | Must NOT Do              |
| ---------------------- | ------------------------ |
| Syntax normalization   | Execution semantics      |
| Type resolution        | Control flow lowering    |
| Symbol binding         | Temporary introduction   |
| Source span capture    | Basic block construction |
| User error diagnostics | Design-global allocation |

HIR should still "look like SV", just cleaned and normalized.

### HIR -> MIR

| Must Resolve                     | Must NOT Do                |
| -------------------------------- | -------------------------- |
| Execution order                  | Re-interpret syntax        |
| Data flow (temporaries)          | Emit user diagnostics      |
| Control flow (basic blocks)      | Platform-specific lowering |
| Place/Value separation           | Design-global allocation   |
| System subroutine classification |                            |

MIR defines how the program executes. All semantic questions are answered here.

### MIR -> LLVM IR

| Must Do                            | Must NOT Do              |
| ---------------------------------- | ------------------------ |
| Translate MIR to LLVM              | Fix language semantics   |
| Emit runtime calls for complex ops | Re-interpret MIR meaning |
| Layout based on TypeId             | Create new types         |
|                                    | Emit user diagnostics    |

LLVM IR is not where language semantics live.

### Specialization -> Realization

| Must Do                                       | Must NOT Do                                           |
| --------------------------------------------- | ----------------------------------------------------- |
| Produce self-contained CompiledSpecialization | Reference design-global slot IDs                      |
| Use specialization-constant offsets only      | Embed instance paths in code                          |
| Classify parameters (structural vs value)     | Depend on instance count or ordering                  |
| Export SpecLayout, metadata, process info     | Require design-global knowledge                       |
| Keep heavy LLVM codegen specialization-scoped | Encode per-instance binding in LLVM functions/globals |

The specialization boundary is the key architectural invariant. Violations here break parallelism and incrementality. Per-instance binding should not appear in LLVM function or global identity. Heavy LLVM codegen shape should be determined by the number of unique specializations, not the number of instances.

### Realization -> Runtime

| Must Do                             | Must NOT Do                   |
| ----------------------------------- | ----------------------------- |
| Bind instances to specializations   | Recompile specialization code |
| Compute design state allocation     | Re-run LLVM optimization      |
| Build connectivity / trigger tables | Modify compiled kernels       |
| Construct per-instance const blocks | Depend on design flattening   |
| Produce instance path debug tables  |                               |

### Codegen -> Runtime

| Must Do                             | Must NOT Do                        |
| ----------------------------------- | ---------------------------------- |
| Emit calls into runtime API         | Invent scheduling semantics        |
| Load/store slots via this + offset  | Manage instance storage directly   |
| Reference slots as instance-local   | Assume global flat slot addressing |
| Delegate dirty/event/NBA to runtime | Implement tracing or subscription  |

Runtime is the sole owner of instance storage, dirty tracking, subscriptions, event queues, and NBA machinery. Slot identity is instance-local: the addressing contract is `this_base + specialization_constant_offset`.

## Information Flow

These must flow end-to-end through the pipeline:

| Information | Created In | Used By                          |
| ----------- | ---------- | -------------------------------- |
| SourceSpan  | AST -> HIR | Diagnostics, debugging           |
| TypeId      | HIR        | All stages (read-only after HIR) |
| ConstId     | HIR        | MIR, LLVM (materialization)      |
| SymbolId    | HIR        | MIR (variable resolution)        |
| ValueId     | HIR -> MIR | MIR, LLVM (temp mapping)         |

## Forbidden Cross-Layer Behavior

| Violation                           | Why It's Wrong                                                  |
| ----------------------------------- | --------------------------------------------------------------- |
| LLVM fixes SV semantics             | Semantics must be fixed in MIR                                  |
| MIR re-interprets syntax            | Syntax interpretation is HIR's job                              |
| HIR encodes execution               | Execution semantics belong in MIR                               |
| Post-HIR user diagnostics           | All user errors caught at AST -> HIR boundary                   |
| Backend creates types               | Types are language-level, owned by HIR                          |
| Backend manages storage             | Instance storage is owned by runtime                            |
| Backend invents scheduling          | Scheduling semantics belong in runtime                          |
| Design-global IDs in specialization | Breaks parallelism and incrementality                           |
| Instance paths in compiled code     | Instance binding belongs to design realization, not compilation |

## Forbidden Representation Shapes

These are concrete anti-models. Any design that produces these shapes is architecturally invalid, even if it passes tests. They represent the specific ways the flat design-global model re-emerges.

| Forbidden Shape                                                                                                      | Why It Is Wrong                                                                                                                                                                                      |
| -------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Resolving hierarchical refs to `kDesignGlobal` places during specialization compilation                              | Non-local targets are construction-time facts. Specialization code must use typed external-ref handles, not design-global slots.                                                                     |
| Resolving connections to final object endpoints (object_index, design-global slot) during specialization compilation | Connection targets are construction-time facts. Connections must be recipes referencing body-local identities (child binding site + local slot), not final topology coordinates.                     |
| Using design-global slot IDs as the semantic source of truth for non-local access                                    | Design-global slot IDs are an implementation detail of the runtime orchestration layer. They must not appear in compiled specialization artifacts or drive correctness of lowering.                  |
| Backend reconstruction of connection semantics by cloning body-local MIR into design-global processes                | If the backend must rebuild the old representation from the new one, the new representation is not yet the source of truth. The backend must consume recipes/artifacts directly.                     |
| Any representation whose correctness depends on final object ordering or topology materialization during compile     | Adding or removing instances must not invalidate compiled specialization artifacts. If it does, the representation has leaked topology into compilation.                                             |
| Representing non-local access as a parent-hop count or path of instance symbols                                      | Hop-count and symbol-path representations bake hierarchy shape into compiled code. Non-local access must use a single typed handle that is bound at construction time regardless of hierarchy depth. |

When evaluating a proposed design, check this table explicitly. The most common failure mode is a representation that uses the right vocabulary ("recipe", "deferred", "body-local") but still encodes final object identity internally.

## Error Boundaries

| Stage       | User Errors         | Compiler Bugs |
| ----------- | ------------------- | ------------- |
| AST -> HIR  | DiagnosticException | InternalError |
| HIR -> MIR  | N/A                 | InternalError |
| MIR -> LLVM | N/A                 | InternalError |

See [error-handling.md](error-handling.md) for details.

## Debugging Philosophy

- **Debug starts at MIR** - If MIR is correct, look at LLVM lowering
- **LLVM is assumed correct** unless proven otherwise
- **Always be able to dump**: HIR, MIR, LLVM IR

## Guiding Principle

> Build **one correct execution path first**.
> Everything else (optimizations, additional features) comes later.

This minimizes debugging surface area and prevents semantic drift.
