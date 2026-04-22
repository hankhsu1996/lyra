# Pipeline Contract

> Before editing, see [documentation-guidelines.md](documentation-guidelines.md). Architecture docs describe the target, not history. No "current state," migration plans, or queue references.

This document defines the behavioral boundaries between pipeline stages. It is not an architecture doc (which describes structure) -- it enforces rules and guardrails.

## Correctness Authority

Four layers of truth, each freezing a different concern:

| Layer   | What it freezes                                    | Authority scope       |
| ------- | -------------------------------------------------- | --------------------- |
| HIR     | What the language means (semantic facts)           | Source-level meaning  |
| XIR     | What the execution model is (executable structure) | Execution-model truth |
| MIR     | How execution maps to control flow (CFG plumbing)  | Control-flow truth    |
| LLVM IR | How control flow maps to machine execution         | Platform translation  |

If XIR correctly represents the executable structure and MIR correctly lowers it to control flow, the language semantics are correct. LLVM IR and other backends are translators, not interpreters of SystemVerilog.

## Scope Authority

**All IR is per-compilation-unit.**

HIR, XIR, and MIR each represent a single module specialization -- the compiler's compilation unit. No design-global slot IDs, no instance paths, no BFS instance ordering. See [compilation-model.md](compilation-model.md).

The compiler does not adopt the frontend's whole-design elaboration world. At the AST-to-HIR boundary, the frontend output is broken into per-specialization compilation units. After that boundary, lowering proceeds per-unit without consulting design-global state.

## Compilation Unit Boundary

The AST-to-HIR boundary is where the compiler establishes its own compilation units:

| Happens here                                         | Does NOT happen here          |
| ---------------------------------------------------- | ----------------------------- |
| Frontend output broken into per-specialization units | Design-global lowering        |
| Stable IDs replace frontend pointers                 | Instance-specific compilation |
| Data ownership transferred to compiler               | Topology-dependent decisions  |
| User errors diagnosed (last chance)                  | Execution-model structure     |

After this boundary, the compiler's world is per-unit. Design-global facts (package/global state, type arenas, symbol tables) exist as frozen shared read-only context, not as a mutable whole-design compilation environment.

## Layer Responsibilities

### AST -> HIR (per unit)

| Must Resolve                      | Must NOT Do              |
| --------------------------------- | ------------------------ |
| Syntax normalization              | Execution structure      |
| Type resolution                   | Control flow lowering    |
| Symbol binding                    | Temporary introduction   |
| Source span capture               | Basic block construction |
| User error diagnostics            | Design-global allocation |
| Specialization unit establishment |                          |

HIR should still "look like SV", just cleaned and normalized. HIR owns stable IDs and data independent from frontend memory. Some execution-relevant semantic shaping (capture classification, system call classification, assignment target canonicalization) is appropriate where it preserves language-level meaning that downstream layers need.

### HIR -> XIR (per unit)

| Must Resolve                                        | Must NOT Do                     |
| --------------------------------------------------- | ------------------------------- |
| Execution-model structure                           | CFG / basic block construction  |
| Closure/capture models for deferred actions         | Place/Operand separation        |
| Observer lifecycles (setup/check/teardown)          | Temporary introduction          |
| Wait/trigger execution models                       | Scheduling / runtime policy     |
| Structured executable control flow                  | Design-global consultation      |
| Object composition (subobject sites, external refs) | Thunk dispatch / payload layout |

XIR makes execution structure explicit and first-class. All execution-model concepts have a formal home here. The compilation unit is a self-contained executable object description. XIR must be projectable into a C++ object/behavior model for architectural review; if the projection must invent structure, XIR is incomplete. See [xir-design.md](xir-design.md).

### XIR -> MIR (per unit)

| Must Resolve                                     | Must NOT Do                   |
| ------------------------------------------------ | ----------------------------- |
| Control flow (basic blocks, terminators)         | Re-interpret execution model  |
| Data flow (temporaries)                          | Emit user diagnostics         |
| Place/Operand separation                         | Platform-specific lowering    |
| Suspension mechanics (Delay/Wait as terminators) | Design-global consultation    |
| Effect and side-effect plumbing                  | Invent new execution concepts |

MIR defines how structured execution maps to control-flow plumbing. All execution concepts should already be formalized in XIR; MIR lowers them to CFG form. See [mir-design.md](mir-design.md).

### MIR -> LLVM IR (per unit)

| Must Do                            | Must NOT Do              |
| ---------------------------------- | ------------------------ |
| Translate MIR to LLVM              | Fix language semantics   |
| Emit runtime calls for complex ops | Re-interpret MIR meaning |
| Layout based on TypeId             | Create new types         |
|                                    | Emit user diagnostics    |

LLVM IR is not where language semantics or execution-model structure live.

### Per-Unit Compilation -> Artifact Composition

| Must Do                                          | Must NOT Do                    |
| ------------------------------------------------ | ------------------------------ |
| Package per-unit artifacts into deliverable      | Recompile specialization code  |
| Combine with elaborated design topology metadata | Re-run LLVM optimization       |
| Produce constructor input data                   | Return to semantic elaboration |
| Keep per-unit boundaries intact                  | Merge or flatten per-unit IRs  |

Artifact composition is a compile-time design-wide pass. It packages per-unit outputs with topology metadata for the constructor. It is not a return to semantic elaboration or a new link-time compilation engine.

### Constructor / Realization -> Runtime

| Must Do                                     | Must NOT Do                   |
| ------------------------------------------- | ----------------------------- |
| Bind instances to specializations           | Recompile specialization code |
| Construct per-instance storage              | Re-run LLVM optimization      |
| Build connectivity / trigger tables         | Modify compiled artifacts     |
| Construct per-instance const blocks         | Depend on design flattening   |
| Execute generate-driven structure selection |                               |
| Produce instance path debug tables          |                               |

The constructor builds the runtime object graph at runtime (or load time for AOT). Generate-driven structural realization happens here, not at compile time. This is object construction, not compilation.

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
| ConstId     | HIR        | XIR, MIR, LLVM (materialization) |
| SymbolId    | HIR        | XIR, MIR (variable resolution)   |

## Forbidden Cross-Layer Behavior

| Violation                                      | Why It's Wrong                                                                  |
| ---------------------------------------------- | ------------------------------------------------------------------------------- |
| LLVM fixes SV semantics                        | Semantics must be fixed before LLVM                                             |
| MIR invents execution concepts                 | Execution model belongs in XIR                                                  |
| MIR re-interprets execution structure          | XIR is the execution-model truth                                                |
| XIR encodes CFG plumbing                       | CFG lowering belongs in MIR                                                     |
| HIR encodes execution structure                | Execution structure belongs in XIR                                              |
| Post-HIR user diagnostics                      | All user errors caught at AST -> HIR boundary                                   |
| Backend creates types                          | Types are language-level, owned by HIR                                          |
| Backend manages storage                        | Instance storage is owned by runtime                                            |
| Backend invents scheduling                     | Scheduling semantics belong in runtime                                          |
| Design-global IDs in per-unit IR               | Breaks compilation-unit isolation                                               |
| Instance paths in compiled code                | Instance binding belongs to constructor, not compilation                        |
| Per-unit lowering consults design-global state | Breaks per-unit independence; only frozen read-only shared context is permitted |

## Forbidden Representation Shapes

These are concrete anti-models. Any design that produces these shapes is architecturally invalid, even if it passes tests.

| Forbidden Shape                                                                       | Why It Is Wrong                                                                                                                                                 |
| ------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Resolving hierarchical refs to `kDesignGlobal` places during per-unit compilation     | Non-local targets are construction-time facts. Per-unit code must use typed external-ref handles, not design-global slots.                                      |
| Resolving connections to final object endpoints during per-unit compilation           | Connection targets are construction-time facts. Connections must be recipes referencing body-local identities.                                                  |
| Using design-global slot IDs as the semantic source of truth for non-local access     | Design-global slot IDs are an implementation detail of the runtime orchestration layer. They must not appear in compiled per-unit artifacts.                    |
| Per-unit lowering code consulting design-global orchestration tables                  | If lowering requires whole-design state beyond frozen read-only context (types, symbols), the compilation-unit boundary is violated.                            |
| Lowering code inventing execution-model structure without XIR representation          | If an execution concept (closure, observer, wait model) is synthesized during MIR lowering without first being named in XIR, the pipeline is missing structure. |
| Any representation whose correctness depends on final object ordering or topology     | Adding or removing instances must not invalidate compiled per-unit artifacts.                                                                                   |
| Compilation-unit IDs used as lookup handles into design-global tables during lowering | IDs like ModuleSpecId name a unit. They must not become surrogate handles for recovering design-global architecture state.                                      |
| Representing non-local access as a parent-hop count or path of instance symbols       | Non-local access must use a single typed handle bound at construction time.                                                                                     |
| Artifact composition that re-enters semantic elaboration                              | Post-compilation design-wide passes must package, not recompile or re-derive semantics.                                                                         |

## Error Boundaries

| Stage       | User Errors         | Compiler Bugs |
| ----------- | ------------------- | ------------- |
| AST -> HIR  | DiagnosticException | InternalError |
| HIR -> XIR  | N/A                 | InternalError |
| XIR -> MIR  | N/A                 | InternalError |
| MIR -> LLVM | N/A                 | InternalError |

See [error-handling.md](error-handling.md) for details.

## Debugging Philosophy

- **Debug starts at XIR** -- If XIR correctly represents execution structure, look at MIR lowering
- **Then MIR** -- If MIR is correct, look at LLVM lowering
- **LLVM is assumed correct** unless proven otherwise
- **Always be able to dump**: HIR, XIR, MIR, LLVM IR

## Guiding Principle

> Build **one correct execution path first**.
> Everything else (optimizations, additional features) comes later.

This minimizes debugging surface area and prevents semantic drift.
