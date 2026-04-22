# XIR Design

XIR (eXecution IR) is the execution-model IR. It makes all executable structure explicit and first-class before lower IRs commit to CFG plumbing.

## Pipeline Position

```
                        per compilation unit
                   +---------------------------------+
frontend output -> | HIR -> XIR -> MIR -> LLVM IR    | -> per-unit artifact
                   +---------------------------------+
```

HIR freezes _what the language means_.
XIR freezes _what the execution model is_ -- the object-level executable structure.
MIR freezes _how that structure maps to control flow and plumbing_.

## Scope

XIR is **per-compilation-unit**. Each XIR artifact describes one module specialization. No instance paths, no design-global slot IDs, no design-global allocation. See [compilation-model.md](compilation-model.md).

XIR is not merely "specialization-scoped" as a naming convention. It is a concrete artifact boundary: each specialization's XIR is independently producible and consumable, not a fragment of a larger design-wide XIR. No XIR lowering step consults design-global state. Frozen shared context (types, symbols) is read-only input, not a design-global compilation environment.

## Guiding Question

> Can this XIR be projected into a readable C++ class without adding new semantic interpretation during projection?

If projection requires the renderer to invent execution structure, reinterpret meaning, or consult external tables, the XIR is incomplete. The projection renders; it does not interpret.

## Why XIR Exists

HIR preserves source-level semantic structure. MIR commits to CFG-level plumbing (basic blocks, terminators, Place/Operand). Between these two layers, many execution-model concepts have no formal home:

- Closure/capture models for deferred actions
- Observer lifecycles (persistent change-triggered programs)
- Wait/trigger execution models (late-bound subscription, rebinding)
- Structured control flow at the execution level (not source-shaped, not yet CFG)
- Object composition (subobject ownership, constructor-time assembly)

Without a dedicated layer, these concepts are invented ad-hoc during lowering. Each feature re-derives its own execution structure, and the results drift toward design-global tables and ID-based orchestration because those are the available coordination mechanisms.

XIR exists so that execution-model concepts have a formal, reviewable, projectable home.

## Core Principles

These are hard rules, not guidelines:

| Rule                                       | Rationale                                                                         |
| ------------------------------------------ | --------------------------------------------------------------------------------- |
| Execution concepts are first-class objects | Closures, observers, wait models are named XIR constructs, not lowering artifacts |
| Structured control flow, not CFG           | XIR preserves executable structure; basic blocks and terminators belong in MIR    |
| Object-local ownership                     | State belongs to the compilation unit; no design-global lookup for owned data     |
| No design-global IDs or tables             | No BodyId, ModuleSpecId, InstanceId as architecture-bearing lookup keys           |
| Per-unit artifact boundary                 | Each XIR unit is independently producible; no design-wide XIR container           |
| Projectable to C++ object model            | Module ~ class, process ~ bound behavior, signal ~ member, hierarchy ~ subobjects |
| Projection renders, does not interpret     | If C++ projection must invent structure, XIR is incomplete                        |
| No scheduling or runtime policy            | Scheduling strategy is runtime's responsibility                                   |
| No LLVM/ABI details                        | Platform-independent; backends handle lowering                                    |

## Key Structural Concepts

### Compilation Unit as Object Description

A XIR compilation unit describes one specialization as an executable object:

- Member state (signals, variables, containers)
- Processes (object-bound behavior with lifetime and scheduling semantics)
- Subroutines (callable behavior, parameter passing, return)
- Subobject sites (child instantiation points with typed port contracts)
- External references (typed handles for non-local access, bound at construction time)
- Connection recipes (data-flow rules using object-local identities)

This is not an abstract grouping concept. It is a concrete artifact that the next layer (MIR) consumes as its sole input for one compilation unit.

### Structured Control Flow

XIR preserves structured control flow: if/else, case, loops, sequential blocks. These are execution-level structures, not source-level syntax preservation (that is HIR's job) and not CFG expansion (that is MIR's job).

The distinction: HIR preserves source shape for fidelity. XIR preserves execution shape for projectability. MIR lowers to basic blocks for backend consumption.

### Closure and Capture Models

Deferred actions, observer programs, and any construct that captures state for later execution are first-class XIR objects with explicit capture lists, callee identity, and invocation semantics.

A deferred assertion action in XIR is a closure: it names what it captures (by value or by reference), what it calls, and under what disposition. The lowering to thunk dispatch, payload layout, and ABI happens in MIR, not XIR.

### Wait and Trigger Models

Event sensitivity, delay, and subscription models are first-class XIR constructs. A wait specifies what signals are monitored, with what edge, and what happens on trigger -- as structured executable objects, not as CFG terminators with resume-block metadata.

The lowering to bytecode index plans, subscription slot tracking, and trigger installation happens in MIR, not XIR.

### Observer Lifecycles

Persistent observers ($monitor, $strobe) are first-class XIR objects with explicit setup/check/teardown lifecycle, snapshot semantics, and change-detection contracts. They are not synthesized as pairs of anonymous functions during lowering.

## The C++ Projection

The C++ projection is a methodology tool, not a product backend. Its purpose is to make XIR's architectural shape visible and reviewable.

A well-formed XIR compilation unit should project into something resembling:

- A class declaration (member state, process methods, subroutine methods)
- Closure objects for deferred actions (captured state, invocation)
- Structured control flow in method bodies (if/else, loops, case)
- Subobject members for child instantiation sites
- Typed handle members for external references

If XIR contains a concept that does not project cleanly, that is evidence the XIR shape is wrong. The projection is the architectural oracle.

The projection is not a second product backend. It does not need to compile or execute. It needs to be readable and to make architectural shape visible.

## What Must NOT Appear in XIR

| Excluded                                | Belongs in               |
| --------------------------------------- | ------------------------ |
| Basic blocks / CFG edges                | MIR                      |
| Terminators (Jump, Branch, Switch)      | MIR                      |
| Place/Operand separation                | MIR                      |
| Temporaries / SSA / value numbering     | MIR                      |
| LLVM intrinsics / ABI details           | LLVM lowering            |
| Scheduling queues / execution policy    | Runtime                  |
| Design-global IDs as lookup keys        | Never in per-unit IR     |
| Frontend pointers                       | Never                    |
| Thunk dispatch / payload layout         | MIR or backend           |
| Bytecode plans / subscription tracking  | MIR or backend           |
| Design-wide container of multiple units | Never -- XIR is per-unit |

If you find yourself adding CFG-level plumbing to XIR, step back -- you are modeling implementation, not execution structure. If you find yourself adding a design-wide XIR container, step back -- the compilation unit is the artifact boundary.

## Relationship to Other Layers

| Layer   | Freezes                                            | Key representation                       |
| ------- | -------------------------------------------------- | ---------------------------------------- |
| HIR     | What the language means (semantic)                 | Source-shaped, owned, normalized         |
| XIR     | What the execution model is (executable structure) | Object-shaped, structured, projectable   |
| MIR     | How it maps to control flow and plumbing           | CFG, Place/Operand, terminators, effects |
| LLVM IR | How it maps to machine execution                   | SSA, intrinsics, ABI                     |

## Summary

XIR is the execution-model IR. It sits between semantic HIR and plumbing-oriented MIR. Its job is to make executable structure -- closures, observers, wait models, object composition, structured control flow -- explicit, named, and projectable into a C++ object model. Each XIR unit is a concrete per-compilation-unit artifact. If a concept cannot be projected cleanly, the XIR shape is wrong.
