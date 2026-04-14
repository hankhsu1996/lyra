# Runtime Object Model

> Before editing, see [documentation-guidelines.md](documentation-guidelines.md). Architecture docs describe the target, not history. No "current state," migration plans, or queue references.

How the natural model materializes as concrete C++ runtime types and APIs.

For the conceptual model, see [natural-model.md](natural-model.md). For compilation artifacts and recipes, see [compilation-model.md](compilation-model.md). For the scheduler and suspension model, see [runtime.md](runtime.md). For byte-level storage layout, see [state-layout.md](state-layout.md). For dirty tracking and subscriptions, see [change-propagation.md](change-propagation.md).

## Core Principle

The runtime is a typed C++ object library. It owns instance lifecycle, process binding, connection wiring, external-reference resolution, hierarchy metadata, and scheduling integration.

LLVM-generated code interacts with the runtime in two ways:

- **Runtime helper calls** for operations with ownership, coordination, or scheduler interaction
- **Direct field reads** from a minimal binary-contract subset of `RuntimeInstance` where hot-path execution requires it

Generated code does not rebuild the object model. It does not manage instance storage, implement scheduling, or resolve non-local targets. It executes against runtime-owned objects and runtime-populated bindings.

Low-level carriers may exist inside the runtime as derived hot-path machinery. They are not architectural contracts. The architectural contracts are object-first, typed, and pointer-based.

## RuntimeInstance

`RuntimeInstance` is the first-class runtime object. One object exists per module instance. `RuntimeInstance*` is the semantic and operational anchor: ownership, binding, dispatch, hierarchy, and tracing all flow through it.

Numeric instance identity is not part of the core model. No object-layer API depends on numeric instance identity. If a code path appears to require a numeric ID, that indicates the pointer or typed reference was lost upstream and the data flow should be restructured so the producer passes the object directly.

### Owns

- Inline and appendix storage regions for module-local mutable state
- External-reference binding array, one entry per external-reference recipe in the body
- Attached behavior set for the instance
- Hierarchical path string for `%m`, tracing, and other hierarchy-derived presentation

### References

- Shared body descriptor / specialization template
- Other instances only through typed bindings, typed structural relations, or runtime-owned linkage records

### Field categories

| Category         | Meaning                                                                                  | Examples                                                  |
| ---------------- | ---------------------------------------------------------------------------------------- | --------------------------------------------------------- |
| Binary contract  | Stable layout readable by generated code where hot-path execution requires direct access | body, storage, path_c_str, ext_ref_bindings               |
| Runtime-internal | Owned entirely by the runtime library; not a generated-code contract                     | observability, event_state, fixpoint_scratch, dedup_state |

The binary contract is intentionally minimal. Only fields that generated code must read at execution time belong in it. Everything else remains runtime-internal.

## Constructor Assembly

The constructor is a structured object-graph assembler.

It is not a flat replay machine that iterates entries and reconstructs relationships afterward from numeric carriers. It assembles the hierarchy in parent context, creating live objects and materializing bindings directly against those live objects.

At each step, the constructor knows:

- the parent object
- the child specialization or body
- the relevant recipes and metadata
- the structural context in which the child is being created

Construction therefore produces concrete object relationships immediately. External references, connections, and process bindings are installed against live `RuntimeInstance*` pointers during assembly, not deferred for later pointer recovery.

### Assembly operations

| Operation         | Input                                                             | Effect                                                                                                              |
| ----------------- | ----------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------- |
| CreateChild       | specialization, parent context, storage recipe, constants         | allocate child `RuntimeInstance`, initialize storage, install per-instance constants, establish structural relation |
| BindExternalRef   | handle ordinal, concrete target `(RuntimeInstance*, LocalSlotId)` | populate one entry in the instance's external-reference binding array                                               |
| InstallConnection | connection recipe, concrete source/destination/trigger endpoints  | materialize one concrete connection and register its trigger behavior                                               |
| InstallProcess    | process artifact, owning instance                                 | attach behavior to the instance and initialize its runtime binding anchor                                           |

### Parent-child context

Parent-child context is structurally required. Child creation, port binding, non-local resolution, connection wiring, and hierarchy-derived naming all depend on assembling a child while the parent context is available.

Generate-controlled structure is expressed by which constructor operations execute in a given structural context. The constructor does not flatten this away into primary numeric identity.

### Output

The primary output is an assembled object graph:

- `RuntimeInstance` objects
- populated storage
- established parent-child structural relations
- attached behavior
- materialized external-reference bindings
- materialized concrete connection records
- hierarchy-derived path ownership

A flat traversal or ownership container may exist as a runtime-internal convenience. It is not the semantic representation. The semantic representation is the assembled object graph.

See [compilation-model.md](compilation-model.md) for recipe and artifact definitions.

## Process Binding

A process is behavior attached to a `RuntimeInstance`.

The ownership direction is instance -> attached behavior. The scheduler schedules activations, but an activation is always behavior executing on a particular object.

### Semantic ownership

Each instance owns the set of processes attached to it. Processes do not exist as free-standing semantic entities detached from an owning object.

The exact carrier used to track that set is an implementation choice. It may be a range, array, list, or another runtime-internal representation. That choice is not part of the semantic model.

### Binding anchor

Each realized process carries:

- a pointer to its owning `RuntimeInstance`
- a shared body function pointer for the `(body, process)` pair
- any runtime-owned suspension / resume state needed for execution

All process binding fields are written during construction or runtime initialization. Generated code reads them during execution and does not rewrite the binding relationship.

### Dispatch contract

Shared body functions are the canonical dispatch shape. Dispatch is shared across instances of the same body/process pair.

No per-instance LLVM wrappers, dispatch trampolines, or per-instance codegen artifacts define process identity.

## Connection Materialization

Connections have two architectural stages:

| Stage        | Carrier                            | Identity model                              |
| ------------ | ---------------------------------- | ------------------------------------------- |
| Compile-time | `ConnectionRecipe`                 | body-local identities only                  |
| Runtime      | concrete runtime connection record | direct object pointers and local identities |

At compile time, a connection is represented as a recipe using body-local carriers only. It does not contain final object identity.

At construction time, the constructor resolves the recipe against the live object graph and materializes a concrete runtime connection record. That runtime record contains direct object references for all participating endpoints.

How compile-time artifacts are transported into the constructor is a codegen boundary detail, not a conceptual architecture stage.

### Runtime connection record

A concrete runtime connection record carries everything needed for propagation without further identity recovery:

- source object reference
- destination object reference
- trigger-owner object reference where applicable
- byte offsets into object-owned storage
- local signal identity for trigger subscription
- copy size and other propagation metadata

### Hot-path cache

The orchestration layer may derive low-level propagation caches from the concrete runtime connection record, such as precomputed byte pointers or batched descriptors. These are derived caches for inner loops. They are not the primary representation.

## External Reference Resolution

The closure-capture model from [natural-model.md](natural-model.md) materializes as a per-instance external-reference binding array on `RuntimeInstance`.

Each binding entry resolves one external-reference recipe to a concrete target:

- target storage address within the target instance
- target object reference for behavioral coordination
- target local signal identity

Resolution happens at construction time. Runtime execution reads bindings through the owning instance and the populated binding array. Non-local access therefore remains object-first and pointer-based.

## Hierarchy and Scope

Hierarchy is a first-class structural concept at runtime.

It is not defined by a flat instance table, path strings, or numeric object lookup. Those may exist as derived runtime conveniences, but they do not define the model.

Generate scopes have identity. Hierarchical naming, durable descendant paths, `%m`, and `$scope` all require the runtime to preserve structural hierarchy semantics.

This does not require every scope to become a heavyweight runtime object. It does require the runtime representation to preserve enough structure that hierarchy is directly representable and navigable.

### Hard requirements

- **Parent-child relations are directly representable and walkable.** The runtime must support structural navigation, not flat recovery from strings or object numbers.
- **Generate-scope identity is preserved.** `RepertoireCoord` and `DurableChildId` remain meaningful structural carriers that resolve against the live hierarchy.
- **Durable-path resolution walks structure.** Construction-time non-local binding navigates the hierarchy step by step.
- **Path is derived, not primary.** Instance path is materialized from hierarchy position. It does not define the hierarchy.
- **Hierarchy-derived naming is built from structure.** `%m`, `$scope`, and trace naming all derive from the structural position of an instance.

### Path ownership

Each `RuntimeInstance` owns its hierarchical path string.

- `path_storage`: runtime-owned string storage
- `path_c_str`: stable pointer into that owned storage for binary-contract reads where needed

The constructor builds the path while assembling the hierarchy, because it already has the parent context and the child's structural identity.

## Design-Global Orchestration Boundary

The runtime has two layers with a strict boundary.

### Object layer

The object layer defines what an instance is.

- `RuntimeInstance` owns state, bindings, path, and attached behavior
- process binding points back to the owning instance
- constructor assembles the object graph with typed operations
- instance-facing APIs use `RuntimeInstance*` and typed local carriers

### Orchestration layer

The orchestration layer coordinates objects. It does not define them.

- scheduler queues and time advancement
- dirty propagation and wakeup filtering
- connection propagation machinery
- registries, tracing tables, observable metadata, and similar coordination systems

The orchestration layer may use low-level carriers for efficiency. Those carriers remain outer-layer implementation details. They must not leak back into the object layer or define constructor APIs, instance APIs, binding APIs, or hierarchy semantics.

## Hot-Path Primitives

Some inner loops require low-level representations:

- scheduling queues
- dirty-bit sets and byte-range arithmetic
- propagation batches with precomputed raw pointers
- cached slot-access primitives for fixpoint execution

These are acceptable only when all of the following hold:

1. They live inside the orchestration layer, not the object layer
2. They are derived from high-level runtime-owned objects and bindings
3. They do not define constructor, binding, hierarchy, or instance-facing API shape
4. The object-first model remains the source of truth

## What Must NOT Appear

| Anti-pattern                                                  | Why it is wrong                                                                                                                            |
| ------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| Numeric instance identity in the core model                   | The core anchor is `RuntimeInstance*`. Numeric identity indicates the object reference was lost or the boundary was flattened incorrectly. |
| ID-to-pointer recovery as a normal access path                | Recovering a pointer from a numeric carrier means the producer failed to pass the object directly.                                         |
| Codegen rebuilding the object model in ABI structs            | Generated code executes against runtime-owned objects; it does not reimplement lifecycle, binding, or hierarchy.                           |
| Low-level carriers defining constructor or binding APIs       | Constructor and binding APIs are typed and object-first. They do not take flat numeric carriers as their primary model.                    |
| Per-instance LLVM functions, globals, or dispatch trampolines | Codegen scales with bodies / specializations, not runtime object count.                                                                    |
| Process ownership detached from `RuntimeInstance`             | Module behavior is attached to an owning instance.                                                                                         |

## LLVM Integration Contract

### Boundary principle

Generated code calls into the runtime library for operations with ownership, coordination, or scheduler consequences. Direct field reads from `RuntimeInstance` are allowed only for a deliberately small binary contract where hot-path access justifies avoiding a helper call.

### Codegen may do

1. Call runtime helpers for suspend, subscribe, store-and-dirty, NBA scheduling, and similar coordination operations
2. Read a minimal binary-contract subset of `RuntimeInstance`
3. Reference compile-time artifacts emitted as constants and consumed by runtime construction or initialization

### Codegen must not do

- allocate or initialize instance storage
- implement scheduling or dirty-tracking policy
- create per-instance codegen artifacts
- resolve non-local targets directly
- write binding fields after construction / initialization
- reconstruct hierarchy or ownership relationships in IR

## Related

- [natural-model.md](natural-model.md) -- canonical definitions
- [compilation-model.md](compilation-model.md) -- recipes, artifacts, construction instruction model
- [runtime.md](runtime.md) -- scheduler, suspension, engine API
- [state-layout.md](state-layout.md) -- byte-level storage layout
- [change-propagation.md](change-propagation.md) -- dirty tracking, subscriptions, wakeup
- [pipeline-contract.md](pipeline-contract.md) -- layer boundaries and forbidden shapes
