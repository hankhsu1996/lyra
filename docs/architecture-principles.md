# Architecture Principles

> Before editing, see [documentation-guidelines.md](documentation-guidelines.md). Architecture docs describe the target, not history. No "current state," migration plans, or queue references.

Why the system is structured the way it is. These principles drive every major design decision and resolve trade-offs when goals conflict.

For product-level priorities, see [philosophy.md](philosophy.md). For coding patterns, see [design-principles.md](design-principles.md). For the concrete data model, see [compilation-model.md](compilation-model.md).

## North Stars

### The Natural Model

A module is a type. An instance is an object of that type. Signals are its members. Processes are its behavior. This is not an analogy -- it is the foundational mental model for Lyra's entire architecture. See [natural-model.md](natural-model.md) for the canonical definition, ownership phases, and regression checks.

Everything follows from this:

- **Instance state is object-local.** An instance owns its state. Member access is naturally object pointer + local offset.
- **Design-global systems are outer-layer orchestration.** Scheduler, time, event queues, cross-instance wiring are coordination concerns, not the core representation of what an instance is.
- **Connectivity does not redefine object shape.** Wiring is linkage or reference, not something that changes what state an instance owns or how it is laid out.
- **Body/specialization facts stay body-shaped.** Compiled behavior and layout for a specialization are stable properties of that specialization. Design topology must not leak back and redefine instance representation.
- **Processes are behavior attached to instances.** They run against their instance's state. Suspension is resumable behavior state, not a separate global concern.

This principle is the first filter for every design decision. If a proposed change makes instances less like objects or makes state less object-local, it moves away from the target.

### Parallelism

Compilation scales with the number of specializations (unique structural parameter configurations), not instances. A design with 1000 instances of the same module compiles code once, not 1000 times.

The expensive parts -- IR construction, LLVM IR generation, LLVM optimization, code emission -- must run in parallel across independent specializations with no shared mutable global state.

### Incrementality

Changes invalidate the minimum possible scope. This requires:

- **Stable keys** -- identifiers that survive edits to unrelated code
- **Small invalidation surface** -- changing one module recompiles only its affected specializations (and explicit dependents), not the entire design
- **Strict layering** -- each pipeline stage depends only on its input, never on stages before or after its predecessor
- **Realization reuse** -- if module bodies are unchanged, compiled specializations are reused and only realization tables are rebuilt

### Performance

Correct scaling laws matter more than constant-factor optimizations:

- **Compile time** scales with unique specializations, not total instances
- **Runtime access** is O(1) via `this_base` + specialization-constant offset, not tree traversal
- **Scheduling** is incremental (dirty-driven), not whole-design per cycle

### Lifecycle and Correctness

Correctness comes from structure, not discipline:

- **Single ownership** -- every piece of mutable state has exactly one owner
- **Single write paths** -- each field is written by exactly one code path
- **Strong invariants** -- type system and structure enforce rules that "be careful" comments cannot
- **Fail-fast** -- detect violations at the earliest possible point (compile time > lowering time > runtime)

## Derived Structural Rules

These are consequences of the north stars, not goals in themselves.

### Compile per Specialization, Realize per Design, Run per Instance

`ModuleSpecId = (ModuleDefId, BehaviorFingerprint)` is the compile-time unit. Code is generated once per specialization -- this is compiling the class, not instantiating objects. During design realization, instances are constructed from compiled specializations and connectivity is wired. At runtime, each instance owns its own state and the shared specialization code operates on it via `this_base` + relative offset -- this is methods running on objects.

This follows from **the natural model** (specialization = type, instance = object), **parallelism** (compile units are specializations), and **performance** (O(1) storage access).

### IDs as Incrementality and Correctness Tools

Strongly-typed IDs (`ModuleSpecId`, `SymbolId`, `TypeId`, etc.) serve two purposes:

- **Incrementality** -- stable identity across recompilation; downstream stages reference IDs, not pointers
- **Correctness** -- type safety prevents mixing IDs from different domains; the type system catches misuse at compile time

This follows from **incrementality** (stable keys) and **lifecycle** (strong invariants).

### Specialization-Scoped IR, Instances at Realization/Runtime

HIR, MIR, and LLVM IR are internal to specialization compilation and are specialization-scoped. No instance paths, no design-global slot IDs, no design-global allocation. Instance creation, storage allocation, and hierarchy wiring happen at realization/runtime. The IR never duplicates code for structurally identical instances.

Per-instance binding should not appear in LLVM function or global identity. Heavy LLVM codegen shape -- function count, global count, and optimization work -- should be determined by the number of unique specializations, not the number of instances. Instance-specific constants (base byte offset, instance ID, per-instance slot offsets) belong in runtime-owned data materialized at construction time.

This follows from **the natural model** (instances are objects, not coordinates into a global arena), **parallelism** (IR scales with specializations), and **incrementality** (specialization changes do not cascade through instance graphs).

### Specialization-Local Optimizations Only

All compile-time optimizations (kernelization, topo sorting, connection batching, pure-comb module promotion) must be specialization-local. Cross-module flattening or whole-design topo sorting is forbidden as a prerequisite for correctness.

This follows from **parallelism** (specializations compile independently) and **incrementality** (local changes have local effects).

### Compile-Owned vs Constructor-Owned Separation

Specialization boundaries are determined by differences in compile-owned facts (packed widths, compiled code shape), not by differences in the constructed design graph. Constructor-owned properties (container sizes, instance counts, process instantiation, connectivity) are resolved during realization without recompilation. Only compile-owned properties require distinct specializations. See [compilation-model.md](compilation-model.md) for the full classification and type ownership model.

This follows from **parallelism** (fewer specializations = better parallelism) and **incrementality** (constructor-owned changes don't force recompilation).

### No Final Object Identity in Specialization Compilation

Specialization compilation must not materialize final object identity. No compiled artifact may depend on:

- Final instance count or object ordering (BFS, DFS, or any flattened enumeration)
- Final hierarchy shape (which instances actually exist after generate evaluation)
- Final object identity of any non-local target (object index, endpoint address, design-global slot ID)
- Final connectivity topology (which specific instance a port is wired to)

"Deferred to realization" is necessary but not sufficient. The compiled intermediate representation must make early resolution **structurally impossible** -- non-local targets must be represented as typed handles or recipes that can only be bound at construction time, not as placeholders that happen to be filled in later.

This follows from **the natural model** (instances are objects constructed at realization, not compile-time coordinates), **parallelism** (specialization compilation must not require the full design graph), and **incrementality** (topology changes must not force recompilation).

### Structural vs Value-Only Parameter Split

Within compile-owned parameters, specialization identity is based on structural effects (packed layout, compiled code shape), not raw parameter values. Parameters that affect only runtime expressions are stored as per-instance constants.

This follows from **parallelism** (fewer specializations = better parallelism) and **incrementality** (value-only parameter changes don't force recompilation).

## Decision Checklist

For any design change, ask in order:

1. **Does it keep instances as objects?** Specifically:
   - Does it introduce a new design-global coordinate as the primary identity for instance-local state?
   - Does it allow connectivity topology to change an instance's storage shape or layout?
   - Does it bypass object-local access and reintroduce direct design-global addressing for instance members?
   - Does it dissolve instance identity into flat design-global metadata?
   - Does it make compiled body code depend on design topology?
2. **Does it preserve parallel compilation units?** -- Can specializations still be compiled independently?
3. **Does it keep invalidation small and dependencies explicit?** -- Does changing X force recompilation of unrelated Y?
4. **Does it keep the scaling law right?** -- Does cost grow with specializations or with instances?
5. **Are ownership and invariants enforced by structure?** -- Or does correctness depend on "be careful"?
6. **Is it specialization-local?** -- Does it require cross-module or design-global knowledge?

7. **Does it avoid materializing final object identity at compile time?** -- Does it require knowing final instance count, object ordering, or target object identity during specialization compilation?

If a proposed change fails any of these questions, reconsider the design before proceeding.

## Applied Examples

How existing decisions follow from these principles:

- **HIR/MIR as specialization-scoped IR** (not instance graphs) -> parallelism: compile per specialization
- **`this_base` + relative offset** for variable access -> performance: O(1) access without hierarchy traversal
- **Strongly-typed IDs throughout the pipeline** -> incrementality + correctness: stable references, no cross-domain misuse
- **Dirty-driven scheduling** -> performance: incremental work, not whole-design recomputation
- **Single-owner mutable state** in runtime -> lifecycle: no aliasing bugs, clear write paths
- **Strict pipeline layering** (HIR -> MIR -> LLVM) -> incrementality: each stage depends only on its input
- **Kernelization within specialization** -> performance: scheduler bypass without cross-module coupling
- **Value-only parameters as instance constants** -> parallelism: 16 banks = 1 specialization, not 16
- **Unpacked sizes as elaboration-time metadata** -> parallelism: `int a[4]` and `int a[8]` share compiled code
- **Generate-for as elaboration, not specialization** -- incrementality: adding instances doesn't recompile parent
