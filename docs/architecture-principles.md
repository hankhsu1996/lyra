# Architecture Principles

Why the system is structured the way it is. These principles drive every major design decision and resolve trade-offs when goals conflict.

For product-level priorities, see [philosophy.md](philosophy.md). For coding patterns, see [design-principles.md](design-principles.md). For the concrete data model, see [compilation-model.md](compilation-model.md).

## North Stars

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

`ModuleSpecId = (ModuleDefId, BehaviorFingerprint)` is the compile-time unit. Code is generated once per specialization. During design realization, instances are bound to compiled specializations and connectivity tables are built. At runtime, each instance owns its own state and the shared specialization code operates on it via `this_base` + relative offset.

This follows from **parallelism** (compile units are specializations) and **performance** (O(1) storage access).

### IDs as Incrementality and Correctness Tools

Strongly-typed IDs (`ModuleSpecId`, `SymbolId`, `TypeId`, etc.) serve two purposes:

- **Incrementality** -- stable identity across recompilation; downstream stages reference IDs, not pointers
- **Correctness** -- type safety prevents mixing IDs from different domains; the type system catches misuse at compile time

This follows from **incrementality** (stable keys) and **lifecycle** (strong invariants).

### Specialization-Scoped IR, Instances at Realization/Runtime

HIR and MIR are internal to specialization compilation and are specialization-scoped. No instance paths, no design-global slot IDs, no design-global allocation. Instance creation, storage allocation, and hierarchy wiring happen at realization/runtime. The IR never duplicates code for structurally identical instances.

This follows from **parallelism** (IR scales with specializations) and **incrementality** (specialization changes do not cascade through instance graphs).

### Specialization-Local Optimizations Only

All compile-time optimizations (kernelization, topo sorting, connection batching, pure-comb module promotion) must be specialization-local. Cross-module flattening or whole-design topo sorting is forbidden as a prerequisite for correctness.

This follows from **parallelism** (specializations compile independently) and **incrementality** (local changes have local effects).

### Compile-Owned vs Constructor-Owned Separation

Specialization boundaries are determined by differences in compile-owned facts (packed widths, compiled code shape), not by differences in the constructed design graph. Constructor-owned properties (container sizes, instance counts, process instantiation, connectivity) are resolved during realization without recompilation. Only compile-owned properties require distinct specializations. See [compilation-model.md](compilation-model.md) for the full classification and type ownership model.

This follows from **parallelism** (fewer specializations = better parallelism) and **incrementality** (constructor-owned changes don't force recompilation).

### Structural vs Value-Only Parameter Split

Within compile-owned parameters, specialization identity is based on structural effects (packed layout, compiled code shape), not raw parameter values. Parameters that affect only runtime expressions are stored as per-instance constants.

This follows from **parallelism** (fewer specializations = better parallelism) and **incrementality** (value-only parameter changes don't force recompilation).

## Decision Checklist

For any design change, ask in order:

1. **Does it preserve parallel compilation units?** -- Can specializations still be compiled independently?
2. **Does it keep invalidation small and dependencies explicit?** -- Does changing X force recompilation of unrelated Y?
3. **Does it keep the scaling law right?** -- Does cost grow with specializations or with instances?
4. **Are ownership and invariants enforced by structure?** -- Or does correctness depend on "be careful"?
5. **Is it specialization-local?** -- Does it require cross-module or design-global knowledge?

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
