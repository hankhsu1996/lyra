# Architecture Principles

Why the system is structured the way it is. These principles drive every major design decision and resolve trade-offs when goals conflict.

For product-level priorities, see [philosophy.md](philosophy.md). For coding patterns, see [design-principles.md](design-principles.md).

## North Stars

### Parallelism

Compilation scales with the number of definitions/shapes, not instances. A design with 1000 instances of the same module compiles the same code once, not 1000 times. Compilation units are shapes (unique parameter configurations), not objects (runtime instances).

### Incrementality

Changes invalidate the minimum possible scope. This requires:

- **Stable keys** - identifiers that survive edits to unrelated code
- **Small invalidation surface** - changing one module does not recompile the world
- **Strict layering** - each pipeline stage depends only on its input, never on stages before or after its predecessor

### Performance

Correct scaling laws matter more than constant-factor optimizations. This means:

- **Compile time** scales with unique shapes, not total instances
- **Runtime access** is O(1) via base pointer + relative offset, not tree traversal
- **Scheduling** is incremental (dirty-driven), not whole-design per cycle

### Lifecycle and Correctness

Correctness comes from structure, not discipline. This means:

- **Single ownership** - every piece of mutable state has exactly one owner
- **Single write paths** - each field is written by exactly one code path
- **Strong invariants** - type system and structure enforce rules that "be careful" comments cannot
- **Fail-fast** - detect violations at the earliest possible point (compile time > lowering time > runtime)

## Derived Structural Rules

These are consequences of the north stars, not goals in themselves.

### Compile per Shape, Run per Object

`ModuleVariant` and `ProcessTemplate` are the compile-time sharing units. Code is generated once per unique shape (parameter configuration). At runtime, each `Instance` owns its own storage and the shared template code operates on it via `this_ptr` + relative offset.

This follows from **parallelism** (compile units are shapes) and **performance** (O(1) storage access).

### IDs as Incrementality and Correctness Tools

Strongly-typed IDs (`ModuleVariantId`, `ProcessSlotId`, etc.) serve two purposes:

- **Incrementality** - stable identity across recompilation; downstream stages reference IDs, not pointers
- **Correctness** - type safety prevents mixing IDs from different domains; the type system catches misuse at compile time

This follows from **incrementality** (stable keys) and **lifecycle** (strong invariants).

### Templates in IR, Instances at Runtime

HIR and MIR represent module templates, not elaborated instance graphs. Instance creation, storage allocation, and hierarchy wiring happen at runtime. The IR never duplicates code for structurally identical instances.

This follows from **parallelism** (IR scales with shapes) and **incrementality** (template changes do not cascade through instance graphs).

## Decision Checklist

For any design change, ask in order:

1. **Does it preserve parallel compilation units?** - Can shapes still be compiled independently?
2. **Does it keep invalidation small and dependencies explicit?** - Does changing X force recompilation of unrelated Y?
3. **Does it keep the scaling law right?** - Does cost grow with shapes or with instances?
4. **Are ownership and invariants enforced by structure?** - Or does correctness depend on "be careful"?

If a proposed change fails any of these questions, reconsider the design before proceeding.

## Applied Examples

How existing decisions follow from these principles:

- **HIR/MIR as module templates** (not instance graphs) -> parallelism: compile per shape, not per instance
- **`this_ptr` + relative offset** for variable access -> performance: O(1) access without hierarchy traversal
- **Strongly-typed IDs throughout the pipeline** -> incrementality + correctness: stable references, no cross-domain misuse
- **Dirty-driven scheduling** -> performance: incremental work, not whole-design recomputation
- **Single-owner mutable state** in runtime -> lifecycle: no aliasing bugs, clear write paths
- **Strict pipeline layering** (HIR -> MIR -> LLVM) -> incrementality: each stage depends only on its input
