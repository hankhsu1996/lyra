# Compilation Model

The concrete data model for specialization-based compilation. Defines the types, identity rules, and invariants that all pipeline stages must respect.

For architectural motivation, see [architecture-principles.md](architecture-principles.md).

## Compilation Unit: Module Specialization

A **Module Specialization** is the unit of compilation and optimization. It is uniquely identified by:

```
ModuleSpecId = (ModuleDefId, BehaviorFingerprint)
```

| Component             | Definition                                                   |
| --------------------- | ------------------------------------------------------------ |
| `ModuleDefId`         | Source-level module definition identity                      |
| `BehaviorFingerprint` | Hash of all inputs that affect the compiled artifact's shape |

A specialization produces a self-contained, cacheable artifact: `CompiledModuleSpec`.

### C++ Analogy

| SystemVerilog         | C++ Equivalent                                       |
| --------------------- | ---------------------------------------------------- |
| Module definition     | Class template definition                            |
| Module specialization | Template specialization with concrete type arguments |
| Instance              | Object with a `this` pointer                         |
| Compile               | Compile a translation unit into `.o`                 |
| Assembly/Link         | Link objects + apply relocations                     |

## Elaboration-Time vs Execution-Time

Specialization boundaries are determined by differences in compiled behavior artifacts, not by differences in constructed design graphs.

**Elaboration-time** parameter effects build the design graph without changing compiled code:

- Unpacked container sizes
- Instance counts and topology
- Process instantiation decisions (which processes exist)
- Generate-controlled graph construction
- Connectivity wiring

These are resolved during elaboration. They do not create specialization boundaries.

**Execution-time** parameter effects change the compiled artifact:

- Packed bit widths, packed layout, signedness
- Arithmetic representation (operation widths, type coercions)
- Compiled process code shape (different instructions, different control flow)

These require specialization.

## Parameter Classification

Parameters are classified into three categories:

### Structural Parameters (part of specialization key)

A parameter is **structural** if changing it affects the compiled behavior artifact: packed widths, arithmetic representation, or compiled code shape. Structural parameters are part of `BehaviorFingerprint`.

### Elaboration-time Parameters

A parameter is **elaboration-time** if it affects only elaboration-time properties: unpacked container sizes, instance counts, process instantiation decisions, or generate-controlled graph construction. These are resolved during elaboration and do not create specialization boundaries.

### Value-Only Parameters (per-instance constants)

A parameter is **value-only** if it affects only runtime expressions (comparisons, display output, address calculations). It is stored in a per-instance `InstanceConstBlock` and read by compiled code at runtime.

**Example**: `BANK_ID` (0..15) used only in `$display` and address expressions. All 16 banks share one compiled specialization; each instance has a different const block.

### Dead Parameters

Parameters that are unused or optimized away. Must not affect the specialization key.

### Classification Rule

The compiler determines classification by examining what each parameter influences after elaboration. The `BehaviorFingerprint` is computed from execution-time results (packed layout, compiled code shape), not from raw parameter values. Two different parameter assignments that produce identical execution-time results map to the same specialization.

## Containers

A **container** is an unpacked array whose size is resolved during elaboration and whose storage location is determined by the runtime layout.

- Container size is elaboration-time metadata, not a specialization input
- Access uses `base_offset + index * element_stride` arithmetic
- Element type (and its compiled representation) remains specialization-scoped
- `int a[4]` and `int a[8]` in two instances of the same module share compiled code

Container size is a property of the constructed design state, not a type property for specialization purposes. See [state-layout.md](state-layout.md) for the arena-based storage model.

## Generate Semantics

A generate block requires specialization only if it changes the compiled behavior artifact. Otherwise it is resolved during elaboration.

Generate blocks that build the design graph (creating instances, instantiating processes, wiring connectivity) are elaboration-time operations. The process body is compiled independently; the generate block only decides whether that process object is created.

Generate blocks that change compiled code (different packed widths, different arithmetic representation inside branches) require specialization.

## Compiled Artifact: CompiledModuleSpec

A compiled specialization contains:

| Component           | Description                                                          |
| ------------------- | -------------------------------------------------------------------- |
| Specialization IR   | HIR and MIR, pointer-free, ID-based, specialization-scoped           |
| SpecLayout          | Slot list with sizes, alignments, offsets relative to `this_base`    |
| Code artifacts      | LLVM IR (or machine code) for processes/kernels, scoped to this spec |
| InstanceConstSchema | Types and positions of value-only parameters in the const block      |
| Metadata            | Source origins, process meta (names, kinds), read/write sets         |
| SpecHash            | Deterministic hash over all semantically relevant outputs            |

### Scope Rule

All contents of `CompiledModuleSpec` are **specialization-scoped**. No design-global numbering is allowed inside specialization artifacts.

## Identity Rules

### Two valid identity layers

**A) Specialization layer (compile-time)**

- `ModuleDefId`, `ModuleSpecId`
- `SymbolId`, `TypeId`, `ConstId`, `FileId`
- IR node IDs scoped to specialization (expression, statement, process, place, slot)
- `SpecLayout` offsets (constants within the specialization)

**B) Instance layer (assembly/runtime)**

- `InstanceId`
- Design connectivity (nets across instances)
- Design-level scheduling tables, runtime container indices
- Instance paths (strings for debugging/`%m`)

### Forbidden in specialization artifacts

No compile-time artifact may depend on design-global allocation:

- Design-global `SlotId` or state offsets
- Hierarchical instance paths
- Any numbering derived from BFS instance order or global instance count
- Runtime engine internals

### State access model

Within a specialization, state is addressed as:

```
this_base + specialization_constant_offset
```

Where `this_base` is an instance-specific runtime pointer and offsets are constants from `SpecLayout`.

## Assembly / Link

Assembly takes compiled specializations + an instance graph and produces a runnable design without recompiling specialization code.

### Inputs

- Instance graph (which instances exist, which specialization each uses)
- Connectivity graph (port wiring, continuous assigns between instances)
- `CompiledModuleSpec` for each required `ModuleSpecId`

### Outputs

| Output                | Description                                                          |
| --------------------- | -------------------------------------------------------------------- |
| InstanceTable         | For each instance: specialization reference + `this_base` plan       |
| DesignStateAllocation | Per-instance state segments derived from `SpecLayout`                |
| ConnectivityTables    | Connection descriptors, trigger/propagation tables between instances |
| InstanceConstBlocks   | Per-instance value-only parameter values                             |
| Debug tables          | Instance path strings (assembly-time only)                           |

### Assembly constraints

Assembly must NOT:

- Re-run LLVM optimization or regenerate kernel bodies
- Depend on full design flattening to create new IR
- Modify specialization code

Assembly may:

- Sort and build index tables
- Compute instance memory placements
- Create runtime lookup structures

### Incrementality

If only wiring or instance graph changes but module bodies do not, compiled specializations are reused and only assembly tables are rebuilt.

## Specialization-Local Optimizations

Within a `CompiledModuleSpec`, behavior may be transformed for performance as long as semantics are preserved. All such optimizations are specialization-local and must not require cross-module flattening.

| Optimization               | Description                                                        |
| -------------------------- | ------------------------------------------------------------------ |
| Kernelization              | Turn purely combinational processes into callable kernel functions |
| Connection batching        | Compile trivial assigns into a single propagation kernel           |
| Topo sorting               | Order internal dependency graph within the specialization          |
| Pure-comb module promotion | Compile stateless, timingless modules as pure function kernels     |
| Scheduling bypass          | Bypass scheduler for kernels that can be evaluated inline          |

Whole-design topo sorting is explicitly forbidden as a prerequisite for correctness.

## Determinism Contract

Required for caching, incrementality, and debugging:

- Stable ordering of specialization IDs, slots, processes, kernels, assembly tables
- Map/set iteration must use deterministic containers or explicit ordering
- `SpecHash` must be bit-for-bit reproducible across runs given identical inputs

## CI Enforcement

These invariants must be enforced automatically:

| Invariant                      | Description                                                                |
| ------------------------------ | -------------------------------------------------------------------------- |
| Specialization hash stability  | Same `ModuleSpecId` produces identical `SpecHash` across runs              |
| No design-global slots in spec | Static checks forbid design-global slot concepts in spec IR                |
| Incremental rebuild boundary   | Changing a leaf module recompiles only affected specializations            |
| Assembly reuse                 | Wiring-only changes reuse compiled specializations                         |
| Parallel build safety          | Multiple specializations compile concurrently without shared mutable state |

## Terminology

| Term                | Definition                                                               |
| ------------------- | ------------------------------------------------------------------------ |
| Module Definition   | Source-level `module M; ... endmodule`                                   |
| Specialization      | `(ModuleDefId, BehaviorFingerprint)` -- unit of compilation              |
| Instance            | Runtime object with `this_base`                                          |
| BehaviorFingerprint | Hash of execution-time inputs that affect the compiled artifact          |
| SpecLayout          | Specialization-scoped mapping from slot to offset                        |
| Assembly / Link     | Binding instances to specializations + connectivity tables               |
| InstanceConstBlock  | Per-instance storage for value-only parameters                           |
| Kernelization       | Transforming a process into an inline-callable kernel                    |
| Elaboration-time    | Properties resolved during elaboration (containers, topology, processes) |
| Execution-time      | Properties requiring specialization (packed widths, compiled code shape) |
| Container           | Unpacked array with elaboration-resolved size and layout metadata        |
| Arena               | Contiguous byte memory for design state, allocated after elaboration     |
