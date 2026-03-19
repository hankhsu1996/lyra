# Compilation Model

The concrete data model for specialization-based compilation. Defines the types, identity rules, and invariants that all pipeline stages must respect.

For architectural motivation, see [architecture-principles.md](architecture-principles.md).

## Pipeline Phases

Four distinct phases with clear boundaries:

1. **Elaboration** -- discover the design: module definitions, instances, parameters, hierarchy, connectivity
2. **Specialization Compilation** -- compile reusable behavior units, one per `ModuleSpecId`, parallelizable
3. **Design Realization** -- materialize the executable runtime image from compiled specializations + elaborated design topology
4. **Execution** -- run the simulation

Each phase answers a different question:

- Elaboration: _What is the design?_
- Specialization Compilation: _How does each reusable behavior class execute?_
- Design Realization: _How does this particular design become a runnable image?_
- Execution: _Run it._

**Compile time** covers elaboration and specialization compilation. These produce design-independent, cacheable artifacts. **Design realization** is a distinct phase that takes compiled specializations and the elaborated design topology and materializes the executable runtime image -- instance placement, connectivity binding, container sizing, metadata construction. It is not compile time (it requires the full design graph) and not execution (the simulation has not started). **Execution** is the simulation itself.

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
| Design Realization    | Construct the object graph + wire references         |

## Specialization Boundary Rule

A specialization exists only when a parameter changes the reusable compiled behavior or the low-level representation in a way that affects code shape.

**Specialization inputs** (require distinct compiled artifacts):

- Packed bit widths, packed layout, signedness
- Arithmetic representation (operation widths, type coercions)
- Compiled process code shape (different instructions, different control flow)

**Realization inputs** (do not create specialization boundaries):

- Unpacked container sizes
- Instance counts and topology
- Process instantiation decisions (which processes exist)
- Generate-controlled graph construction
- Connectivity wiring
- Per-instance constants (value-only parameters)

The clearest example is packed data. Packed widths are tightly coupled to the LLVM-level representation (`i8`, `i16`, `i32`, etc.), operation lowering, and bit-level semantics. When a packed shape changes, that is a real specialization boundary.

Unpacked/container size is different. A change in unpacked extent is handled during design realization, not specialization compilation. The right mental model is not `std::array<N>`-style type explosion but a runtime container model: descriptor-driven storage, vector-like regions, small-buffer optimization where profitable, and runtime materialization of container size.

Generate-controlled graph construction is not by itself a specialization boundary; it becomes one only when it changes reusable compiled behavior or packed low-level representation.

The goal: a small number of specializations. Specialization count should not explode because elaboration produced many different unpacked sizes or design-specific container shapes.

### Decision Rule

For any parameter or shape difference, ask:

> Does this change the reusable compiled behavior or packed low-level representation?

If yes: specialization boundary.
If no (only changes container realization, runtime sizing, placement, connectivity, or the realized object graph): handled by realization.

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

The compiler determines classification by examining what each parameter influences after elaboration. The `BehaviorFingerprint` is computed from compile-owned facts (packed layout, compiled code shape), not from raw parameter values. Two different parameter assignments that produce identical compile-owned facts map to the same specialization.

## Type Ownership

A single SystemVerilog type declaration (e.g., `logic [7:0] data [0:3]`) encodes facts that belong to different owners in the compilation model. Lyra separates these into distinct ownership worlds:

### Compile-owned type facts

Facts that affect compiled artifact identity and code shape. These are inputs to specialization grouping and are baked into generated code.

- Packed bit width, signedness, two-state vs four-state
- Packed array element type and packed range
- Packed struct/union field layout (types, bit offsets, bit widths)
- Enum base type and member values
- Unpacked container element type (the compiled access pattern depends on it)

Two instances with different compile-owned type facts require different specializations.

### Constructor-owned type facts

Facts resolved when the design is constructed (realization phase). They affect instance layout and container sizing but not compiled code.

- Unpacked array dimensions (number of elements)
- Queue maximum bound
- Instance counts and topology
- Generate-controlled existence of artifacts

Two instances with different constructor-owned facts but identical compile-owned facts share one specialization. Their differences are resolved during realization.

### Runtime-owned state

Mutable simulation state that changes during execution.

- Field values
- Container contents
- String values
- Dynamic array / queue storage

### Why this matters

These are not "two unrelated type systems." They are one semantic truth projected into owner-specific views. The `TypeArena` in `common/` serves the runtime/codegen world (HIR, MIR, layout, interpreter). The `CompileOwnedTypeStore` in `lowering/ast_to_hir/` captures compile-owned type facts for specialization identity. Both derive from the same frontend types but capture different subsets of information.

Specialization identity requires compile-owned facts only. Constructor-owned and runtime-owned facts must not split specializations. This separation is a first-class architectural principle, not an implementation detail of one subsystem.

## Containers

A **container** is an unpacked array whose size is resolved during elaboration and whose storage location is determined by the runtime layout.

- Container size is elaboration-time metadata, not a specialization input
- Access uses `base_offset + index * element_stride` arithmetic
- Element type (and its compiled representation) remains specialization-scoped
- `int a[4]` and `int a[8]` in two instances of the same module share compiled code

Container size is a property of the constructed design state, not a type property for specialization purposes. See [state-layout.md](state-layout.md) for the arena-based storage model.

## Specialization as Artifact Library

A specialization is not a specific execution path through the generate tree. It is a **compiled artifact library** -- the complete repertoire of all artifacts that the definition can produce, compiled once and reused by any instance regardless of constructor-time selections.

### Mental model

```
Specialization  = compiled artifact library (all possible artifacts)
Constructor     = selects / instantiates / binds from the library
Generate        = frontend syntax for constructor-time assembly decisions
```

### Concrete example

```systemverilog
module Top #(
    parameter int MODE = 0,
    parameter int STAGES = 3,
    parameter bit USE_ACC = 1,
    parameter int WIDTH = 32
) (...);

  logic [WIDTH-1:0] pipe [0:STAGES-1];
  logic [WIDTH-1:0] acc;

  if (MODE == 0) begin
    always_comb pipe[0] = in + 1;
  end else begin
    always_comb pipe[0] = in ^ 'h55;
  end

  for (genvar i = 1; i < STAGES; i++) begin
    always_ff @(posedge clk or negedge rst_n) begin
      if (!rst_n) pipe[i] <= '0;
      else        pipe[i] <= pipe[i-1];
    end
  end

  if (USE_ACC) begin
    always_ff @(posedge clk or negedge rst_n) begin
      if (!rst_n) acc <= '0;
      else        acc <= acc + pipe[STAGES-1];
    end
    assign out = acc;
  end else begin
    assign out = pipe[STAGES-1];
  end
endmodule
```

The specialization owns the full artifact library:

- storage template: `pipe` (element type `logic [WIDTH-1:0]`)
- storage template: `acc` (type `logic [WIDTH-1:0]`)
- process artifact: `pipe0_mode_add` (`always_comb pipe[0] = in + 1`)
- process artifact: `pipe0_mode_xor` (`always_comb pipe[0] = in ^ 'h55`)
- process template: `pipeline_reg(i)` (the `always_ff` body, parameterized over index)
- process artifact: `accumulate` (the accumulator `always_ff`)
- connection artifact: `out_from_acc` (`assign out = acc`)
- connection artifact: `out_from_pipe` (`assign out = pipe[STAGES-1]`)

All eight artifacts are compiled. The constructor selects which subset to install:

```
construct(spec, params):
  allocate pipe with size params.STAGES
  allocate acc
  if params.MODE == 0: install spec.pipe0_mode_add
  else:                 install spec.pipe0_mode_xor
  for i in 1..params.STAGES-1: install spec.pipeline_reg(i)
  if params.USE_ACC:    install spec.accumulate, bind spec.out_from_acc
  else:                 bind spec.out_from_pipe
```

### What this means for specialization identity

Only WIDTH creates a different specialization (different packed bit width = different compiled artifacts). MODE, STAGES, and USE_ACC are constructor-time selections that do not change the artifact library:

- MODE=0 vs MODE=1: same library, constructor installs different process
- STAGES=3 vs STAGES=5: same library, constructor instantiates different count
- USE_ACC=1 vs USE_ACC=0: same library, constructor enables different subset

Two instances share a specialization when their artifact libraries have identical compile-owned facts, regardless of which artifacts the constructor selects.

## Generate Semantics

Generate constructs are frontend syntax for constructor-time assembly decisions. They describe which artifacts exist in each instance, not which artifacts are compiled.

A generate block requires specialization only when it changes compile-owned facts (packed widths, arithmetic representation, compiled code shape). Otherwise it is a constructor-time operation resolved during design realization.

The key distinction: generate does not control **what is compiled**. The specialization compiles all artifacts from all branches. Generate controls **what is installed** -- which artifacts the constructor selects for a particular instance.

## Compiled Artifact: CompiledModuleSpec

A compiled specialization is a self-contained artifact library. It contains compiled code for all artifacts the definition can produce -- not just the artifacts selected by one particular instance.

| Component           | Description                                                               |
| ------------------- | ------------------------------------------------------------------------- |
| Specialization IR   | HIR and MIR for all artifacts, pointer-free, ID-based                     |
| SpecLayout          | Slot list with sizes, alignments, offsets relative to `this_base`         |
| Code artifacts      | LLVM IR (or machine code) for all process/kernel artifacts in the library |
| InstanceConstSchema | Types and positions of value-only parameters in the const block           |
| Metadata            | Source origins, process meta (names, kinds), read/write sets              |
| SpecHash            | Deterministic hash over all semantically relevant outputs                 |

### Scope Rule

All contents of `CompiledModuleSpec` are **specialization-scoped**. No design-global numbering is allowed inside specialization artifacts.

## Identity Rules

### Two valid identity layers

**A) Specialization layer (compile-time)**

- `ModuleDefId`, `ModuleSpecId`
- `SymbolId`, `TypeId`, `ConstId`, `FileId`
- IR node IDs scoped to specialization (expression, statement, process, place, slot)
- `SpecLayout` offsets (constants within the specialization)

**B) Instance layer -- realization and runtime**

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

## Design Realization

Realization takes compiled specializations + the elaborated design graph and materializes a runnable design image without recompiling specialization code.

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
| Debug tables          | Instance path strings (realization-time only)                        |

### Realization constraints

Realization must NOT:

- Re-run LLVM optimization or regenerate kernel bodies
- Depend on full design flattening to create new IR
- Modify specialization code
- Generate per-instance LLVM functions, globals, or types

Realization may:

- Sort and build index tables
- Compute instance memory placements
- Create runtime lookup structures
- Build per-instance descriptors that reference shared compiled code

### Instance-Independence of Compiled Artifacts

Changing instance counts, generate expansion, or topology must not materially change the LLVM codegen workload. The expected compile-time scaling property:

- **Specialization compilation** scales with the number of unique specializations (unique `ModuleSpecId` values). Adding more instances of the same specialization does not increase compilation cost.
- **Realization/construction** scales with total instance count. This is runtime construction work: allocating state, building per-instance metadata, wiring connectivity. This cost is expected and acceptable.

The boundary rule: per-instance binding must not appear in LLVM function or global identity. Heavy LLVM codegen shape -- function count, global count, and optimization work -- must be determined by the number of unique specializations, not the number of instances. Instance-specific constants (base byte offset, instance ID, signal ID offset, per-instance slot offset tables) belong in runtime-owned data materialized at construction time.

Process and comb dispatch must carry instance-specific binding via runtime-owned realization data, not per-instance LLVM wrapper functions. Per-instance wrapper generation in LLVM IR is not the target architecture.

Per-instance data (such as unstable slot offset tables for parameterized specializations) must be runtime-owned memory populated at construction time, not per-instance LLVM globals.

### Incrementality

If only wiring or instance graph changes but module bodies do not, compiled specializations are reused and only realization tables are rebuilt.

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

- Stable ordering of specialization IDs, slots, processes, kernels, realization tables
- Map/set iteration must use deterministic containers or explicit ordering
- `SpecHash` must be bit-for-bit reproducible across runs given identical inputs

## CI Enforcement

These invariants must be enforced automatically:

| Invariant                      | Description                                                                |
| ------------------------------ | -------------------------------------------------------------------------- |
| Specialization hash stability  | Same `ModuleSpecId` produces identical `SpecHash` across runs              |
| No design-global slots in spec | Static checks forbid design-global slot concepts in spec IR                |
| Incremental rebuild boundary   | Changing a leaf module recompiles only affected specializations            |
| Realization reuse              | Wiring-only changes reuse compiled specializations                         |
| Parallel build safety          | Multiple specializations compile concurrently without shared mutable state |

## Terminology

| Term                | Definition                                                                     |
| ------------------- | ------------------------------------------------------------------------------ |
| Module Definition   | Source-level `module M; ... endmodule`                                         |
| Specialization      | `(ModuleDefId, BehaviorFingerprint)` -- compiled artifact library              |
| Instance            | Runtime object with `this_base`                                                |
| BehaviorFingerprint | Hash of compile-owned inputs that affect the compiled artifact                 |
| SpecLayout          | Specialization-scoped mapping from slot to offset                              |
| Design Realization  | Materializing the executable runtime image from specializations + design graph |
| InstanceConstBlock  | Per-instance storage for value-only parameters                                 |
| Kernelization       | Transforming a process into an inline-callable kernel                          |
| Elaboration-time    | Properties resolved during elaboration (containers, topology, processes)       |
| Compile-owned       | Type/behavior facts requiring specialization (packed widths, code shape)       |
| Constructor-owned   | Type facts resolved at realization (unpacked dimensions, container sizing)     |
| Container           | Unpacked array with elaboration-resolved size and layout metadata              |
| Arena               | Contiguous byte memory for design state, allocated after elaboration           |
