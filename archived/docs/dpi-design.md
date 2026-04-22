# DPI-C Design

Foreign function interface between SystemVerilog and C/C++ code, per IEEE 1800-2023 Chapter 35.

## Architectural Position

DPI-C is a **foreign-ABI subsystem**. It is not a special case of user-defined function calls. It has its own semantic properties (pure, context, import/export direction, C name override), its own type mapping (SV types to C ABI types), and its own calling convention (no Lyra-internal implicit parameters).

The subsystem spans four layers:

| Layer  | Responsibility                                                                |
| ------ | ----------------------------------------------------------------------------- |
| HIR    | Semantic normalization of slang DPI metadata into Lyra-owned declarations     |
| MIR    | Explicit foreign-call intent with resolved C symbol name and DPI signature    |
| LLVM   | DPI ABI type lowering and symbol materialization (separate from internal ABI) |
| Driver | Link/load orchestration for user-provided libraries (no embedded C compiler)  |

## Hard Rules

1. **Dual ABI boundary.** Internal ABI (DesignState, Engine, instance context) and DPI ABI (plain C types, no simulator parameters) are separate. Never leak one into the other.
2. **DPI is a first-class IR concept.** DPI declarations and calls have dedicated representation at HIR and MIR level. They are not flags on generic subroutine or call nodes.
3. **External compilation.** Lyra does not compile C/C++ source. Users compile their DPI code with their own toolchain. Lyra provides ABI metadata (headers, symbol names) and handles linkage.
4. **Import and export are separate.** Import (Lyra calls external C) and export (external C calls into Lyra) share DPI type mapping but differ in every other dimension: control flow direction, wrapper generation, context capture, linkage model.
5. **DPI ABI owns boundary marshaling.** All temporary buffers and type conversions at the foreign boundary are owned by the DPI ABI layer. Lyra internal packed/4-state storage layouts are never exposed as-if they were DPI storage. The internal representation is an implementation detail that the DPI layer translates, not passes through.
6. **Normalized declarations are the single source of truth.** All DPI boundary behavior is defined in terms of normalized HIR DPI declarations plus DPI ABI classification. No downstream layer re-reads original slang syntax or inspects internal LLVM function shapes to derive DPI semantics.

## What Must NOT Appear

- C/C++ parsing, preprocessing, or compilation inside the Lyra pipeline
- DPI metadata derived from slang AST types in the LLVM layer (must be normalized at HIR)
- DPI calls routed through the user-function or system-TF call path in MIR
- Internal ABI parameters (DesignState*, Engine*, this_ptr) in DPI function signatures
- Dual execution models (one path for DPI, one for non-DPI) in the same call lowering code
- Internal packed/4-state storage layout leaking into DPI call arguments or return values
- DPI semantics derived by re-reading slang syntax or by inspecting internal LLVM function signatures

## Semantic Model (HIR)

DPI declarations are first-class HIR nodes, separate from regular subroutine declarations.

Each DPI import declaration captures:

- SV function name
- C function name (explicit override or defaulting to SV name)
- Pure vs context vs default property
- Function vs task kind
- Return type (SV semantic type)
- Parameter list with direction and SV semantic type per argument

Each DPI export declaration captures:

- SV function name being exported
- C function name (explicit override or defaulting to SV name)
- Function vs task kind

The HIR layer normalizes slang's representation (MethodFlags, syntax-level c_identifier token) into these self-contained declarations. After HIR, no downstream layer accesses slang types for DPI information.

## Foreign-Call Representation (MIR)

MIR owns an explicit **foreign-call family** for DPI, separate from user-function, design-function, and system-TF call paths. The foreign-call representation preserves the full foreign-boundary semantics through lowering: marshaling requirements, argument directionality, temporary buffer needs, and (for export) wrapper generation obligations.

For import, the foreign-call representation carries:

- Final C symbol name (resolved from HIR declaration)
- DPI ABI signature with per-argument classification (direct value, pointer-to-C-storage, etc.)
- Return classification (void, direct scalar, or indirect)

For export, MIR carries metadata on the exported function indicating that the LLVM layer must generate a C-ABI wrapper. This metadata includes the C symbol name and the DPI signature that the wrapper must present.

The key property: foreign-call intent is structurally distinct at MIR level, not inferred from flags during LLVM lowering. This means import marshaling, export wrapper generation, output/inout temporary allocation, and 4-state conversion all have a clean attachment point in MIR rather than being discovered ad-hoc during code generation.

## DPI ABI Layer

The DPI ABI layer maps SV semantic types to C ABI types per IEEE 1800-2023 Section 35.5.6. This is a dedicated module, separate from the internal-ABI type lowering used for user functions.

The layer's responsibilities:

- **Type classification**: Given an SV semantic type and argument direction, determine its **DPI ABI class** -- the canonical category that governs how the value crosses the foreign boundary (direct scalar, pointer to packed word array, pointer to logic vector, opaque handle, etc.). DPI ABI class is a named concept owned by this layer; all downstream decisions (marshaling strategy, temporary allocation, calling convention) are keyed on it, not on raw SV type properties.
- **Marshaling**: Generate conversion code at call boundaries between Lyra's internal representation and the DPI C representation. This includes 4-state layout conversion (Lyra's two-plane layout vs DPI's 32-bit-granular svLogicVecVal) and storage width normalization.
- **Temporary buffer management**: Allocate and manage boundary temporaries for output/inout arguments and for types that require layout conversion.

Detailed type mapping tables (exact C types, LLVM types, passing conventions per direction and width) will be specified in a separate ABI reference document as each phase solidifies.

## LLVM Layer

Two concerns, kept separate:

**DPI ABI lowering.** Uses the DPI ABI layer to map SV semantic types to C ABI LLVM types. Produces marshaling code at call boundaries: conversion from Lyra internal representation to C ABI representation (for import call arguments and export wrapper returns) and vice versa (for import return values and export wrapper arguments).

**Symbol materialization.**

- Import: creates an external-linkage LLVM function declaration with the C symbol name and DPI-typed signature. Call sites emit a standard LLVM call instruction with marshaled arguments.
- Export: creates an external-linkage LLVM wrapper function with the C symbol name and DPI-typed signature. The wrapper body marshals arguments from C types to Lyra internal types, obtains runtime context, calls the internal function using Lyra's internal ABI, and marshals the return value back.

## Driver and Link Model

Lyra orchestrates linkage but does not compile user code.

**AOT mode.** User-provided DPI link inputs (shared libraries or object files) are included as external link inputs in the link command via `LinkRequest`. Users compile their DPI C/C++ code separately and provide the resulting artifacts.

**JIT mode.** Each validated DPI link input is dynamically loaded into the ORC JIT symbol resolver. Unresolved DPI symbols are found in the loaded inputs.

**LLI mode.** Each validated DPI link input is passed to the `lli` interpreter via `--dlopen` so foreign symbols are available at execution time.

**Header generation.** Lyra emits a C header file containing prototypes for all DPI import and export functions, analogous to Verilator's `__Dpi.h`. This header uses standard DPI-C types and enables users to compile their DPI code against the correct signatures. Header generation is part of D4 scope.

**Link input specification.** Users specify DPI link inputs via the `--dpi-link` CLI flag (repeatable) or the `[dpi].link_inputs` array in `lyra.toml`. Config and CLI sources are merged additively. All inputs are validated (existence, regular file, absolute path) before reaching any backend.

## Export Runtime Contract

> **Open design question.** This section describes the problem and constraints. The specific mechanism is not yet decided and will be resolved during D4 investigation.

DPI export wrappers must obtain Lyra runtime context (DesignState, Engine, instance binding) without the external caller providing it. The mechanism must satisfy two constraints:

- **Re-entrancy**: export wrapper -> user code -> context import -> back into Lyra must work without corrupting process state.
- **No caller burden**: external C code calls the exported function with plain C arguments only. The simulator context is never part of the exported function signature.

Candidate approaches include thread-local simulation context, global context valid during simulation, and scope-based lookup via DPI scope handles. The choice depends on how Lyra's single-threaded execution model and process activation boundaries interact with export call sites. This will be investigated as part of D4.

## Testing Strategy

DPI changes the project's testing model by introducing dual-language tests. Three permanent testing layers:

- **Semantic validation.** Verify that the frontend correctly accepts valid DPI declarations and rejects invalid ones. No C library needed, no simulation.
- **ABI/codegen validation.** Verify that generated LLVM IR has correct DPI function signatures, linkage, marshaling sequences, and wrapper shapes. Can inspect IR without running a full simulation.
- **Mixed-language integration.** SV + C/C++ compiled, linked, and executed end-to-end. Verifies return values, argument passing, type conversions, and link-time symbol resolution across the full pipeline.

## Phasing

| Phase | Scope                                                     | Key constraint removed         |
| ----- | --------------------------------------------------------- | ------------------------------ |
| D1    | Pure import, 2-state scalars/real/string, input-only      | End-to-end DPI pipeline exists |
| D2    | General import, output/inout, chandle, packed             | Bidirectional argument passing |
| D3a   | 4-state scalar/vector marshaling                          | 4-state boundary conversion    |
| D3b   | Wide packed multiword transport (> 64-bit vectors)        | Full type coverage             |
| D4    | Export with C-ABI wrappers                                | Reverse control flow direction |
| D5a   | Context functions (svSetScope/svGetScope, scope plumbing) | Simulator context access       |
| D5b   | DPI tasks (time-consuming foreign calls, suspension)      | Scheduler integration          |

See [queues/dpi.md](queues/dpi.md) for operational tracking.
