# LLVM Backend

Design decisions for MIR -> LLVM IR lowering.

## Pipeline Position

```
HIR -> MIR -> LLVM IR -> executable
              |
              +-> Runtime library (Lyra*)
```

MIR fixes all execution semantics. The LLVM backend only translates--it does not interpret SystemVerilog rules.

## Scope

LLVM compilation is **per-specialization**. Each module specialization produces its own LLVM module. There is no monolithic design-level LLVM module. This enables parallel compilation and incremental rebuilds. See [compilation-model.md](compilation-model.md).

Specialization-local optimizations (kernelization, connection batching, topo sorting) are performed within the specialization boundary. Cross-module optimization is forbidden.

## Core Philosophy

Three principles govern all lowering decisions:

| Principle                              | Implication                                                  |
| -------------------------------------- | ------------------------------------------------------------ |
| LLVM does not understand SystemVerilog | MIR must resolve all SV semantics before lowering            |
| Lowering is mechanical, not semantic   | No interpretation, no decision-making in LLVM layer          |
| One MIR construct -> one LLVM shape    | No conditionals that produce different IR for same construct |

If LLVM needs to "understand" SV rules to run correctly, the design has failed.

## Responsibility Split

### MIR Guarantees

Before lowering, MIR has already fixed:

- Rvalue vs Place is explicit
- Bit-width, signedness, and 2-state vs 4-state
- Evaluation order
- Side effects (calls, stores, terminators)

### LLVM Lowering Rules

| Must Do                                | Must NOT Do                      |
| -------------------------------------- | -------------------------------- |
| Translate MIR faithfully               | Infer SV semantics               |
| Emit runtime calls for SV-specific ops | Re-decide evaluation order       |
| Derive layout from TypeId              | Encode language rules implicitly |
| Produce deterministic IR               | Create new types                 |

LLVM is a backend, not a language layer. See [pipeline-contract.md](pipeline-contract.md) for layer boundaries.

## State Modes

The LLVM backend supports both 2-state and 4-state execution, controlled by the `--two-state` CLI flag.

**4-state mode** (default): Packed 4-state types use `{iN, iN}` SSA structs (value lane + unknown lane). Storage uses dense canonical layout (see [state-layout.md](state-layout.md)). Design state is X-initialized via the patch table.

**2-state mode**: All packed types forced to 2-state representation. Storage uses single integer lanes. Design state is zero-initialized. Controlled by `StorageMode::kTwoState` at layout resolution time.

## Lowering Decision Framework

For every MIR construct, answer three questions:

| Question                 | If Yes                | If No                 |
| ------------------------ | --------------------- | --------------------- |
| Does it produce a value? | LLVM SSA value        | Call / store / branch |
| Is it pure?              | LLVM instruction      | Runtime call          |
| Is it SV-specific?       | Runtime API (`Lyra*`) | Native LLVM op        |

This framework determines the LLVM shape mechanically.

## Lowering Categories

### Pure Arithmetic and Bitwise

Examples: `add`, `sub`, `and`, `xor`, shifts

- MIR ensures operands are normalized
- Use native LLVM instructions: `add`, `and`, `xor`, `shl`, `lshr`, `ashr`
- Signedness matters for shifts and comparisons
- Do not rely on C/C++ overflow semantics

### Comparisons and Conditions

- LLVM branches require 2-state 1-bit booleans
- 2-state comparisons -> `icmp`
- 4-state comparisons -> runtime call (never branch on 4-state directly)

### Constants

- Small scalars -> `ConstantInt`
- Wide 2-state -> global constant arrays + `LyraAssignBits2`
- Wide 4-state -> three globals (value, x_mask, z_mask)
- Layout fully decided by MIR; LLVM does not compute SV layout

### SV-Specific Operations

Always lower to runtime calls:

- `$display`, `$write` (I/O)
- 4-state arithmetic and propagation
- Dynamic arrays, queues
- Timing constructs

## Width Threshold

| Width       | Strategy                                  |
| ----------- | ----------------------------------------- |
| <= 128 bits | Native LLVM integers and operations       |
| > 128 bits  | Global constant blobs + runtime API calls |

The threshold may be tuned, but the principle is fixed: LLVM handles what it's good at; runtime handles the rest.

## Layout and Storage Boundary

Layout is derived from `SlotStorageSpec`, not from LLVM type introspection. The storage spec is resolved once per slot at layout time and drives all downstream decisions (commit, metadata, initialization). See [state-layout.md](state-layout.md) for the canonical storage contract.

### SSA Compute Form vs Storage Form

LLVM SSA types are transient compute form. Canonical arena storage may differ in layout. Crossing between them happens only at explicit storage-boundary helpers.

| Type           | SSA compute form      | Storage form                            |
| -------------- | --------------------- | --------------------------------------- |
| Packed 2-state | `iN` (semantic width) | `iM` (storage lane width, M >= N)       |
| Packed 4-state | `{iN, iN}` struct     | Two dense integer lanes at byte offsets |
| Float          | `float` / `double`    | IEEE 754 bytes                          |
| Aggregate      | LLVM struct/array     | Recursive canonical bytes per spec      |

For widths <= 64, semantic width equals storage width (both power-of-2 rounded). For widths > 64, storage rounds up to byte alignment (e.g., 65-bit -> 9 bytes -> i72).

### Storage Boundary Helpers

| Helper                         | Purpose                                                                                     |
| ------------------------------ | ------------------------------------------------------------------------------------------- |
| `LowerToStorageLaneWidth`      | Explicit lowering from semantic to storage width. Callers must invoke before store/flatten. |
| `EmitStoreToCanonicalStorage`  | Store SSA value to canonical bytes. Asserts storage-width input. Recurses for aggregates.   |
| `EmitLoadFromCanonicalStorage` | Load canonical bytes into SSA form. Scalar only (aggregates use projections).               |
| `EmitPackedToCanonicalBits`    | Flatten to canonical integer for inline compare/store. Asserts storage-width input.         |

### Anti-Patterns

| Violation                                               | Why it breaks                                         |
| ------------------------------------------------------- | ----------------------------------------------------- |
| Store typed LLVM object directly to arena               | LLVM padding may differ from canonical layout         |
| Derive storage layout from LLVM `StructLayout`          | Couples to LLVM internals, wrong for wide 4-state     |
| Local "try/guess" from LLVM type shape                  | Fragile, misses edge cases, spreads storage knowledge |
| `alloca + store + reload as another type`               | Type-punning hides layout mismatch                    |
| Re-derive storage facts from `TypeId + force_two_state` | Duplicates resolved spec, can drift                   |

## Four-State Representation

Four-state packed values use a two-lane model:

| Lane          | Purpose                |
| ------------- | ---------------------- |
| value (known) | The 0/1 bit pattern    |
| unknown       | X/Z bits (1 = unknown) |

SSA compute form is `{iN, iN}` (value, unknown). Storage form is two dense integer lanes at canonical byte offsets (see [state-layout.md](state-layout.md)).

Branchless propagation for hot paths -- masks flow through bitwise operations so unknown bits contaminate results. `case`/`casez`/`casex` inspect masks directly.

## Runtime APIs vs LLVM Intrinsics

**LLVM intrinsics** (`llvm.*`): Only for operations LLVM understands natively--`llvm.memcpy`, `llvm.ctpop`, etc.

**Runtime APIs** (`Lyra*`): For SystemVerilog semantics LLVM cannot express--wide arithmetic, 4-state propagation, SV-specific operations.

## What LLVM Backend Must NOT Do

| Violation                         | Why It's Wrong                 |
| --------------------------------- | ------------------------------ |
| Handle 4-state logic directly     | Runtime responsibility         |
| Decide evaluation order           | MIR already fixed it           |
| Produce different IR for same MIR | Leads to non-determinism       |
| Recompute bit offsets or slices   | MIR owns all address decisions |
| Interpret SV semantics            | MIR is the semantic endpoint   |

## Instance-Independence

### Target shape

Per-instance binding must not appear in LLVM function or global identity. Heavy LLVM codegen shape -- function count, global count, and optimization work -- must be determined by the number of unique specializations, not the number of instances. Changing instance counts or generate expansion must not increase heavy LLVM codegen work (IR construction, LLVM optimization, code emission).

Shared body functions are the correct long-term shape: one function per (body, process) pair, parameterized by instance-specific constants passed as arguments. Process and comb dispatch must carry instance-specific binding via runtime-owned realization data, not per-instance LLVM wrapper functions.

### Current state

Module-process dispatch is descriptor-driven (G1 complete). Codegen emits a constant descriptor table (`__lyra_process_descriptors`) with per-module-process binding data. The runtime reads descriptors and calls shared body functions directly with the 7-arg ABI. No per-instance process wrapper functions exist. Standalone (connection) processes keep the existing 3-arg direct dispatch path. A single shared trampoline (`__lyra_descriptor_dispatch`) bridges comb wrappers to the descriptor table; comb dispatch itself is not yet migrated to the runtime (G2).

**Remaining per-instance LLVM artifacts** (tracked as G2-G4):

- Per-instance comb wrapper functions (`__lyra_comb_wrapper_N`) -- adapted to call the shared trampoline instead of deleted process wrappers; still per-instance LLVM functions (G2)
- Per-instance unstable-offset globals (`inst_N_unstable_offsets`) -- constant data, not executable artifacts (G3)
- Per-scheduled-process named LLVM struct types (`ProcessStateN`, `ProcessFrameN`) -- cosmetic, should be per-body (G4)
- `__lyra_module_funcs` function pointer array with null module entries -- temporary compatibility residue (G4)
- `__lyra_proc_offsets` state offset array -- instance-count-shaped data (G4)

## Engineering Guidelines

- One lowering function per MIR construct
- Deterministic IR output (byte-for-byte reproducible)
