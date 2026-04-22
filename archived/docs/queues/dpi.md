# DPI-C

> **Queue rules.** Checkboxes at top for progress. Unchecked = gap. Finished items: remove the section, keep one checked line. Every unchecked item must have a short tag/ID for tracking (e.g. D1). Each item should be independently addressable -- it can be investigated and landed as a standalone change. No code in items (no function names, variable names, snippets). Items describe what the gap is, where to look, and why it matters -- enough to re-investigate from scratch. Investigation logs and design details belong in conversation history, not here.

Working queue for DPI-C foreign function interface support.

For the stable architecture: see [dpi-design.md](../dpi-design.md).

## Progress

- [x] D1 -- Pure import functions (2-state scalars, real, string)
  - [x] D1a -- Scalar-only pure import, JIT, `--dpi-link` CLI, semantic + integration tests
  - [x] D1b -- Add `string`
  - [x] D1c -- AOT/LLI link plumbing, `LinkRequest`, unified test linking
  - [x] D1d -- `lyra.toml` `[dpi].link_inputs`, validated input phase
- [x] D2 -- General import functions (output/inout args, chandle, packed types)
  - [x] D2a -- Output/inout args, non-pure imports, packed 2-state direct-scalar <=64
  - [x] D2b -- DPI-own MIR call family (`DpiCall`), per-param descriptors, 3-phase LLVM lowering
  - [x] D2c -- `chandle` variable LLVM support (load/store/null literal)
- [x] D3 -- 4-state and wide packed marshaling
  - [x] D3a -- 4-state scalar/vector marshaling (narrow <=64-bit, input/output/inout, scalar return)
  - [x] D3b -- Wide packed multiword transport (> 64-bit vectors, parameters only)
  - [x] D3c -- `chandle` in internal callable ABI (user-function parameter/return, not DPI-specific)
- [x] D4 -- DPI export (package-scoped, scalar 2-state + real + string wrappers, header generation)
  - [x] D4a -- Module-scoped DPI export (instance-bound wrapper dispatch)
  - [x] D4b -- 4-state scalar and packed by-pointer export wrapper marshaling
  - [x] D4c -- Indirect-return modeling for DPI return types requiring hidden result storage (integer); wide packed lowering (kLogicVecWide, kBitVecWide) exists in backend infrastructure but is not end-to-end reachable because slang rejects explicit packed-array DPI return types per IEEE 1800
  - [x] D4d -- AOT/LLI export end-to-end integration
- [x] D6a -- svdpi.h runtime library and linker surface
  - [x] D6b -- DPI scope registry and instance-bound export context
  - [x] D6c -- Context import functions (svGetScope in import bodies)
  - [x] D6d -- svdpi time query functions
  - [ ] D6e -- svGetCallerInfo import call-site metadata
- [ ] D7 -- DPI tasks
  - [x] D7a -- Non-suspending DPI export tasks (module-scoped only)
  - [ ] D7b -- Non-suspending DPI import tasks
  - [ ] D7c -- Suspending DPI import tasks (suspension protocol + scheduler integration)
  - [ ] D7d -- Suspending DPI export tasks (suspend-aware export wrapper reentry)
- [ ] D8 -- Open arrays
  - [ ] D8a -- Open-array query and pointer surface
  - [ ] D8b -- Open-array packed/scalar element access

## D7b: Non-suspending DPI import tasks

DPI import tasks are rejected at AST-to-HIR time (routine.cpp TryLowerDpiImport). Same capability-based first stage as D7a but on the import side: remove the import task rejection and route non-suspending import tasks through the existing immediate-call machinery (shared call ABI, marshaling).

Architectural rule: DPI task semantic identity stays task. We reuse the immediate-call machinery where valid, but we do not reinterpret or rewrite a task as a function.

D7a landed the shared non-suspending task validator (CheckNonSuspendingTask), DpiRoutineKind enum, and nested export-call context with suspension guard. D7b reuses the validator for import admission and extends DpiImportRef / DpiCall with routine kind. The MIR DPI import call path currently carries no task/function distinction; this item adds it.

Where to look: routine.cpp import task rejection, DpiImportRef/DpiCall in call.hpp (no routine_kind yet), import marshaling path, CheckNonSuspendingTask validator.

## D7c: Suspending DPI import tasks

Full suspension support for DPI import tasks that consume simulation time. Replaces the D7b runtime error with real suspension. Defines the suspension/resumption contract: how a DPI task signals time consumption, how the calling process yields to the scheduler, how it resumes. Includes the disable protocol (svIsDisabledState, svAckDisabledState). Wires into the live scheduler with MIR/codegen support for suspending calls.

Architectural rule: DPI task semantic identity stays task. Suspension extends the capability model from D7b, not a separate parallel path.

Depends on D7b (non-suspending import path exists).

Where to look: process scheduling/suspension in engine, svdpi.h disable-state functions, MIR DpiCall suspend capability, process scheduler yield/resume path.

## D7d: Suspending DPI export tasks

Adds suspend-aware reentry contract to export wrappers. External C code can invoke SV export tasks that consume simulation time, with the export wrapper managing suspension/resumption across the foreign boundary.

Architectural rule: DPI task semantic identity stays task. Suspension extends the capability model from D7a, reusing the suspension protocol defined in D7c.

Depends on D7a (non-suspending export path exists), D7c (suspension protocol defined), and D6b (scope registry for instance binding).

Where to look: export wrapper emission, scheduler integration from D7c.

## D6e: svGetCallerInfo import call-site metadata

`svGetCallerInfo` is trapped from D6a. It returns the SV source file and line number of the import call site. This requires threading source-location metadata through DPI import call boundaries so imported C code can query it at runtime.

This item threads call-site source location into the DPI import context and replaces the trap with a working implementation.

Depends on D6b (import context infrastructure).

Where to look: AST/HIR/MIR call-site metadata, DPI import lowering, `svdpi_runtime.cpp` caller-info trap.

## D8a: Open-array query and pointer surface

First open-array cut. Replaces traps for the introspection and pointer-access families (13 functions):

- Query (7): `svLeft`, `svRight`, `svLow`, `svHigh`, `svIncrement`, `svSize`, `svDimensions`
- Pointer access (6): `svGetArrayPtr`, `svSizeOfArray`, `svGetArrElemPtr`, `svGetArrElemPtr1`, `svGetArrElemPtr2`, `svGetArrElemPtr3`

This is the foundational open-array step -- all data access helpers depend on the query/pointer infrastructure.

Where to look: `svdpi_runtime.cpp` open-array trap section, open-array parameter passing in DPI ABI layer.

## D8b: Open-array packed/scalar element access

Replaces traps for the remaining 48 open-array data access functions:

- Packed VecVal copy (16): `svPut{Bit,Logic}ArrElem{,1,2,3}VecVal`, `svGet{Bit,Logic}ArrElem{,1,2,3}VecVal`
- Scalar element access (16): `svGet{Bit,Logic}ArrElem{,1,2,3}`, `svPut{Bit,Logic}ArrElem{,1,2,3}`
- Legacy-named Vec32 entrypoints (16): `svPut{Bit,Logic}ArrElem{,1,2,3}Vec32`, `svGet{Bit,Logic}ArrElem{,1,2,3}Vec32`

Depends on D8a (query/pointer infrastructure exists).

Where to look: `svdpi_runtime.cpp` open-array trap sections, packed-array marshaling helpers from D6a.

## Trapped svdpi.h functions (from D6a)

D6a added all 99 svdpi.h symbols to the runtime. 29 have working implementations. The remaining 70 are hard-error traps that print a diagnostic and abort. Each trap names its owning queue item.

- Time (3, done in D6d)
- Caller-info (1, unblocked by D6e)
- Disable-state (2, unblocked by D7c)
- Open-array query/pointer (13, unblocked by D8a)
- Open-array data access/copy (48, unblocked by D8b)
