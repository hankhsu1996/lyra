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
  - [ ] D4d -- AOT/LLI export end-to-end integration (currently JIT-only tested)
- [x] D6a -- svdpi.h runtime library and linker surface
  - [x] D6b -- DPI scope registry and instance-bound export context
  - [x] D6c -- Context import functions (svGetScope in import bodies)
  - [ ] D6d -- svdpi time query functions
  - [ ] D6e -- svGetCallerInfo import call-site metadata
- [ ] D7 -- DPI tasks
  - [ ] D7a -- DPI import task suspension protocol
  - [ ] D7b -- DPI import task scheduler integration
  - [ ] D7c -- DPI export tasks (external C invocation of time-consuming SV tasks)
- [ ] D8 -- Open arrays
  - [ ] D8a -- Open-array query and pointer surface
  - [ ] D8b -- Open-array packed/scalar element access

## D4d: AOT/LLI export end-to-end integration

Export wrappers are emitted with ExternalLinkage for all backends, but only JIT has been tested end-to-end. AOT and LLI need verification that export symbols are linkable and callable from external C code.

No infrastructure dependencies.

Where to look: AOT LinkRequest in driver, LLI --dlopen path, test infrastructure for backend-specific DPI tests.

## D7a: DPI import task suspension protocol

DPI import tasks are rejected at AST-to-HIR time. DPI tasks can consume simulation time, requiring the calling process to suspend and resume. This is fundamentally different from DPI import functions, which are instantaneous.

This item defines the suspension/resumption contract for foreign task calls: how a DPI task signals that it needs to consume time, how the calling process yields to the scheduler, and how it resumes when the time advance completes. Includes the disable protocol (svIsDisabledState, svAckDisabledState) since it is part of the same suspension contract.

Does not remove the import task rejection or integrate with the scheduler -- those are D7b.

Where to look: process scheduling/suspension in engine, svIsDisabledState/svAckDisabledState in svdpi.h, cooperative scheduling model.

## D7b: DPI import task scheduler integration

Wires the suspension protocol from D7a into the live scheduler. Removes the import task rejection at AST-to-HIR time. Adds MIR/codegen support for DPI task calls that can suspend. End-to-end: a DPI import task that calls a delay advances simulation time and resumes correctly.

Depends on D7a (suspension protocol defined).

Where to look: routine.cpp task rejection, MIR DpiCall (task vs function distinction), process scheduler yield/resume path.

## D7c: DPI export tasks (external C invocation of time-consuming SV tasks)

DPI export tasks allow external C code to invoke SV tasks that consume simulation time. This requires both the suspension protocol from D7a/D7b and the scope/instance binding from D6b (export tasks are typically module-scoped).

Depends on D7b (import task suspension works) and D6b (scope registry for instance binding).

Where to look: design.cpp task rejection, export wrapper emission, scheduler integration from D7b.

## D6d: svdpi time query functions

Three svdpi.h time query functions are trapped from D6a: `svGetTime`, `svGetTimeUnit`, `svGetTimePrecision`. These require a scope handle to determine the time unit/precision context, plus access to the current simulation time.

This item replaces the time traps with working implementations. Defines what `scope == NULL` means in Lyra (simulation-level time unit), maps non-null scope to the correct time unit/precision context via the scope registry, and exposes current simulation time through the svdpi surface.

Depends on D6b (scope registry must exist for scope-to-time-unit resolution).

Where to look: runtime time model, scope registry from D6b, `svdpi_runtime.cpp` time trap section.

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

D6a added all 99 svdpi.h symbols to the runtime. 26 have working implementations. The remaining 73 are hard-error traps that print a diagnostic and abort. Each trap names its owning queue item.

- Scope (6, unblocked by D6b)
- Time (3, unblocked by D6d)
- Caller-info (1, unblocked by D6e)
- Disable-state (2, unblocked by D7a)
- Open-array query/pointer (13, unblocked by D8a)
- Open-array data access/copy (48, unblocked by D8b)
