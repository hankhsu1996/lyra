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
- [x] D3a -- 4-state scalar/vector marshaling (narrow <=64-bit, input/output/inout, scalar return)
- [x] D3b -- Wide packed multiword transport (> 64-bit vectors, parameters only)
- [x] D3c -- `chandle` in internal callable ABI (user-function parameter/return, not DPI-specific)
- [x] D4 -- DPI export (package-scoped, scalar 2-state + real + string wrappers, header generation)
- [ ] D4a -- Module-scoped DPI export (instance binding for wrapper entry)
- [ ] D4b -- 4-state scalar and packed by-pointer export wrapper marshaling
- [ ] D4c -- Packed-vector DPI return types (import and export, indirect return modeling)
- [ ] D4d -- AOT/LLI export end-to-end integration (currently JIT-only tested)
- [ ] D5a -- Context functions (simulator scope access)
- [ ] D5b -- DPI tasks (time-consuming foreign calls)

## D3c: chandle in internal callable ABI

`chandle` as user-function parameter or return type is not supported in the internal callable ABI. Works through the DPI import path only (which has its own ABI mapper). Blocks export of functions that take or return `chandle`. Fix belongs in the internal callable ABI layer, not in the DPI layer.

## D4a: Module-scoped DPI export

Module-scoped exports are explicitly rejected with a diagnostic. The wrapper would need instance binding (this_ptr, signal-id offset, instance id) which the current export-call runtime context does not carry. The runtime context only has design_state and engine.

Ibex dependency: `mhpmcounter_num` and `mhpmcounter_get` in `ibex_simple_system.sv` are module-scoped exports hidden behind `ifndef LYRA`. Removing the guard requires D4a.

## D4b: 4-state scalar and packed by-pointer export wrapper marshaling

The current export wrapper model validator explicitly rejects: 4-state scalar (kLogicScalar) parameters and returns, and packed by-pointer parameters (kLogicVecNarrow, kLogicVecWide, kBitVecWide). These require the same encode/decode helpers used by the import path but composed in the reverse direction.

## D4c: Packed-vector DPI return types

Both import and export reject packed-vector return types (kLogicVecNarrow, kLogicVecWide, kBitVecWide). These require indirect return modeling (sret-style or output-param convention) which DpiReturnKind does not yet have a variant for.

## D4d: AOT/LLI export end-to-end integration

Export wrappers are emitted with ExternalLinkage for all backends, but only JIT has been tested end-to-end (via object-file loading into the JITDylib). AOT and LLI need verification that the wrapper symbols are linkable and callable from external C code.

## D5a: Context functions

Adds DPI context property support. Context functions may call back into the simulator to query or manipulate scope, requiring a scope/context handle infrastructure that does not exist today. This is effectively a minimal simulator-access API surface.

Shares runtime context infrastructure with D4 -- the dependency is on the infrastructure, not on D4 being fully complete.

## D5b: DPI tasks

Adds support for time-consuming DPI foreign calls. DPI tasks can consume simulation time, which requires integration with Lyra's cooperative process scheduling model. This is a separate problem from context scope access (D5a) and may require changes to the suspension protocol.
