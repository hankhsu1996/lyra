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
- [ ] D3b -- Wide packed multiword transport (> 64-bit vectors)
- [ ] D4 -- DPI export (C-callable wrappers, header generation)
- [ ] D5a -- Context functions (simulator scope access)
- [ ] D5b -- DPI tasks (time-consuming foreign calls)

## D3b: Wide packed multiword transport

Extends D3a to packed values wider than 64 bits. The D3a marshaling helpers (word extraction, svLogicVecVal encode/decode, semantic masking) are loop-based and accept arbitrary word counts, but are guarded to <=2 words at call sites. D3b removes those guards and adds:

- Semantic validation: accept 2-state and 4-state packed types > 64 bits
- Variable-size buffer allocation for svBitVecVal / svLogicVecVal arrays
- Input passing mode change: wide 2-state input also requires by-pointer (svBitVecVal\*)
- Indirect return modeling: DpiReturnKind::kIndirect, caller-allocated output pointer for vector returns
- LLVM function declaration changes for indirect return (sret-style or output-param convention)

This phase excludes open arrays, export, context, and tasks.

## D4: DPI export

Enables external C/C++ code to call SV functions during simulation via `export "DPI-C"`. Includes C header generation for exported function prototypes.

Structurally different from import: the generated code is the callee, not the caller. Lyra must produce a callable entry point with a stable C name that external code can link against, and that entry point must bridge into Lyra's internal execution model without the caller knowing about simulator internals.

This phase excludes context functions and DPI tasks.

Ibex dependency: the ibex simple_system uses `SYNTHESIS` define to suppress DPI export declarations in `prim_util_memload.svh` (`simutil_memload`, `simutil_set_mem`, `simutil_get_mem`) and in `ibex_simple_system.sv` (`mhpmcounter_num`, `mhpmcounter_get`). Memory loading works via pure SV `$readmemh` instead. Removing the `SYNTHESIS` workaround requires D4.

## D5a: Context functions

Adds DPI context property support. Context functions may call back into the simulator to query or manipulate scope, requiring a scope/context handle infrastructure that does not exist today. This is effectively a minimal simulator-access API surface.

Shares runtime context infrastructure with D4 -- the dependency is on the infrastructure, not on D4 being fully complete.

## D5b: DPI tasks

Adds support for time-consuming DPI foreign calls. DPI tasks can consume simulation time, which requires integration with Lyra's cooperative process scheduling model. This is a separate problem from context scope access (D5a) and may require changes to the suspension protocol.
