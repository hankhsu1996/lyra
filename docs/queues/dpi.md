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
- [ ] D2 -- General import functions (output/inout args, chandle, packed types)
  - [x] D2a -- Output/inout args, non-pure imports, packed 2-state direct-scalar <=64
  - [x] D2b -- DPI-own MIR call family (`DpiCall`), per-param descriptors, 3-phase LLVM lowering
  - [ ] D2c -- `chandle` variable LLVM support (load/store/null literal)
- [ ] D3a -- 4-state scalar/vector marshaling
- [ ] D3b -- Wide packed multiword transport (> 64-bit vectors)
- [ ] D4 -- DPI export (C-callable wrappers, header generation)
- [ ] D5a -- Context functions (simulator scope access)
- [ ] D5b -- DPI tasks (time-consuming foreign calls)

## D2c: chandle variable support

Cross-layer feature: make chandle work as a plain variable, not just a DPI declaration type. The type infrastructure (`TypeKind::kChandle`, storage spec, default init, `TypeDescKind::kChandle`, DPI ABI classification) is in place from D2a/D2b. The gap is expression-level operations and LLVM write/rvalue dispatch.

Concrete gaps (verified from code, not assumption):

1. No `NullConstant` in `ConstantValue` variant (`constant.hpp`). Null literals have no compile-time representation.
2. `NullLiteral` expression kind unhandled in AST-to-HIR dispatch (`expression.cpp`).
3. Null-to-chandle and chandle identity conversions rejected by `LowerConversionExpression` (`expression_access.cpp:116`).
4. `GetTypeInfoFromType` (`rvalue.cpp:19-48`) rejects kChandle: not managed, not packed. Blocks all rvalue computation including equality/inequality.
5. `ClassifyWriteShape` (`write_plan.cpp:35`) falls through to packed-scalar, then `PackedBitWidth` throws on kChandle. Blocks assignment and CommitValue.

Do not route chandle through packed-scalar fallback. Give it an explicit pointer-scalar path in rvalue classification, write-shape dispatch, and commit.

## D3a: 4-state scalar/vector marshaling

Adds 4-state value passing across the DPI boundary. Lyra's internal 4-state representation differs from the DPI-C standard representation, so a dedicated conversion layer is needed at call boundaries.

Covers logic, integer (4-state), and 4-state packed types that do not require multiword transport.

This phase excludes wide packed vectors, open arrays, export, context, and tasks.

## D3b: Wide packed multiword transport

Extends D3a to packed values wider than 64 bits. The marshaling primitives from D3a are reused, but the transport changes to indirect passing with caller-managed storage.

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
