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
- [ ] D6a -- svdpi.h runtime library and linker surface
- [ ] D6b -- DPI scope registry and instance-bound export context
- [ ] D6c -- Context import functions (svGetScope in import bodies)
- [ ] D4a -- Module-scoped DPI export (instance-bound wrapper dispatch)
- [ ] D4b -- 4-state scalar and packed by-pointer export wrapper marshaling
- [ ] D4c -- Packed-vector DPI return types (import and export, indirect return modeling)
- [ ] D4d -- AOT/LLI export end-to-end integration (currently JIT-only tested)
- [ ] D7a -- DPI import task suspension protocol
- [ ] D7b -- DPI import task scheduler integration
- [ ] D7c -- DPI export tasks (external C invocation of time-consuming SV tasks)

## D6a: svdpi.h runtime library and linker surface

The generated DPI header includes `<svdpi.h>`, but Lyra provides zero implementations of the standard functions declared in that header. Any user DPI code calling a svdpi.h function gets a linker error. This is a tool obligation, not optional convenience.

This item establishes a Lyra-owned svdpi.h runtime library as a build target and wires it into the JIT/AOT/LLI link paths so it is automatically available. Stateless utility functions (version query, packed-array bit/part-select helpers, canonical size helpers) get working implementations. All scope-dependent and time-dependent functions are present as hard-error traps with clear diagnostics. No scope registry, no instance binding, no context threading.

Prerequisite-free. Can land independently.

Where to look: svdpi.h (slang `external/ieee1800/`), JIT symbol resolution in execution.cpp, AOT link command in driver, LLI --dlopen path.

## D6b: DPI scope registry and instance-bound export context

The export call context carries only design_state and engine pointers. No runtime data structure maps scope handles to instances. Module-scoped exports and context imports both need scope-to-instance resolution that does not exist.

This item builds the scope registry (hierarchical path to RuntimeInstance mapping, constructed during instance realization), defines svScope as an opaque handle into the registry, and extends the export call context to carry active scope. Replaces the scope-dependent traps from D6a with working implementations: scope lookup/set/get and per-scope user data storage. Caller info and time queries remain traps (require additional metadata threading beyond scope).

Depends on D6a (runtime library exists to host scope implementations).

Where to look: RuntimeInstance (path_c_str, instance_id), constructor instance registration, DpiExportCallContext, dpi_export_context.hpp.

## D6c: Context import functions (svGetScope in import bodies)

Context imports are explicitly rejected at AST-to-HIR time. The DPIContext flag on imports is detected but gated. Inside a context import body, user C code should be able to call svGetScope() to retrieve the scope of the import call site.

This item removes the context import rejection, threads instance identity through the import call boundary (codegen passes scope handle as implicit state), and ensures svGetScope() returns the correct scope during context import execution.

Depends on D6b (scope registry and svGetScope implementation exist).

Where to look: routine.cpp context import rejection, scope threading through import calls, svGetScope implementation from D6b.

## D4a: Module-scoped DPI export

Module-scoped exports are explicitly rejected at AST-to-HIR time. The is_module_scoped flag exists on DpiExportDecl but is gated. Export wrappers currently call internal functions with only design and engine pointers. Module-scoped internal functions take three additional arguments: this_ptr, instance_ptr, and instance_id.

This item removes the module-scoped export rejection, emits module-scoped export wrappers that resolve active scope to a RuntimeInstance and extract the three instance-binding arguments, and calls the internal function with the full module-scoped ABI.

Depends on D6b (scope registry and instance-bound export context exist).

Ibex dependency: module-scoped exports in ibex_simple_system.sv are hidden behind `ifndef LYRA`. Removing the guard requires this item.

Where to look: design.cpp module-scoped export rejection, dpi_abi.cpp EmitDpiExportWrappers, process.cpp module-scoped function entry (this_ptr/instance_ptr/instance_id arguments).

## D4b: 4-state scalar and packed by-pointer export wrapper marshaling

The export wrapper model validator rejects 4-state scalar (svLogic) parameters and returns, and packed by-pointer parameters (narrow/wide logic vectors, wide bit vectors). The encode/decode helpers exist in the import path but are not composed in the export direction.

This item extends export wrappers to support the same type surface as import calls: 4-state scalar params use the existing svLogic encode/decode, packed by-pointer params use staged decode/writeback with the same temporary buffer strategy as imports reversed.

No infrastructure dependencies. Can land in parallel with the D6 series.

Where to look: dpi_abi.cpp export wrapper emission (type filter), svLogic encode/decode helpers, packed vector marshaling helpers.

## D4c: Packed-vector DPI return types (import and export, indirect return modeling)

Both import and export reject packed-vector return types. DpiReturnKind has only kVoid and kDirectValue. Packed vectors cannot be returned by value in C; the standard convention is an implicit output pointer parameter.

This item adds an indirect return variant to DpiReturnKind, modifies import call lowering to pass a caller-allocated return buffer as an implicit first argument, and modifies export wrappers to write the return value into the caller-provided buffer.

No infrastructure dependencies. Can land in parallel with the D6 series.

Where to look: DpiReturnKind in call.hpp, import return handling in LowerDpiImportCall, export return encoding in EmitDpiExportWrappers, return type rejections in routine.cpp and design.cpp.

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

Open arrays (svOpenArrayHandle query/access APIs) are deferred. They are a large self-contained LRM surface unrelated to the scope/export/context unblock path. They belong in a later DPI completeness pass, not the current queue.
