# DPI-C

> **Queue rules.** Checkboxes at top for progress. Unchecked = gap. Finished items: remove the section, keep one checked line. Every unchecked item must have a short tag/ID for tracking (e.g. D1). Each item should be independently addressable -- it can be investigated and landed as a standalone change. No code in items (no function names, variable names, snippets). Items describe what the gap is, where to look, and why it matters -- enough to re-investigate from scratch. Investigation logs and design details belong in conversation history, not here.

Working queue for DPI-C foreign function interface support.

For the stable architecture: see [dpi-design.md](../dpi-design.md).

## Progress

- [ ] D1 -- Pure import functions (2-state scalars, real, string)
- [ ] D2 -- General import functions (output/inout args, chandle, packed types)
- [ ] D3a -- 4-state scalar/vector marshaling
- [ ] D3b -- Wide packed multiword transport (> 64-bit vectors)
- [ ] D4 -- DPI export (C-callable wrappers, header generation)
- [ ] D5a -- Context functions (simulator scope access)
- [ ] D5b -- DPI tasks (time-consuming foreign calls)

## D1: Pure import functions

First end-to-end vertical slice. Covers `import "DPI-C" pure function` with input-only arguments of simple 2-state types (int, shortint, longint, byte, bit, real, shortreal, string) and void or scalar return.

Requires work at every layer: HIR declaration, MIR foreign-call representation, LLVM ABI lowering and symbol materialization, driver link/load extension, and dual-language testing infrastructure. See [dpi-design.md](../dpi-design.md) for the architectural contracts each layer must satisfy.

This phase excludes output/inout arguments, 4-state types, chandle, open arrays, export, context, and tasks.

## D2: General import functions

Extends D1 to non-pure import functions with output and inout argument directions, chandle type support, and packed types whose DPI ABI class is direct scalar (no multiword or 4-state marshaling required).

The core new capabilities: bidirectional argument passing across the foreign boundary where the pointee storage uses C ABI layout, and chandle as a new type kind representing an opaque foreign pointer.

This phase excludes 4-state types, wide packed vectors, open arrays, export, context, and tasks.

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

## D5a: Context functions

Adds DPI context property support. Context functions may call back into the simulator to query or manipulate scope, requiring a scope/context handle infrastructure that does not exist today. This is effectively a minimal simulator-access API surface.

Shares runtime context infrastructure with D4 -- the dependency is on the infrastructure, not on D4 being fully complete.

## D5b: DPI tasks

Adds support for time-consuming DPI foreign calls. DPI tasks can consume simulation time, which requires integration with Lyra's cooperative process scheduling model. This is a separate problem from context scope access (D5a) and may require changes to the suspension protocol.
