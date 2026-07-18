# DPI-C (import and export)

Tracks the SystemVerilog Direct Programming Interface (LRM 35): `import "DPI-C"`, where SV calls a
foreign C function, and `export "DPI-C"`, where foreign C calls an SV subroutine. This is the
foreign-language-boundary workstream that `functions.md` lists as out of scope for user subroutines.

Under the post-reset architecture a DPI import is not a separate call subsystem: it is the
**external** implementation form of the one callable concept (`callable.md`) -- a signature plus a
foreign linkage name and calling convention, with no body -- and an import call is an ordinary call.
A DPI export is an ordinary internal SV subroutine for which a backend additionally materializes a
foreign-linkage wrapper; the wrapper obtains its runtime context (design object, engine, and, for a
module-scoped export, the calling instance) from a runtime-installed context, never from the foreign
caller. Export is supported within the LRM import -> export call chain, under Lyra as the driver;
the distinct execution model where an external C program drives a Lyra design as a linked library is
a separate roadmap capability, out of scope here. The DPI type mapping between an SV type and its C
ABI type (LRM 35.5.6) is a backend type-mapping concern, so the MIR representation is
backend-agnostic: the same MIR is materialized by the C++ backend as an `extern "C"` entry linked by
the emitted build recipe, and by the LLVM / JIT backend as an external-linkage symbol resolved by
its execution session. Lyra never compiles the user's C; it provides the ABI surface (a generated
header, resolved symbol names) and orchestrates linkage.

The settled IR, value, and boundary model -- import as the external arm of the one callable,
marshaling as a cross-ABI carrier conversion through runtime primitives, the export context, and the
out-of-scope external-driver boundary -- is recorded in `../decisions/dpi-foreign-boundary.md`.

Done when the LRM 35 surface reproduces on both backends: import (pure and context, every argument
direction, the full DPI type surface including 4-state, wide packed, `chandle`, and open arrays),
export (package / `$unit`-scoped and module-scoped instance-bound), DPI tasks (both directions,
including a foreign task that consumes simulation time and the disable protocol), the `svdpi`
context surface, a generated ABI header, and link-input orchestration.

## Actionable

Two frontiers are open. On the C++ backend the function import surface (D1-D3) is in, the export
marshaling surface (D4, D4b) runs -- an export executes end to end through the import -> export
chain across every argument direction and both value families (by-value scalars, by-pointer scalar
`output` / `inout`, and canonical 2-state / 4-state packed vectors), its wrapper recovering the
exported subroutine's single top-level instance from the running design -- and the import task (D5)
runs: SV calls a foreign C task under the uniform task protocol. What remains is instance-bound
export dispatch beyond that single instance via `svSetScope` and receiver-less package / `$unit`
scope (D4a), the generated ABI header and export linkage (D9), the export task (D6), and
foreign-boundary suspension (D6b). On the execution backend scalar import (D10) is in: a foreign
call lowers to an external-linkage symbol and the by-value carriers marshal. The rest of the import
surface (D11) is blocked, and not by anything DPI owns: by-pointer marshaling is expressed as a
closure, which that backend does not yet lower at all (`architecture-reset.md`).

## Sub-Steps

The `D*` IDs are stable references. They do not impose a total order; real dependencies are stated
inline. D1-D9 are deliverables on the C++ backend; D10 onward bring the same backend-agnostic MIR up
on the execution backend, mirroring the C++-backend items one surface at a time.

### Import: SV calls foreign C

- [x] D1 -- Pure scalar import: 2-state integral scalars, `real`, and `string`, `input`-only,
      function kind (LRM 35.4, 35.5.1). An imported subroutine lowers to a callable whose
      implementation form is an external symbol, and an import call is an ordinary call to it; a
      user-provided C object links in through the emitted build recipe, asserted by a mixed-language
      test.
- [x] D2 -- General scalar import (LRM 35.5.5, 35.5.6): `output` and `inout` arguments cross by
      pointer to a boundary temporary and copy back into the actual, a value result works in
      expression position alongside `output` / `inout` arguments, non-pure imports are accepted, and
      `chandle` crosses as an opaque pointer in either direction and round-trips its identity.
- [x] D3 -- 4-state and wide marshaling (LRM 35.5.6, Annex H.10): every packed vector --
      `bit [N:0]`, `logic [N:0]`, the 4-state `integer` / `time`, and values wider than one machine
      word -- crosses by pointer as its canonical `svBitVecVal*` / `svLogicVecVal*` buffer,
      classified by declared type shape (WYSIWYG, LRM 35.6.1.1) not width, so `int` (by value) and
      `bit [31:0]` (canonical vector) get distinct C ABIs; the canonical layout matches Lyra's
      two-plane representation, so marshaling is a plane reshape. A 4-state scalar `logic` result
      crosses by value; a wider result stays restricted to a small value (LRM 35.5.5).

### Export: foreign C calls SV

Export is scoped to the LRM import -> export call chain under Lyra as the driver (an imported C
function, called from a running simulation, calls back an exported SV function). The distinct model
where an external C program is the `main` driving a Lyra design as a linked library is out of scope
here (see the design record); a design that declares an export only for such an external driver is
accepted, records its metadata, and can have its ABI header generated, but is not claimed callable
from an external main.

- [x] D4 -- The scalar export foreign-linkage wrapper and the import -> export call chain under Lyra
      as the driver (LRM 35.5): a scalar, input-only export executes end to end. The wrapper
      marshals the C arguments, recovers the exported subroutine's instance from the running design
      (the single top-level instance), calls the method, and marshals the result back. The
      subroutine keeps its ordinary body; the wrapper's marshaling is stated in MIR so a backend
      renders it mechanically, and only the receiver recovery and the external linkage are the
      backend's shell.
- [ ] D4a -- Instance-bound dispatch beyond the single top-level instance (LRM 35.5.3): the wrapper
      resolves the specific instance the foreign call targets from the scope the foreign side set
      (`svSetScope`), rather than the single-instance recovery above, and a receiver-less package /
      `$unit`-scoped export needs no instance at all (riding on the package unit and package
      callable in `packages.md`, PK1-PK2). This is the `mhpmcounter_num` / `mhpmcounter_get` shape
      in the Ibex bring-up; the pure-SV Ibex run never calls them, so this closes the construct, not
      the Ibex external-driver usage.
- [x] D4b -- 4-state and by-pointer packed export marshaling. An `output` / `inout` scalar crosses
      by pointer to its by-value carrier; a packed vector of any direction crosses by pointer as its
      canonical `svBitVecVal*` / `svLogicVecVal*` buffer, its planes reshaped without a per-bit
      transcode. The `output` / `inout` values ride the completion payload the exported subroutine
      already returns (LRM 13.5), so the wrapper destructures the payload and marshals each
      component out through its foreign pointer. A function result stays a by-value return, limited
      to a small value (LRM 35.5.5): an atom such as `int` / `longint` / `real` / `string` /
      `chandle`, or a scalar `bit` / `logic`; a packed-vector, `integer`, or `time` result is not a
      small value and cannot be returned.

### DPI tasks

A DPI task rides the same task call protocol as any SV task (LRM 13.4.4): the call is a suspension
point, lowered to a coroutine the caller awaits. A task is classified by kind, never by whether it
consumes time -- a task that consumes no simulation time is the trivial case of one that does, on
the same protocol, not a separate path. The two items below are the boundary directions; the runtime
machinery that lets a foreign task actually suspend across the C boundary, with the disable
protocol, extends the same awaitable and is tracked as D6b.

- [x] D5 -- DPI import task (LRM 35.5.2): SV calls a foreign C task. The call rides the uniform task
      protocol -- a coroutine the caller awaits -- with the actuals marshaled in and the writeback
      arguments marshaled back; a task that consumes no time completes within the await.
- [ ] D6 -- DPI export task: foreign C calls an SV task through its foreign-linkage wrapper, driving
      the exported task's coroutine body to completion and marshaling its writebacks back across the
      boundary.
- [ ] D6b -- Foreign-boundary suspension: a foreign task that actually consumes simulation time,
      yielded across the C boundary, with the disable protocol (LRM 35.9). This extends the task
      awaitable's runtime; the call protocol and lowering are unchanged from D5 / D6. Rides on the
      timing and suspension machinery (`scheduling.md`, `processes.md`).

### Context and the svdpi surface

- [ ] D7 -- `context` imports and exports and the `svdpi` runtime surface: DPI scope handles, set /
      get scope, the scope registry, and time queries. A context import observes the scope of its
      call site.

### Open arrays

- [ ] D8 -- Open-array arguments (LRM 35.5.6.1): the introspection and pointer surface (array
      bounds, size, dimension queries, element pointers) and packed / scalar element access.

### Driver and link

- [ ] D9 -- Link-input orchestration: a repeatable `--dpi-link` CLI option and a `lyra.toml` DPI
      link-inputs array, validated before any backend runs, added to the emitted build recipe's link
      line. A generated ABI header lets the user compile their C against the correct signatures.

### Execution backend

The execution backend (MIR lowered to LIR to LLVM, run as JIT or AOT) elaborates module hierarchies
and runs procedural code, so a foreign call has a body to sit inside. The DPI-specific gap is the
two points the MIR-to-LIR lowering names: the import-call target and the ABI carrier type. These
items bring the same backend-agnostic MIR the C++ backend consumes up on the execution backend, one
surface at a time; export and tasks follow once the C++-backend items fix their shape.

- [x] D10 -- Scalar import on the execution backend: 2-state integral and `string`, `input`-only
      functions. The import-call target lowers to an external-linkage symbol the execution session
      resolves, the by-value carriers marshal, and a JIT run cross-checks the result against the C++
      backend. A `real` import is excluded, but not by anything DPI owns: the execution backend has
      no real value domain at all, so it cannot read a real out of an SV value in the first place
      (`architecture-reset.md`).
- [ ] D11 -- General and 4-state / wide import marshaling on the execution backend: the D2 and D3
      surface -- `output` / `inout` copy-back, `chandle`, and canonical `svBitVecVal*` /
      `svLogicVecVal*` buffers. Blocked on closures reaching that backend: a by-pointer argument is
      marshaled by a sequence of statements in expression position, which MIR expresses as a
      closure, so nothing on this path lowers today. The DPI-specific remainder once that lands is
      the buffer constructors and the canonical-plane marshaling primitives. A `real` import rides
      on the real value domain, not on this item.
- [ ] D12 -- Export and DPI tasks on the execution backend, once the C++-backend export and task
      items (D4-D6b) define the shape.

## Design record

The two questions that gated the design -- where marshaling lives and how an export wrapper recovers
its context -- are settled in `../decisions/dpi-foreign-boundary.md`, along with the callable model
and the out-of-scope boundary. Work proceeds against that record.

## Cross-references

- Design record: `../decisions/dpi-foreign-boundary.md` (the settled callable / marshaling / export
  model and the rejected alternatives).
- LRM 35: 35.4 (imported subroutines), 35.5 (functions and tasks; type mapping 35.5.6; scope
  35.5.3), 35.9 (disable protocol).
- Architecture: `callable.md` (the external-symbol callable), `backend_contract.md` (mechanical
  per-type marshaling), `emission_model.md` (the SDK as link-time-resolution substrate, per-unit
  artifacts, header and link), `runtime_distribution.md` (link and run model), `scheduling.md`
  (suspending tasks).
- Rides on: `scheduling.md` and `processes.md` (suspending DPI tasks, D6).
- `functions.md` lists DPI as its out-of-scope sibling; the module-scoped export D4a is the
  `ibex.md` full-top frontier.
