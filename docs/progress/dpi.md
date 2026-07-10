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
export (package / `$unit`-scoped and module-scoped instance-bound), DPI tasks (non-suspending and
suspending, both directions), the `svdpi` context surface, a generated ABI header, and link-input
orchestration.

## Actionable

D1 is the entry point: the thinnest end-to-end vertical -- one pure scalar import, materialized,
linked against a user-provided C object, and asserted by a mixed-language test -- standing up the
whole pipeline the later items extend. D1 lands three foundations together (the external-callable IR
shape, per-backend foreign-call materialization, and per-backend foreign-symbol linkage) and
completes as one deliverable: scalar import works end to end on both the C++ and the LLVM / JIT
backend. Every later item leaves the MIR shape backend-agnostic and lands on both backends.

The C++ backend half of D1 is done: a scalar import (2-state integral, `real`, and `string`,
`input`-only functions) declares, marshals across the boundary, calls the foreign symbol, and
marshals the result back; a user C source is linked in through a repeatable `--dpi-link` option, and
mixed-language tests assert the round trip. The remaining D1 work is the LLVM / JIT backend, which
does not yet resolve an external foreign symbol; the MIR is already backend-agnostic, so that half
is additive.

## Sub-Steps

The `D*` IDs are stable references. They do not impose a total order; real dependencies are stated
inline.

### Import: SV calls foreign C

- [ ] D1 -- Pure scalar import, end to end on both backends: 2-state integral scalars, `real`, and
      `string`, `input`-only, function kind (LRM 35.4, 35.5.1). An imported subroutine lowers to a
      callable whose implementation form is an external symbol, and an import call is an ordinary
      call to it; a user-provided C object is linked in (via the emitted build recipe on the C++
      backend, resolved through the execution session on the LLVM / JIT backend), asserted by a
      mixed-language test.
- [ ] D2 -- General import: `output` and `inout` arguments, non-pure imports, `chandle`, and packed
      2-state values up to a single machine word (LRM 35.5.5, 35.5.6). Bidirectional boundary
      temporaries for the copy-out directions.
- [ ] D3 -- 4-state and wide marshaling: 4-state scalars and vectors across the boundary (the DPI
      canonical 4-state layout versus Lyra's internal representation) and packed vectors wider than
      one machine word (LRM 35.5.6).

### Export: foreign C calls SV

Export is scoped to the LRM import -> export call chain under Lyra as the driver (an imported C
function, called from a running simulation, calls back an exported SV function). The distinct model
where an external C program is the `main` driving a Lyra design as a linked library is out of scope
here (see the design record); a design that declares an export only for such an external driver is
accepted, records its metadata, and can have its ABI header generated, but is not claimed callable
from an external main.

- [ ] D4 -- Package / `$unit`-scoped export with foreign-linkage wrappers: scalar 2-state, `real`,
      and `string` (LRM 35.5). Generated ABI header and driver linkage of the user C that calls the
      export.
- [ ] D4a -- Module-scoped export with instance-bound dispatch (LRM 35.5.3): the exported subroutine
      reads the state of the specific instance the foreign call targets, so the wrapper resolves
      that instance's context from the scope the foreign side set. This is the `mhpmcounter_num` /
      `mhpmcounter_get` shape in the Ibex bring-up; the pure-SV Ibex run never calls them, so this
      closes the construct, not the Ibex external-driver usage.
- [ ] D4b -- 4-state and by-pointer packed export marshaling, including indirect return for return
      types that need hidden result storage.

### DPI tasks

- [ ] D5 -- Non-suspending DPI tasks (LRM 35.5.2), import and export. Reuses the immediate-call
      path; a task keeps its task call protocol and is never rewritten into a function.
- [ ] D6 -- Suspending DPI tasks: a foreign task that consumes simulation time, including the
      disable protocol (LRM 35.9). Rides on the timing and suspension machinery (`scheduling.md`,
      `processes.md`).

### Context and the svdpi surface

- [ ] D7 -- `context` imports and exports and the `svdpi` runtime surface: DPI scope handles, set /
      get scope, the scope registry, and time queries. A context import observes the scope of its
      call site.

### Open arrays

- [ ] D8 -- Open-array arguments (LRM 35.5.6.1): the introspection and pointer surface (array
      bounds, size, dimension queries, element pointers) and packed / scalar element access.

### Driver and link

- [ ] D9 -- Link-input orchestration across both backends: a repeatable `--dpi-link` CLI option and
      a `lyra.toml` DPI link-inputs array, validated before any backend runs. The C++ backend adds
      the inputs to the emitted build recipe's link line; the LLVM / JIT backend resolves them
      through its execution session. A generated ABI header lets the user compile their C against
      the correct signatures.

## Design record and prerequisites

The two questions that gated the design -- where marshaling lives and how an export wrapper recovers
its context -- are settled in `../decisions/dpi-foreign-boundary.md`, along with the callable model
and the out-of-scope boundary. Work proceeds against that record.

Prerequisites surfaced during design:

- `chandle` has no runtime representation or type-mapping entry today; it must gain one before D2
  (where a `chandle` argument first crosses the boundary).
- Foreign-symbol linkage does not exist on either backend: the C++ backend needs a user-link-input
  seam in its build recipe, and the LLVM / JIT backend needs external-symbol resolution in its
  execution session; both are part of D1.

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
