# DPI-C (import and export)

Tracks the SystemVerilog Direct Programming Interface (LRM 35): `import "DPI-C"`, where SV calls a
foreign C function, and `export "DPI-C"`, where foreign C calls an SV subroutine. This is the
foreign-language-boundary workstream, which the user-subroutine surface excludes.

A DPI import is not a separate call subsystem: it is the **external** implementation form of the one
callable concept (`callable.md`) -- a signature plus a foreign linkage name and calling convention,
with no body -- and an import call is an ordinary call. A DPI export is an ordinary internal SV
subroutine that additionally gets a foreign-linkage entry point; the entry obtains its runtime
context (design object, engine, and, for an export declared in a structural scope, the calling
instance) from a runtime-installed context, never from the foreign caller. An exported name is one
program-global symbol while the subroutine behind it is compiled once per specialization of its
declaring scope, so the symbol resolves against the scope the foreign call established rather than
naming any one of them. Export is supported within the LRM import -> export call chain, under Lyra
as the driver; the distinct execution model where an external C program drives a Lyra design as a
linked library is a separate roadmap capability, out of scope here. The DPI type mapping between an
SV type and its C ABI type (LRM 35.5.6) is a backend type-mapping concern, so the MIR representation
is backend-agnostic: the same MIR is materialized by the C++ backend as an `extern "C"` entry linked
by the emitted build recipe, and by the LLVM / JIT backend as an external-linkage symbol resolved by
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

Two frontiers are open, the two backends.

On the C++ backend the boundary runs in both directions and the surfaces below are closed: import
(D1-D3, D13) including open arrays (D8), export (D4, D4a-D4d) including instance-bound,
generate-scope, and receiver-less dispatch, DPI tasks in both directions including one that consumes
simulation time (D5, D6, D6b, D6d), the `svdpi` context surface (D7), and the generated ABI header
with link-input orchestration (D9). What remains is the disable protocol across the boundary (D6c)
and the element types Annex H.7.3 puts in C-compatible representation.

On the execution backend scalar import (D10) is in: a foreign call lowers to an external-linkage
symbol and the by-value carriers marshal. The rest of the import surface (D11) is blocked, and not
by anything DPI owns: by-pointer marshaling is expressed as a closure, which that backend does not
yet lower at all (`execution-backend.md`). Export and tasks there (D12) follow once the C++-backend
items fix their shape.

## Sub-Steps

The `D*` IDs are stable references. They do not impose a total order; real dependencies are stated
inline. D1-D9 and D13 are deliverables on the C++ backend; D10-D12 bring the same backend-agnostic
MIR up on the execution backend, mirroring the C++-backend items one surface at a time.

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
  - [x] A signature carrying a type outside that mapping (LRM 35.5.6, Table H.1) is rejected at the
        declaration rather than mis-marshalled at the call. The wired carriers are the packed
        vector, the by-value scalar, `real`, `string`, and `chandle`; `shortreal` and a packed
        struct / union are the notable types outside them.
- [x] D13 -- Where an import is declared does not restrict who calls it. A declaration in a package
      (LRM 26.3) or at `$unit` scope (LRM 3.12.1) is called from any unit, exactly as one in the
      calling scope is: an imported subroutine resolves to a program-global symbol in a name space
      of its own (LRM 35.4), so the declaration's position is a name-resolution fact only and the
      call crosses no unit boundary -- the calling unit holds its own record of the ABI projection
      and depends on no other unit's artifact. A `context` import declared in such a namespace
      observes no scope (LRM 35.5.3), a package and `$unit` never being instantiated; a
      receiver-less export stays directly reachable from it, and any other needs `svSetScope`, which
      is what the LRM already requires of a caller with no scope of its own.

### Export: foreign C calls SV

Export is scoped to the LRM import -> export call chain under Lyra as the driver (an imported C
function, called from a running simulation, calls back an exported SV function). The distinct model
where an external C program is the `main` driving a Lyra design as a linked library is out of scope
here (see the design record); a design that declares an export only for such an external driver is
accepted, records its metadata, and can have its ABI header generated, but is not claimed callable
from an external main.

- [x] D4 -- The scalar export foreign-linkage entry point and the import -> export call chain under
      Lyra as the driver (LRM 35.5): a scalar, input-only export executes end to end. The entry
      marshals the C arguments, recovers the exported subroutine's receiver from the running design,
      calls the method, and marshals the result back. The subroutine keeps its ordinary body; the
      wrapper's marshaling is stated in MIR so a backend renders it mechanically, and only the
      receiver recovery and the external linkage are the backend's shell.
- [x] D4a -- Instance-bound dispatch beyond the single top-level instance (LRM 35.5.3): a
      module-scoped export dispatches to the specific instance the foreign call targets, resolved
      from the current DPI scope the call chain established -- the calling context import's scope,
      or one `svSetScope` redirected to -- rather than the single-instance recovery above; a
      receiver-less package-scoped export needs no instance at all and calls its package function
      directly (riding on the package unit and callable in `packages.md`, PK1-PK2). Reaching any
      export now requires a valid scope context, so its caller must be a context import or set the
      scope with `svSetScope` (LRM 35.5.3). This is the `mhpmcounter_num` / `mhpmcounter_get` shape
      in the Ibex bring-up; the pure-SV Ibex run never calls them, so this closes the construct, not
      the Ibex external-driver usage.
- [x] D4d -- An export declared inside a generate block (LRM 27.6): the subroutine belongs to that
      block's scope object rather than to the module, and the entry point recovers that scope as its
      receiver the same way D4a recovers an instance. The entry point is a program-global C symbol
      outside every scope (LRM 35.7), so what this needed was for a generate scope to be nameable
      from there at all. This is the icache scramble-key shape in the Ibex bring-up.
- [x] D4c -- `$unit`-scoped receiver-less export (LRM 35.7): a subroutine at compilation-unit scope
      (LRM 3.12.1) exports exactly as a package function does. The `$unit` scope is an anonymous
      namespace unit, so its export rides the same receiver-less entry as D4a's package export with
      no added machinery.
- [x] D4b -- 4-state and by-pointer packed export marshaling. An `output` / `inout` scalar crosses
      by pointer to its by-value carrier; a packed vector of any direction crosses by pointer as its
      canonical `svBitVecVal*` / `svLogicVecVal*` buffer, its planes reshaped without a per-bit
      transcode. The `output` / `inout` values ride the completion payload the exported subroutine
      already returns (LRM 13.5), so the entry destructures the payload and marshals each component
      out through its foreign pointer. A function result stays a by-value return, limited to a small
      value (LRM 35.5.5): an atom such as `int` / `longint` / `real` / `string` / `chandle`, or a
      scalar `bit` / `logic`; a packed-vector, `integer`, or `time` result is not a small value and
      cannot be returned.

### DPI tasks

A DPI task rides the same task call protocol as any SV task (LRM 13.4.4): the call is a suspension
point, lowered to a coroutine the caller awaits. A task is classified by kind, never by whether it
consumes time -- a task that consumes no simulation time is the trivial case of one that does, on
the same protocol, not a separate path. D5 and D6 are the boundary directions; D6b lets a foreign
task actually suspend across the C boundary while simulation time advances, and D6c adds the disable
protocol on top of it.

- [x] D5 -- DPI import task (LRM 35.5.2): SV calls a foreign C task. The call rides the uniform task
      protocol -- a coroutine the caller awaits -- with the actuals marshaled in and the writeback
      arguments marshaled back; a task that consumes no time completes within the await.
- [x] D6 -- DPI export task (LRM 35.8): foreign C calls an SV task through its foreign-linkage
      entry. The entry drives the exported task's coroutine body to completion -- the foreign caller
      is not a coroutine and cannot await it -- marshals its writebacks back across the boundary,
      and returns the disable-acknowledgment int.
- [x] D6b -- Time-consuming foreign task (LRM 35.5.2): a foreign task consumes simulation time by
      calling back an exported SV task that suspends on a delay, an event, or a wait. The foreign
      call stack is parked across the boundary while simulation time advances and then resumes, so
      an imported task and the exported task it drives both suspend and continue across the
      boundary. Rides on the timing and suspension machinery (`scheduling.md`, `processes.md`).
- [ ] D6c -- The disable protocol (LRM 35.9): a `disable` reaching a process suspended inside a
      foreign task returns control across the C boundary the runtime does not own, cooperatively via
      the disable-acknowledgment return rather than a stack unwind.
- [x] D6d -- Side-effect attribution inside an exported subroutine body. `%m` (LRM 21.2.1.5) renders
      the exported subroutine's own instantiated position, and a severity task (LRM 20.10) tags its
      report with the call site, so both already read where the exported body is rather than where
      its caller is. A deferred `unique` / `priority` violation report is owned by the executing
      process (LRM 12.4.2.1), and reaching an export creates no new process -- the exported body
      runs as part of the process whose foreign call reached it -- so that process is the correct
      owner and no DPI-specific attribution exists. Fork parenting follows for the same reason (LRM
      9.5).

### Context and the svdpi surface

- [x] D7 -- `context` imports and the `svdpi` runtime surface (LRM 35.5.3, Annex H): DPI scope
      handles, set / get scope, resolve a scope to and from its fully qualified name, per-scope user
      data, and time queries (a scope's effective unit and precision, and the current time scaled to
      it). A context import observes the instantiated scope of its declaration, established for the
      duration of its foreign call; the current scope rides the calling process, so two
      time-consuming context imports suspended concurrently never observe each other's scope. Every
      export is a context function reached through the same run context (LRM 35.7).

### Open arrays

- [x] D8 -- Open-array arguments (LRM 35.5.6.1): a formal that leaves a dimension unsized accepts
      actuals of any size and range, in every direction. The actual crosses as a canonical image of
      the whole array (LRM Annex H.7.3, the representation `"DPI-C"` specifies for an open array),
      and a writeback direction reconstructs one SV value from that image and stores it, so nothing
      aliases the actual's storage. The foreign side gets the introspection surface (bounds, size,
      increment, dimension count, reported per dimension from the declared range the call site
      supplied), the addressing surface (whole-array and element addresses, answered where an
      element's canonical form is also how an individual value of its type crosses and with a null
      where it is not, per Annex H.12.4), and the canonical and scalar element accessors. Element
      types are the 2- and 4-state scalar and packed ones; the design record is
      `../decisions/dpi-open-array-boundary.md`.
  - [ ] An element type Annex H.7.3 puts in C-compatible rather than canonical representation
        (`real`, `shortreal`, `string`, `chandle`, an unpacked struct) is legal SystemVerilog that
        is rejected: serving it needs C-compiler layout for the element, which a SystemVerilog value
        does not have. The same limitation rejects a sized (non-open) unpacked array argument, which
        Annex H.11.4 requires to have that layout.
  - [ ] An open array of more than three unpacked dimensions is rejected. The foreign side reaches
        an element through the one-, two-, and three-index entries of Annex H.12.3; the
        variable-argument forms a deeper array needs are not published.

### Driver and link

- [x] D9 -- The ABI header and link-input orchestration. A repeatable `--dpi-link` option names each
      native source that supplies foreign symbols; the inputs are classified and checked before any
      backend runs, so a mistyped path is reported against the command line rather than as a
      compiler error much later. A generated C header declares every DPI-C name the design takes
      part in -- the imports the user's C must define and the exports it may call -- so their C
      compiles against the real signatures instead of hand-copied ones, and a definition that
      disagrees with its declaration is now a compile error rather than a silent ABI mismatch. It is
      generated from the design's own foreign declarations and is target-language-neutral, so the
      same header serves either backend. An emitted project carries that header, the standard DPI-C
      header its types are spelled in, and a copy of each foreign source, and its build recipe
      compiles and links them -- so a project that crosses the boundary still builds where neither
      Lyra nor the original foreign sources are reachable. Naming the same link inputs from a
      project file rides on project mode itself (`dev-ergonomics.md`), not on anything DPI owns.

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
      (`execution-backend.md`).
- [ ] D11 -- General and 4-state / wide import marshaling on the execution backend: the D2 and D3
      surface -- `output` / `inout` copy-back, `chandle`, and canonical `svBitVecVal*` /
      `svLogicVecVal*` buffers. Blocked on closures reaching that backend: a by-pointer argument is
      marshaled by a sequence of statements in expression position, which MIR expresses as a
      closure, so nothing on this path lowers today. The DPI-specific remainder once that lands is
      the buffer constructors and the canonical-plane marshaling primitives. A `real` import rides
      on the real value domain, not on this item.
- [ ] D12 -- Export and DPI tasks on the execution backend, once the C++-backend export and task
      items (D4-D6c) define the shape.

## Design record

The two questions that gated the design -- where marshaling lives and how an export entry recovers
its context -- are settled in `../decisions/dpi-foreign-boundary.md`, along with the callable model
and the out-of-scope boundary. Work proceeds against that record.

## Cross-references

- Design record: `../decisions/dpi-foreign-boundary.md` (the settled callable / marshaling / export
  model and the rejected alternatives) and `../decisions/dpi-open-array-boundary.md` (the open-array
  boundary object and where the formal's unsized shape lives).
- LRM 35: 35.4 (imported subroutines), 35.5 (functions and tasks; type mapping 35.5.6; scope
  35.5.3), 35.9 (disable protocol).
- Architecture: `callable.md` (the external-symbol callable), `backend_contract.md` (mechanical
  per-type marshaling), `emission_model.md` (the SDK as link-time-resolution substrate, per-unit
  artifacts, header and link), `runtime_distribution.md` (link and run model), `scheduling.md`
  (suspending tasks).
- Rides on: `scheduling.md` and `processes.md` (suspending DPI tasks, D6b).
- The scope-declared export D4a is the `ibex.md` full-top frontier.
