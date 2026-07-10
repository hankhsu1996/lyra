# DPI-C as a foreign arm of the one callable model

Date: 2026-07-08 Status: accepted

## Context

DPI-C (LRM 35) is the SystemVerilog / C foreign-language boundary: `import "DPI-C"` (SV calls a
foreign C function) and `export "DPI-C"` (foreign C calls an SV subroutine). It needs a home in the
post-reset architecture, where the pipeline is HIR -> MIR -> LIR -> LLVM and two backends consume
MIR: the C++ backend (transitional) and the LLVM/JIT backend.

A pre-reset DPI implementation exists as reference (`archived/`). It was built for an LLVM backend
and modeled DPI as its own parallel IR subsystem: a dedicated MIR call family (`DpiCall`,
`DpiImportRef`) separate from ordinary calls, and marshaling logic that lived in the LLVM backend.
That shape is a good source for the ABI type classification, the export-context mechanism, and the
header/link design, but its MIR node shape is a forbidden shape after the reset: `mir.md` bans a
node kind invented to express a runtime-library wrapper, and `callable.md` already defines the right
home.

This entry fixes how DPI fits the callable model, the value model, and the two-backend boundary
before implementation begins.

## The model

`callable.md` already states that a callable's implementation form is either an internal body or an
external symbol. DPI is the first client of the external form; it is not a special case:

```
Internal callable = signature + body
External callable = signature + foreign symbol + ABI
```

DPI marshaling is a value operation of the same kind the value model already uses everywhere else: a
real value reshape is a call against a runtime primitive (the C++ backend renders it as a runtime
method call; the JIT renders it as an opaque-handle `extern "C"` runtime-ABI call). A DPI boundary
conversion is one more such primitive, not backend-special lowering.

## Decisions

### 1. A DPI import is a receiver-less associated callable with an external-symbol implementation form

An imported subroutine lowers to a callable -- not a member of any object, and not a separate DPI
call family. Three structural facts fix its shape:

- **The implementation form is a variant on the callable.** A callable's code is either an internal
  body or an external symbol (a foreign linkage name, source language, and calling convention, with
  no body). External-ness is a structural variant on the callable's code, not a flag on an internal
  callable and not a separate incompatible declaration type -- `callable.md` invariant 7, mirroring
  how an object type is intra-unit or external-unit.
- **It has no receiver, so it is an associated callable, not an instance method.** A DPI import is
  called by name with no `self`: a non-context import cannot touch SV state, and a context import
  reaches its scope through the DPI scope handle, never an instance receiver. A receiver-less
  callable is a type-associated function in the enclosing unit's associated namespace, a category
  distinct from the instance method set (`object_model.md` invariants 7 and 8). It is stored in a
  class-level static-callable namespace -- `StaticCallableDecl`, the callable peer of the existing
  `StaticConstantDecl` -- never in the instance method arena.
- **It shares one declaration shape with SV class statics.** A DPI external callable and a future SV
  class static method are the same shape (a receiver-less associated callable, the former with an
  external-symbol code form, the latter with an internal one), reached through one associated
  call-target identity. This is the shared callable-declaration shape the callable model
  anticipates; the associated call-target arm is permanent and serves statics too, so it is added
  once, never as a temporary DPI-only variant refactored away later.

An import call is an ordinary call whose target is that associated callable.

The internal/external implementation variant is realized at the callable declaration layer -- the
static-callable declaration is either a bodied callable or an external symbol -- not inside the
shared callable-code struct. This still realizes the structural variant of `callable.md` invariant 7
(a variant, not a flag on a method), while leaving every instance-method, constructor, and closure
consumer of the callable code untouched; moving the variant into the callable code is a later
unification option, not a prerequisite.

Reason: putting the import in the instance method arena is a category error (it has no receiver and
is not an instance member); a separate incompatible external-declaration type contradicts the one
callable-declaration shape the model unifies toward; a DPI-specific target id would be a temporary
variant. The associated-callable home follows the object model's receiver-less-function category
directly, reuses the static-constant precedent, and is the home a future static method needs, so it
is built once.

### 2. Two signatures, separate but linked

DPI keeps the SV semantic signature and its ABI projection as distinct, connected records:

```
CallableSignature   = the SV semantic signature (type checking, diagnostics; slang already validated it)
ForeignAbiSignature = the ABI projection of that signature: per parameter an ABI type class,
                      direction, and passing mode; a return ABI class and return kind
ExternalSymbol      = foreign name + language + calling convention + ForeignAbiSignature
```

The ABI type class is a fixed function of an SV type and direction, computed once at lowering and
carried on the signature; a backend reads it and never re-derives it. Header generation, ABI
diagnostics, and the foreign-call ABI all read `ForeignAbiSignature`; SV semantic checking stays on
`CallableSignature`. The import C linkage name (which slang does not persist for imports) and the
pure/context properties live on `ExternalSymbol`, not on the SV callable.

Reason: conflating the SV signature with the C ABI signature loses the separation that keeps type
checking, diagnostics, and header emission each reading the surface they need.

### 3. Marshaling is a cross-ABI carrier conversion through runtime primitives, expressed in MIR

The boundary conversion between an SV value and its foreign ABI carrier is an ordinary MIR call
against a runtime conversion primitive, emitted at HIR-to-MIR. An import call desugars to: marshal
each input to its carrier, call the external symbol over the carriers, marshal each output carrier
back to the actual's write path.

The carrier is a **backend-ABI carrier type**, not an SV value-semantic type. It lives in the same
category as the runtime plumbing types the MIR type system already carries (the services, scope,
reference, and pointer types) and maps through the per-backend type-mapping dispatch to the target C
ABI type. Its lifetime is a call-lowering artifact: it is produced by a marshal-in primitive and
consumed by the foreign call or a marshal-out primitive, never held as a user variable.

Invariant -- the carrier is ABI-temporary. The carrier type is produced only by marshal lowering;
its only legal occurrences are the result type of a marshal-to-carrier call and the operand or
result type of a foreign call. It is never a variable's type, never appears in a user expression or
type check, is never stored to an SV variable, and never escapes the single lowered-call window
(marshal-in, foreign call, marshal-out, one statement). It is a runtime plumbing type, never a
member of the SV value-type set.

Reason: the two backends have different runtime ABIs (native value methods vs opaque-handle
`extern "C"` calls). A conversion expressed as a MIR call renders mechanically on both; a
backend-realized conversion would be written twice and drift. The carrier being a plumbing type, not
a value type, keeps it out of the SV value model and adds no new MIR expression primitive -- it is a
type in an existing category, and the conversion is an ordinary call.

### 4. A DPI export is an internal callable plus foreign-wrapper metadata; context is a thread-local ambient handle

An exported subroutine is an ordinary internal SV callable. The export contributes metadata that
tells a backend to also materialize a foreign-linkage wrapper: the wrapper marshals the ABI
arguments, obtains the runtime context, calls the internal callable, and marshals the result back.
The wrapper is a backend realization; the metadata and the ABI signature are backend-neutral.

The wrapper obtains its context (design object, engine, and, for a module-scoped export, the calling
instance) from a **thread-local ambient context** installed for the duration of a run, not from the
foreign caller. Every backend funnels its run through one shared entry (`RunSimulation` over the
engine), which is the single install point. A module-scoped export resolves its instance from the
scope the foreign side established (LRM 35.5.3 `svSetScope`); every export is a context function
(LRM 35.7).

Invariant: the DPI export context is valid only while a Lyra simulation engine is actively running
on the current thread. Nested foreign entries (an import called from inside an export) push and pop
a thread-local stack. A foreign callback that runs on a different thread must install its own
context on that thread; this is a stated constraint, not a supported path today.

Reason: the runtime is otherwise entirely explicit-pointer-threaded with no ambient anchor, so a
wrapper that receives only plain C arguments has nothing to recover the context from. A thread-local
handle is the precise scope (it does not outlive the run, and it does not assume a single global
engine), which keeps a future parallel or multi-engine test from aliasing one global.

### 5. The "SV as a library driven by an external C main" execution model is out of scope

DPI export is supported within the LRM import -> export call chain, under Lyra as the driver: an
imported C function, called from a running Lyra simulation, calls back an exported SV function. The
distinct execution model where an external C/C++ program is the `main` and drives a Lyra design as a
linked library -- which is what a Verilator-style C++ testbench needs -- is a separate roadmap
capability (it needs standalone/object emission and an embedding entry point), not part of the DPI
base implementation. A design that declares an export whose only consumer is such an external driver
is accepted, records its metadata, and can have its ABI header generated, but is not claimed to be
callable from an external main.

Reason: both backends today run Lyra as the driver (the C++ backend emits a self-contained program;
the LLVM backend runs in-process). Folding the external-driver model into DPI would drag standalone
emission, an embedding API, and driver lifecycle into a base feature and let one design's incidental
usage inflate the scope.

## Rejected alternatives

- **A separate DPI call family (`DpiCall` / `DpiImportRef`), as in the archive.** A DPI-specific
  bypass around the callable and call vocabulary; a forbidden shape after the reset (`mir.md`). The
  external-callable arm carries the same information without a parallel subsystem.
- **Backend-realized marshaling driven by the ABI signature.** Forces DPI-ABI-driven conversion
  logic into value emission, which `backend_contract.md` forbids, and duplicates the logic across
  the two backends' different runtime ABIs.
- **The ABI carrier as an SV value-semantic type or a new MIR expression primitive.** Pollutes the
  value model with a foreign-ABI shape and opens the closed primitive set. The carrier is a plumbing
  type in an existing category, materialized only during call lowering.
- **A process-global (non-thread-local) export context.** Works under a single-threaded engine but
  assumes one global engine and aliases across concurrent or multi-engine runs; the thread-local
  form is the precise scope.
- **The imported callable stored as an instance method (in the per-class method arena).** A DPI
  import has no receiver and is not an instance member; a receiver-less callable is a
  type-associated function (`object_model.md` invariants 7, 8), so its home is the
  associated-callable namespace, not the instance method set.
- **A temporary DPI-only callable-target variant, unified later.** Leaves a DPI-specific identity in
  the IR to be refactored away; the unification is done once, consistent with the reset.

## Consequences

- Both backends consume the same MIR for a foreign call and a marshal conversion; only the
  type-mapping of the ABI carrier and the realization of the foreign-linkage symbol differ.
- Foreign-symbol linkage is a per-backend concern: the C++ backend resolves it through a
  user-link-input seam in its build recipe; the LLVM/JIT backend resolves it through external-symbol
  resolution in its execution session.
- The export-context install composes into the one shared run entry, so it serves both backends from
  a single place.

## Cross-references

- `docs/architecture/callable.md` (the external-symbol implementation form; direction as data flow)
- `docs/architecture/mir.md` (closed primitive set; no invented runtime-library node kind)
- `docs/architecture/backend_contract.md` (mechanical render; runtime-library names only in
  type-mapping)
- `docs/architecture/emission_model.md` (per-unit artifact; the runtime SDK as link-time substrate)
- `docs/decisions/jit-value-realization.md` (the opaque-handle runtime ABI the JIT marshals through)
- `docs/decisions/generated-behavior-boundary.md` (backend-neutral native-entry ABI; symbol lookup
  by specialization identity)
- LRM 35: 35.4 (imports), 35.5 (functions/tasks; type mapping 35.5.6; scope 35.5.3), 35.7 (exports
  are context functions), 35.9 (disable protocol)
