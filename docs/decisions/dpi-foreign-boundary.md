# DPI-C as a foreign arm of the one callable model

Date: 2026-07-08 Status: accepted

## Context

DPI-C (LRM 35) is the SystemVerilog / C foreign-language boundary: `import "DPI-C"` (SV calls a
foreign C function) and `export "DPI-C"` (foreign C calls an SV subroutine). It needs a home in an
architecture whose pipeline is HIR -> MIR -> LIR -> LLVM and whose two backends consume MIR: the C++
backend (transitional) and the LLVM/JIT backend.

An earlier DPI implementation informed this entry. It modeled DPI as its own parallel IR subsystem:
a dedicated MIR call family (`DpiCall`, `DpiImportRef`) separate from ordinary calls, and marshaling
logic that lived in the backend. That shape is a good source for the ABI type classification, the
export-context mechanism, and the header/link design, but its MIR node shape is forbidden: `mir.md`
bans a node kind invented to express a runtime-library wrapper, and `callable.md` already defines
the right home.

This entry fixes how DPI fits the callable model, the value model, and the two-backend boundary
before implementation begins.

## The model

`callable.md` already states that a callable carries its signature and a body only where the
declaration defines it, and that foreign linkage is an axis over that. DPI is the first client of
both; it is not a special case:

```
definition          = signature + body
import              = signature, no body, foreign linkage
export entry point  = signature + body,  foreign linkage
```

DPI marshaling is a value operation of the same kind the value model already uses everywhere else: a
real value reshape is a call against a runtime primitive (the C++ backend renders it as a runtime
method call; the JIT renders it as an opaque-handle `extern "C"` runtime-ABI call). A DPI boundary
conversion is one more such primitive, not backend-special lowering.

## Decisions

### 1. A DPI import is a bodyless receiver-less callable the unit owns

An imported subroutine lowers to a callable -- not a member of any object, and not a separate DPI
call family. Three structural facts fix its shape:

- **Not defining it is the whole of what makes it external.** The callable carries its signature and
  simply has no body, and its foreign linkage names the symbol the linker resolves it against.
  Neither is a flag on a callable and neither is a separate declaration type -- `callable.md`
  invariant 7.
- **It has no receiver, so it is an associated callable, not an instance method.** A DPI import is
  called by name with no `self`. A non-context import cannot touch SV state at all; a context import
  observes the instantiated scope of its declaration, which the call site establishes around the
  foreign call rather than binding as a receiver on the callable. A receiver-less callable is a
  type-associated function (`object_model.md` invariants 7 and 8), a category distinct from the
  instance method set.
- **Its symbol is global, so the unit owns it.** The linkage name lives in the DPI-C name space,
  which is separate from every compilation-unit scope name space (LRM 35.4). No class contains it,
  so the unit that takes part in the import publishes the prototype, beside the entry point an
  export publishes: both directions of the boundary are owned alike and separated only by whether
  the declaration has a body.

An import call names that symbol directly. Because the name space is its own, the call reaches it
without naming a class or a unit, and a declaration written in a package or at `$unit` scope is
called from any unit with no cross-unit reference at all. What the calling unit needs is the
declaration's ABI projection -- a fact of the declaration, identical wherever it is read -- so it
holds its own copy and depends on no other unit's artifact.

Reason: putting the import in a class's callable arena is a category error twice over -- it has no
receiver, and its symbol belongs to a name space no class contains -- and it makes an import's
presence shift the identity of the class's other callables. A separate incompatible
external-declaration type contradicts the one callable-declaration shape the model unifies toward.
Owning the declaration at the unit and naming the symbol by its linkage name is what makes the
package and `$unit` cases fall out with no mechanism of their own.

### 2. The C prototype is the callable's own signature; linkage is a separate record

DPI keeps the SV semantic signature and the C prototype distinct, but the C prototype is not a
record of its own -- it is the callable's signature, in the shape every callable already carries:

```
CallableSignature = the SV semantic signature (type checking, diagnostics; slang already validated it)
callable code     = the C prototype: one binding per formal, typed as the value crosses the
                    boundary, plus the result type. A bodyless callable carries it with an empty
                    body, exactly as a pure virtual prototype does.
ForeignLinkage    = the linkage name the symbol is reached by. Source language and calling
                    convention are implicitly C, the only foreign linkage; a second one adds them.
```

`ForeignLinkage` is an independent axis on a callable declaration, not part of the bodyless
implementation form: a bodyless callable carrying it is an import, a bodied one is the entry point
an export publishes, and nothing tags which. The prototype is a fact of the program-global **name**,
not of any one declaration of it -- two scopes may export one name, and a program-level consumer
such as the generated ABI header reads the name without holding any declaration -- so the signature
is stated on the linkage. A declaration's own formals are that same signature realized as bindings a
body can name, derived from one projection, so the two cannot state different prototypes.

The boundary type of a formal is a fixed function of its SV type and direction, computed once at
lowering and interned there. The ABI **classification** that computes it -- the carrier and the
direction, LRM 35.5.6 and 35.5.1.2 -- stays on the source-near declaration, which is where a call
site reads it to build its marshaling and where the diagnostic for a signature outside the mapping
is reported. Nothing below reads it: an import's marshaling is expanded at the call, an export's is
lowered into its entry point's body, so by the time a backend sees a foreign callable all that
remains is its signature and its linkage name. SV semantic checking stays on `CallableSignature`.

Reason: conflating the SV signature with the C prototype loses the separation that keeps type
checking, diagnostics, and header emission each reading the surface they need. Keeping the prototype
as MIR-typed bindings rather than a carrier list is what lets every consumer render it mechanically
through its target's ordinary type mapping; a carrier list would have each of them re-implement LRM
35.5.6, and two implementations of one mapping drift.

### 3. Marshaling is a cross-ABI carrier conversion through runtime primitives, expressed in MIR

The boundary conversion between an SV value and its foreign ABI carrier is an ordinary MIR call
against a runtime conversion primitive, emitted at HIR-to-MIR. An import call desugars to: marshal
each input to its carrier, call the external symbol over the carriers, marshal each output carrier
back to the actual's write path.

That desugaring happens at each call rather than once per declaration, because an open-array formal
leaves a dimension unsized (LRM 35.5.6.1): what crosses is then a function of the _actual's_ static
type, which only the call site holds. One mechanism serves every import, so the call site is where
all of them marshal.

The carrier is a **backend-ABI carrier type**, not an SV value-semantic type. It lives in the same
category as the runtime plumbing types the MIR type system already carries (the services, scope,
reference, and pointer types) and maps through the per-backend type-mapping dispatch to the target C
ABI type. Its lifetime is a call-lowering artifact: it is produced by a marshal-in primitive and
consumed by the foreign call or a marshal-out primitive, never held as a user variable.

Invariant -- the carrier is ABI-temporary. The carrier type occurs only where the boundary itself
is: a foreign signature's formals and result, the boundary object an argument crosses in, and the
marshal calls that convert between it and an SV value. It is never a user variable's type, never
appears in a user expression or type check, is never stored to an SV variable, and on the calling
side never escapes the single lowered-call window (marshal-in, foreign call, marshal-out, one
statement). It is a runtime plumbing type, never a member of the SV value-type set.

Reason: the two backends have different runtime ABIs (native value methods vs opaque-handle
`extern "C"` calls). A conversion expressed as a MIR call renders mechanically on both; a
backend-realized conversion would be written twice and drift. The carrier being a plumbing type, not
a value type, keeps it out of the SV value model and adds no new MIR expression primitive -- it is a
type in an existing category, and the conversion is an ordinary call.

### 4. A DPI export is an internal callable plus a foreign entry point; context is a thread-local ambient handle

An exported subroutine is an ordinary internal SV callable. The export additionally contributes an
entry point carrying the foreign linkage above: it marshals the ABI arguments, calls the exported
subroutine, and marshals the result back. Its body is ordinary MIR a backend renders mechanically;
only the external linkage is the backend's shell.

The C name is a program-global symbol in the DPI name space and never a class member (LRM 35.4,
35.7), while the subroutine behind it may be compiled once per specialization of the scope declaring
it -- so the two are separated. A scope **publishes** an entry, taking the scope it runs against
ahead of the C formals; the symbol resolves that entry against the scope in effect and calls it, and
belongs to the design root, the one place a name several scopes may export has an owner (LRM 35.4).
A package subroutine has no receiver and a package has one form, so the two collapse: the package's
own namespace defines the symbol directly.

The entry obtains its context (design object, engine, and, for an export declared in a scope, the
calling instance) from a **thread-local ambient context** installed for the duration of a run, not
from the foreign caller. Every backend funnels its run through one shared entry (`RunSimulation`
over the engine), which is the single install point. Such an export resolves its instance from the
scope the foreign side established (LRM 35.5.3 `svSetScope`); every export is a context function
(LRM 35.7). The declaring scope is any structural scope -- a module, or a generate scope, whose type
the unit declares within another -- so the entry point names it by the same one spelling every other
reference to that type uses.

That resolution is **checked, not asserted**. Which scope the foreign side established is not a fact
the compiler can know: LRM 35.5.3 obliges the caller to reach an export only from its own scope and
obliges `svSetScope` to name a scope that declares it, and neither obligation is enforceable on this
side. The lookup is where that is settled -- a scope that publishes no entry under the name is
reported, not proceeded past. What the lookup returns is generated code of the scope it was found
on, so narrowing that scope to the entry's own instance type needs no second check: the pairing is
what the table states. Every other typed recovery in the compiler rests on a compile-time proof -- a
resolved reference, an explicit receiver parameter, a scope the runtime itself allocated -- and this
one rests on a runtime lookup that has to succeed before any receiver exists.

Invariant: the DPI export context is valid only while a Lyra simulation engine is actively running
on the current thread. Nested foreign entries (an import called from inside an export) push and pop
a thread-local stack. A foreign callback that runs on a different thread must install its own
context on that thread; this is a stated constraint, not a supported path today.

Reason: the runtime is otherwise entirely explicit-pointer-threaded with no ambient anchor, so a
symbol that receives only plain C arguments has nothing to recover the context from. A thread-local
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

- **A separate DPI call family (`DpiCall` / `DpiImportRef`).** A DPI-specific bypass around the
  callable and call vocabulary, which `mir.md` forbids. The external-callable arm carries the same
  information without a parallel subsystem.
- **Backend-realized marshaling driven by the ABI signature.** Forces DPI-ABI-driven conversion
  logic into value emission, which `backend_contract.md` forbids, and duplicates the logic across
  the two backends' different runtime ABIs.
- **The ABI carrier as an SV value-semantic type or a new MIR expression primitive.** Pollutes the
  value model with a foreign-ABI shape and opens the closed primitive set. The carrier is a plumbing
  type in an existing category, materialized only during call lowering.
- **A process-global (non-thread-local) export context.** Works under a single-threaded engine but
  assumes one global engine and aliases across concurrent or multi-engine runs; the thread-local
  form is the precise scope.
- **The imported callable owned by a class.** Beyond the category error decision 1 states, a class
  arena orders its entries, so an import's presence shifts the identity of every callable declared
  after it -- and a package, which has no class at all, could then declare no import.
- **A temporary DPI-only callable-target variant, unified later.** Leaves a DPI-specific identity in
  the IR to be refactored away; the unification is done once instead.
- **The program-global export symbol as its own species beside the callable arena.** It is exactly
  what a unit-level namespace callable already is -- receiver-less, bodied, owned by the unit that
  defines it -- plus a linkage name, so a parallel container would give it a second declaration
  shape, a second render path, and a second thing every consumer must walk. Worse, it would leave
  the export with no prototype record while the import has one, so the two directions of one
  boundary are modeled differently and a generated header derives the same LRM 35.5.6 mapping twice.
  The per-specialization entry a scope publishes is a different object and is not what this rejects:
  the runtime holds it by address in the scope's table, which is what a scope's lifecycle entries
  already are, so it joins that species instead of inventing one, and it carries the linkage too --
  so neither direction is left without a prototype.
- **The C prototype as a record beside the callable rather than the callable's signature.** A
  bodyless callable looks like it has no signature to put it on, so the prototype gets its own home
  and only the bodyless direction reads it -- which forces a second signature-rendering path for
  imports beside the ordinary one every other callable uses, and the two agree only by construction.
  A bodyless callable carrying its signature is the shape a pure virtual prototype already uses for
  exactly this reason.
- **Foreign linkage as part of the bodyless implementation form.** Ties "reached under a C symbol"
  to "has no body", which the export half contradicts: its entry point has both. Linkage and body
  presence are independent, and keeping them independent is what makes the direction readable from
  the structure instead of from a tag.
- **An implementation-form species -- internal, external, prototype -- as the discriminator.** Once
  every arm carries the signature, the species stops discriminating structure and only restates
  which of the other facts is present: external means "no body and foreign", prototype means "no
  body and a dispatch role". Two facts that must agree drift, and the peer languages this IR models
  itself on do not carry the species either -- a declaration has an optional body, a separate
  language linkage, and a separate pure marker.

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
