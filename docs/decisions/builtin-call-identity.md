# Built-in call identity

Date: 2026-06-22 Status: accepted

## Context

A SystemVerilog programmer writes `s.len()`, `q.push_back(x)`, `a.sort()`, `e.triggered()`,
`MyEnum::first()`. The LRM defines each method under the chapter of the type it operates on (6.16
string, 6.19.5 enum, 7.9 associative array, 7.10 queue, 7.12 unpacked-array shared family, 15.5
named event). Lyra needs to identify each built-in call uniformly at HIR and at MIR so HIR-to-MIR
translates mechanically and every backend reads one shape.

The choice the compiler makes is: what is the identifier carried in the IR for a built-in method
call?

## Decision

The identifier is a flat closed-namespace enum, `lyra::support::BuiltinFn`. One entry per built-in
runtime entry across every family. Receiver type carries the type-side context; the callee carries
the function-side identity. Both HIR and MIR reference the same enum from the support layer (the
same pattern that `support::SystemSubroutineId` already follows for `$xxx` calls).

HIR's callee for a built-in method is `BuiltinMethodRef` carrying `support::BuiltinFn`. The struct
is a one-field wrapper, symmetric with `SystemSubroutineRef { id }`.

What the namespace holds is every built-in the runtime library carries out, and nothing else. An
operation the front end answers outright is not one: naming it here would make MIR carry a name for
something it can never hold, and every layer below carry an arm for an alternative it can never see.
The LRM defines two such operations as built-in methods, and each keeps its identity at the layer
that still holds what answers it.

LRM 7.12.4 `item.index` is answered by the with-clause binding it names. The receiver is discarded
and the value is the enclosing closure's index parameter -- a closure over an LRM 7.12 array method
carries both `item` and `index`, supplied per iteration -- so nothing is left to call, and
AST-to-HIR resolves it to the same iteration-binding reference `item` itself resolves to, differing
in which of the two roles it names. HIR therefore holds one shape for the pair the LRM defines
together, and no callee names it.

LRM 6.19.5's enumerated type methods are answered from the enumeration's declared members: `first` /
`last` / `num` are constants of that member table, and `name` / `next` / `prev` are a search through
it, which HIR-to-MIR emits as a callable synthesized once per enumeration. Here a call does survive
to HIR, because answering one needs the members in their lowered form and a class for the
synthesized callable to home on, and neither exists where names are resolved. So these keep the call
shape the source wrote, carried by a `SubroutineRef` arm of their own over a six-valued HIR-local
identity. Nothing below HIR names them.

MIR's callee for a built-in is one shape -- `Direct { target = BuiltinFn }` -- shared with
user-method calls, where `target` is the symbol identity (several alternatives today, one identity
space once callable identity is unified). The instance / static / free distinction is **not**
structural at MIR: at the generic-language layer these are one direct invocation, differing only in
whether the callee binds an object. The split into `BuiltinFnCallee` / `BuiltinStaticCallee` /
`FreeFnCallee` arms was a backend-convenience pre-classification that violated `mir.md` invariant 10
(a field a backend's realization can ignore, restating what the id and signature already fix), and
is gone.

The scope a static entry is reached on is not carried either, for the same reason. A factory is
declared on the type it builds, and the value a call to it answers with is of that type, so the
call's own type already states it -- a second statement at the call site is a fact with two
producers and nothing keeping them in step. No source-level qualifier survives to MIR to be carried
instead: the one construct that writes one, an enumerated type's methods, is answered above MIR
entirely.

HIR-to-MIR is a near-identity translation: pass the `BuiltinFn` through as `Direct::target`.

AST-to-HIR still dispatches by receiver type to choose which name-to-id lookup table to query --
`first` on an associative array resolves to `kAssocFirst`, and `first` on an enumeration resolves to
no runtime entry at all. The receiver-type dispatch is unaffected by the identifier shape; only the
lookup table's return type changes, and one of those tables now returns an identity of the front
end's own.

## Why flat

Three shapes were considered.

**D1: method-as-type-member.** `(TypeId, MethodNameOnThatType)`. Every type owns a method table; a
call is "method M on the type T of the receiver". Most source-true -- mirrors the SV programmer's
mental model of "len belongs to string". But the LRM 7.12 family (sort, sum, find, ...) is _defined_
as a single set of methods that applies uniformly to multiple unpacked-array containers (fixed
unpacked, dynamic, queue). D1 forces every container to spell `kSort` separately, three different
identifiers for one LRM concept. The IR cannot represent "the same method, shared across container
kinds" in a single entry, which is the load-bearing structural fact for LRM 7.12.

**D2: method-as-concept-member.** `(ConceptId, MethodInConcept)`. Methods grouped by runtime concept
(Indexable, Sliceable, Sortable, Reducible, Searchable, ...) -- the structure that the runtime
container types already implement via `lyra::value::concepts`. SV-faithful in the trait sense but
adds an indirection (name -> concept -> method) the AST-to-HIR receiver-type dispatch does not
naturally walk, and a single method may participate in multiple concepts (`kSize` is Sized;
`kReverse` is Sortable; `kElement` is Indexable). The concept axis is cross-cutting; making it the
identity axis forces choosing one concept per method, and the choice is artificial.

**D3: flat closed-namespace.** One enum, one entry per runtime function. Receiver type carries the
container/element shape; the enum carries the function identity. The LRM 7.12 family is one entry
per method (`kSort` once), and which container realizes it is the receiver type's job. Adding a new
container kind is zero-touch on the enum. Adding a new method is one entry. Adding a new method
shared across N containers is still one entry.

D3 wins on every evolution axis (cross-container sharing, scalability under new types, scalability
under new methods) without losing any SV-level expressivity -- the source-level "len belongs to
string" fact is preserved by the receiver's type at the call site, not by the callee identifier. The
cosmetic "method belongs to type" property D1 offers does not earn the cost of failing to represent
LRM 7.12.

## Rejected alternatives

- **Per-family enum variant at HIR
  (`BuiltinMethodRef = variant<EnumMethodKind, StringMethodKind, ArrayMethodKind, QueueMethodKind, AssociativeMethodKind, EventMethodKind, IteratorMethodKind>`).**
  The original shape, set when each family was being added incrementally and
  `runtime-effects-as-generic-calls.md` had not yet established the flat-callee pattern. The family
  axis carries information already in the receiver's type (the same fact in two places). The
  per-family arm is compiler scaffolding rather than a source-faithful structure: the LRM's chapter
  organization (6.16 string, 6.19.5 enum, ...) is documentation layout, not a semantic level of SV.
  The SV programmer writes `s.len()`, not "string-family method len"; the family is not a thing the
  source language exposes.

- **Per-family enum at HIR + flat enum at MIR (asymmetric).** Considered briefly on the grounds that
  HIR is SV-faithful and might justify the per-family structure as a mirror of the LRM's chapter
  organization. Rejected because the SV source itself does not surface family as a semantic level --
  so per-family at HIR is not more SV-faithful than flat at HIR, it is just one organizational
  choice. With no source-faithfulness gain, the residual reason to keep per-family at HIR collapses
  into "we already have it that way", which is not a design argument.

- **Different namespaces for HIR's `BuiltinFn` and MIR's `BuiltinFn`.** Considered to keep HIR / MIR
  strictly separated. Rejected because the identifier set is identical at both layers (the runtime
  entries are real entities both layers must name) and forking the enum forces a pure-renaming
  translation at HIR-to-MIR plus duplicate maintenance of every new entry. The shared enum lives in
  `lyra::support`, the same way `SystemSubroutineId` does, so neither layer imports the other's
  vocabulary.

- **Concept-keyed identity (D2 in full).** Sketched above. Rejected on indirection cost and the
  artificial single-concept choice per method.

- **Method-as-type-member (D1 in full).** Sketched above. Rejected because it cannot represent the
  LRM 7.12 cross-container family as one identity.

- **`item.index` as an inner variant arm inside `hir::BuiltinMethodRef`
  (`variant<support::BuiltinFn, IteratorMethodKind>`).** The historical shape, set when slang's
  parse choice (a `KnownSystemName::Index` system subroutine, syntactic-method-call form) was
  carried into HIR without re-examination. Rejected because two structurally different things shared
  one callee slot: `support::BuiltinFn` enumerates runtime entries (lower as identity-preserving
  `CallExpr`), while `IteratorMethodKind::kIndex` names no entry at all. The HIR-to-MIR visitor had
  to switch on the inner kind to pick the translation path, and the runtime-callee translation
  carried an unreachable "should have been rewritten" throw to handle the case it could never see.
  Taking it out of the callee entirely is what removes both.

- **`item.index` among the leaf primary atoms.** The objection is HIR's "preserve LRM-level
  constructs without flattening" identity: LRM 7.12 (Syntax 7-5) classifies `item.index` as an
  `array_method_call` (`expression . array_method_name`), one level above LRM 11.2.1's primary
  atoms, and a reference indistinguishable from `IntegerLiteral` / `StructuralVarRef` in a dump or a
  consumer visitor loses that. What answers it is that a primary of its own -- one naming the
  with-clause it belongs to and which of the clause's two bindings it is -- is neither a leaf atom
  nor a call. The construct LRM 7.12 defines is the pair of bindings a closure carries, `item` and
  `index` together, and one HIR shape carrying both at their real grammar level is what preserves
  it; a callee arm would have named half the pair and called it a call besides.

- **Folding a front-end-answered operation into the flat enum.** Considered as a way to keep "every
  built-in identity in one place". Rejected because the shared namespace would then carry a name for
  something MIR cannot represent, and every consumer below -- dump, both backends, the MIR-to-LIR
  lowering -- would need a dead arm for an alternative no node can hold. `item.index` was the case
  that established this; the LRM 6.19.5 enumerated type methods were folded in anyway and cost
  exactly what the rejection predicted, including a divergence where one backend refused them and
  the other gave them live entries. Both are now named at the layer that answers them.

## Backend rendering

The identity decision (flat `BuiltinFn`) is orthogonal to the shape a backend renders a call in. Two
shapes were considered for the C++ backend:

- **X -- C++ method-call syntax.** `recv.Name(args)` for value receivers, `recv->Name(args)` for
  pointer receivers. The receiver's MIR type carries pointer-vs-value; the backend reads it and
  picks the syntax mechanically.
- **Y -- free function with receiver as first argument.** `lyra::runtime::Name(recv, args)`, uniform
  across receiver kinds. A free-function adapter layer in the runtime forwards each free-function
  call to the underlying method.

X is the backend's chosen shape. The reasoning:

- `compiler_overview.md` makes emit readability load-bearing -- emitted C++ is "the human-readable
  rendering of MIR" used to validate MIR semantics by a developer reading from SystemVerilog source.
  Method-call syntax mirrors the SV form (`q.push_back(x)` in SV becomes `q.PushBack(x)` in emitted
  C++); free-function form (`PushBack(q, x)`) reads as an artificial flattening that obscures the
  receiver / argument distinction in the source.
- `mir.md` invariant 10 forbids a backend re-deriving a fact MIR states. The `.` / `->` choice is
  not a re-derivation: the receiver's MIR type already says pointer or value, and the C++ syntax is
  a mechanical translation of that stated fact (the same kind of translation that turns a MIR
  integer literal into a C++ integer literal). Reading structure is allowed; only scanning a body to
  infer what it must be is forbidden.
- `mir.md` itself names the two backends' spellings explicitly: "a method call rendered as C++
  method-call syntax versus lowered to an LLVM call instruction." The C++ backend takes method- call
  syntax; the LLVM backend's call-instruction form is produced at MIR-to-LIR, not by the C++
  backend. The two paths are independent.
- The "free-function form aligns with LLVM" argument fails first-principles: LLVM-alignment is the
  MIR-to-LIR boundary's job (`lir.md`: "callable invocation becomes a call instruction"). The C++
  backend never feeds LLVM, so it has no LLVM alignment to satisfy. Picking the unidiomatic C++ form
  for an alignment that does not exist trades emit readability for nothing.

### kIsUnknown

The MIR result type of `kIsUnknown` is a 1-bit `PackedArray` (the SV `$isunknown` shape, LRM 20.9 /
21.3.4.3). The runtime exposes `PackedArray::IsUnknown() const -> PackedArray` returning that 1-bit,
2-state value directly; the host-`bool` `HasUnknown()` stays as the internal X-check the operator
implementations use, but is not what the backend names. The render emits `(x).IsUnknown()` uniformly
with every other instance-form builtin -- no host-to-SV bridge step.

### Where an entry's properties live

The shape is the backend's; the properties it reads are not. What the runtime library calls an
entry, whether it declares it as a free function, a method on the object the entry acts on, or a
factory on the type it builds, and what the entry does with the operands it is given are facts about
the library, and one library serves both backends. So they are stated once, beside the identity, and
each backend renders from them.

They were once a table per property, each with its own default arm for the entries it did not list,
and that shape cost what a scattered declaration costs: adding an entry meant editing every table
and nothing said which, the enumerated type methods the namespace no longer holds were refused by
one backend and given a live entry by the other, and five pairs of entries shared one
target-language name and let overload resolution over the argument list stand in for the identity
the pair already carried.

### Adding an entry

Adding a new builtin runtime entry is: one `support::BuiltinFn` enum value; one entry in the
receiver-type name table at AST-to-HIR; one row in the entry declaration, which the build refuses
until it is written and which names every property the entry has; and the library method or function
that row names. No render-side special case for any entry.

## Consequences

- One closed-namespace enum (`support::BuiltinFn`) names every built-in runtime entry, and only
  those. The two layers (HIR, MIR) and every backend reference the same identity, so a layer never
  carries an arm for an operation it cannot meet; an operation the front end answers is named where
  it is answered instead.
- HIR-to-MIR's built-in method translation is near-identity: it passes the `BuiltinFn` id through as
  `Direct::target`; the instance / static / free distinction is read from the id and its signature
  at render, not carried as a Callee arm.
- AST-to-HIR keeps its receiver-type dispatch. The per-receiver name tables return
  `support::BuiltinFn` directly. Adding a new receiver type (e.g. user-defined class methods) is a
  new name table, not a new HIR-level enum or variant arm.
- The backend reads `BuiltinFn` plus receiver MIR type and renders without any family-axis switch.
  Every property of an entry -- what the library calls it, how a call site reaches it, whether it
  updates the object it acts on or hands it back, which operands carry an index, a spread part, a
  closure, or a result prototype -- is one row of one declaration in the support layer, so a
  consumer reads the property it needs and no consumer lists the entries that have it.
- Neither layer carries a per-family variant arm or per-family enum. The per-family scaffolding (one
  `*MethodKind` enum and one `*MethodInfo` wrapper per LRM chapter) is gone from both HIR and MIR;
  the LRM-chapter organization survives only as comment-level grouping inside the flat
  `support::BuiltinFn` enum.

## Cross-references

- `architecture/hir.md` (HIR is SV-faithful; the LRM's chapter organization is documentation, not a
  semantic level of SV).
- `architecture/mir.md` (MIR is a generic programming-language IR; a method call is
  `callee + arguments` with no family axis at MIR level).
- `decisions/runtime-effects-as-generic-calls.md` (the flat-callee pattern this decision extends to
  built-in method calls).
- `decisions/array-method-dispatch.md` (LRM 7.12 array runtime semantics; its earlier per-family
  dispatch shape is superseded here).
- `decisions/callable-receiver.md` (`self`; the receiver mechanics this decision keeps unchanged).
