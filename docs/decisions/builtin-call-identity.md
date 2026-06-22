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

HIR's callee for a built-in method is `BuiltinMethodRef` carrying `support::BuiltinFn`. The HIR also
keeps an `IteratorMethodKind` sibling arm for `item.index` (LRM 7.12.4), which HIR-to-MIR rewrites
to a `LocalRef` on the closure's index binding and which never reaches MIR's vocabulary.

MIR's callee is one of `BuiltinFnCallee{id: BuiltinFn}` (instance call: receiver is `args[0]`) or
`BuiltinStaticCallee{id: BuiltinFn, type_qual: TypeId}` (type-namespace-qualified static call: no
receiver, the qualifier rides on the callee). The instance / static split is structural at MIR
because the calling convention differs; static-vs-instance is recoverable from the `BuiltinFn` id
itself, but pushing it into a separate variant arm spares every backend from re-deriving the
distinction at render time.

HIR-to-MIR is a near-identity translation: pick the right MIR arm based on the id's
static-vs-instance classification and pass the `BuiltinFn` through.

AST-to-HIR still dispatches by receiver type to choose which name-to-id lookup table to query --
`first` on an enum receiver resolves to `kEnumFirst`, `first` on an associative array resolves to
`kAssocFirst`. The receiver-type dispatch is unaffected by the identifier shape; only the lookup
table's return type changes.

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

- **Folding `IteratorMethodKind::kIndex` into the flat enum.** `item.index` is HIR-only: it is
  rewritten at HIR-to-MIR into a `LocalRef` on the array-method closure's index binding (LRM 7.12.4
  binding rule) and never appears in MIR. Putting an HIR-only entry in a shared support enum would
  mean MIR carries a name for a value it cannot represent, so the iterator kind stays as a separate
  variant arm in `hir::BuiltinMethodRef`.

## Backend rendering

The identity decision (flat `BuiltinFn`) is orthogonal to how each backend spells the call in its
target. Two shapes were considered for the C++ backend:

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

### Adding an entry

Adding a new builtin runtime entry is: one `support::BuiltinFn` enum value; one entry in the
receiver-type name table at AST-to-HIR; one C++ method on the receiver type whose name matches the
backend's `BuiltinFnMemberName` table; and a `Mutating` / `ContainerAccess` predicate entry if
applicable. No render-side special case for any entry.

## Consequences

- One closed-namespace enum (`support::BuiltinFn`) names every built-in runtime entry. The two
  layers (HIR, MIR) and every backend reference the same identity.
- HIR-to-MIR's built-in method translation is near-identity: it inspects the id to decide
  `BuiltinFnCallee` vs `BuiltinStaticCallee` and passes the id through.
- AST-to-HIR keeps its receiver-type dispatch. The per-receiver name tables return
  `support::BuiltinFn` directly. Adding a new receiver type (e.g. user-defined class methods) is a
  new name table, not a new HIR-level enum or variant arm.
- The backend reads `BuiltinFn` plus receiver MIR type and renders without any family-axis switch.
  `IsMutatingBuiltinFn` and `IsContainerAccessFn` are predicates on the flat enum.
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
- `decisions/array-method-dispatch.md` (LRM 7.12 family dispatch; its "Subsequent revision" note
  records this transition for the array family).
- `decisions/callable-receiver.md` (`self`; the receiver mechanics this decision keeps unchanged).
- `progress/refactor.md` R29 (tracking entry for the work that lands this shape).
