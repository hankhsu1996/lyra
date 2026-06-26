# MIR type pool is a structural-equality interner

Date: 2026-06-26 Status: accepted

## Context

MIR's unit-wide type pool is a plain append-only `Arena<Type, TypeId>`: every construction appends
and mints a fresh `TypeId`, with no deduplication. So the same semantic type -- `Coroutine<void>`, a
borrowed pointer to the scope type, `Ref<T>` -- is interned to multiple distinct `TypeId`s. The
hottest few are hand-cached as fields on a builtin table to avoid the duplication, and the
coroutine-type constructor special-cases a `void` payload to return the cached instance. This
conflates three distinct concepts: an **arena** (append -> new local id), an **interner** (a
semantic key -> one canonical id), and a **builtin registry** (named handles for atomic types).

A committed downstream consumer forces the question. A class is a data type: its MIR type is an
object type naming the class's declaration. A class can reference itself (a member that is a pointer
to its own type) or participate in a cycle with another class (mutual reference). Lowering such a
class is the MIR analogue of a C++ forward declaration: a reference to the class's type is formed
_before_ the class body exists, and it must denote the **same** type as the class's own definition
uses. A plain arena cannot provide this -- each `Add(ObjectType{...})` yields a different `TypeId`,
so a forward reference and the definition would carry unequal type identities and not be recognized
as the same type. Only canonicalization -- interning an object type by the class's declaration
identity, which exists before the body -- gives the forward reference and the definition one shared
`TypeId`.

The same property makes `TypeId` equality a valid, cheap semantic-equality test (`if (a == b)` means
"the same MIR type"), which a non-canonicalizing arena cannot offer.

The hard requirement is the nominal half of this canonicalization, and it bites on object types: a
class's type must share one `TypeId` across its references and its definition -- what the
recursive-class consumer needs -- and a class needs an explicit declaration identity because two
classes with identical members are distinct types. Interning every other constructed type -- the
structural types, and enum types whose enumerators already form a scope-unique key (see the
Decision) -- is the consistent extension of the same mechanism: not separately forced by a consumer,
but it removes the manual builtin caches for hot composites and makes `TypeId` equality uniform, at
the marginal cost of keying a few more variants through the same path.

## Decision

The MIR type pool is a **type interner**: arena storage for the canonical representatives plus a
structural-key index that maps each semantic type to one canonical `TypeId`.

1. **Interning is the type-construction API.** Constructing a type returns the canonical `TypeId`
   for its semantic identity; two requests for the same semantic type return the same `TypeId`. Raw
   append (mint a new id unconditionally) is not how a type is obtained.

2. **The key encodes MIR semantic equality, not layout.** A nominal type is keyed by its declaration
   identity wherever a structural key could otherwise merge it with a genuinely different type:
   - An **object type** (a class) is keyed by its **class id**. Two classes with identical members
     are different types and may coexist in one scope, so a structural expansion of the members is
     not a valid identity -- only the declaration id is.
   - An **enum type** is keyed by its base and its enumerators (names and values). SystemVerilog
     inserts enumerator names into the enclosing scope and forbids two enumerations in one scope
     from sharing a name (LRM 6.19), so an enumerator set is a scope-unique fingerprint: it _is_ the
     enum's declaration identity, and keying on it cannot merge two distinct enums. An explicit
     enum-definition id would be the same identity by a different key; it is added only if a
     consumer needs an enum identity the enumerators do not already carry (such as the source type
     name), not speculatively.
   - **Structural types** -- pointer, reference, vector, coroutine, packed/unpacked array, tuple --
     are keyed by the constructor plus the canonical `TypeId`s of their children plus their semantic
     modifiers (ownership, dimensions, signedness, const-ness).

3. **The key excludes non-semantic fields.** Source locations, debug/display names, and derived or
   cached data do not participate. Two types equal in semantics but differing only in such a field
   canonicalize to one `TypeId`.

4. **`TypeId` is the unit-local canonical handle.** Within one compilation unit, `TypeId` equality
   implies semantic-type equality, and the converse holds by construction. `TypeId` is not a
   cross-revision or incremental key: it depends on construction order within a build, so the same
   semantic type may carry a different `TypeId` in the next build. Cross-revision identity is a
   separate semantic key / fingerprint, computed on demand, not the `TypeId`.

5. **Object identity breaks recursion.** A self- or mutually-referential type reaches the cycle only
   through an object type, which is keyed by a class id that exists before the body is built.
   Interning an object type therefore needs no completed body, and no anonymous structural type
   forms a cycle. This is what makes the forward-declaration use case work.

6. **Builtins hold only atomic canonical types.** The builtin table names language and runtime
   atomic types (void, the literals, string, time, ...). A composite type that is materialized
   pervasively -- `Coroutine<void>`, the borrowed scope pointer -- is obtained through interning
   like any other composite; a retained named handle for it is a convenience alias produced via the
   same interning path, never a separate allocation and never a deduplication mechanism. The
   coroutine constructor has no `void`-payload special case.

## Rejected

- **Plain append-only arena plus manual builtin caches (status quo).** Cannot canonicalize, so a
  forward-declared or recursive class cannot share one `TypeId` between its references and its
  definition -- the committed consumer is unserviceable. Duplication of every other synthesized type
  persists, and the builtin table accretes composite caches that misrepresent instantiations as
  primitives.

- **Naive structural deduplication for object types (fold classes whose members match).** Two
  classes with identical members are distinct types, so folding them by structure corrupts type
  identity; an object type must be keyed by its class id. This hazard is specific to types whose
  structure can collide with a different type's. An enum cannot: LRM 6.19 makes its enumerator set
  scope-unique, so keying an enum by its enumerators is its declaration identity, not naive
  structural dedup.

- **Keep the arena, document "do not compare `TypeId`s for semantic equality."** Sufficient if the
  only goal were to avoid a latent trap, but it cannot serve the class consumer: a documented
  prohibition does not give a forward reference and a definition a shared type identity. Once a
  consumer needs canonical type identity, the pool must canonicalize.

## Consequences

- The same semantic type maps to one `TypeId`; `if (a == b)` is a valid, cheap semantic-equality
  fast path, available to type comparisons, dispatch, and future type-keyed caches.
- A forward reference to a class type and the class's definition resolve to the same `TypeId`,
  enabling self- and mutually-recursive class types.
- The coroutine constructor's `void` branch and the composite builtin caches (`Coroutine<void>`, the
  borrowed scope pointer) are removed or become convenience aliases produced through interning.
- Each `TypeData` variant must state which of its fields determine semantic identity. This is a
  per-variant audit governed by the key rules above (object types by class id, enum types by their
  scope-unique enumerators, structural types by constructor plus canonical children plus modifiers,
  non-semantic fields excluded).
- The interner is unit-global, single-writer-during-construction, mutable state. Under future
  body-level parallel lowering it is a shared-state boundary; its concurrency model is decided
  together with the body-level ownership model, not assumed lock-free.
- The arena remains the storage primitive beneath the interner; the interner adds the key index. The
  arena's reference-lifetime contract is unchanged: a lookup returns a transient view, the canonical
  `TypeId` is the durable handle.

## Cross-references

- [generic-lowering-machinery](generic-lowering-machinery.md) (the arena versus interning-table
  distinction; this realizes the interning-table shape for the type pool).
- [arena-reference-lifetime](arena-reference-lifetime.md) (the underlying arena's `Get`/`Add`
  contract).
- `architecture/identity_and_ownership.md` (typed compilation-unit-local identity; nominal identity
  follows declaration, not structure).
- `architecture/incremental_build.md` (`TypeId` is not a cross-revision or cache key;
  ownership-based keys are).
- `architecture/mir.md` (an object type names a class of the unit; nominal type identity).
