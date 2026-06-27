# Unpacked union is overlapping storage realized as an active-member value (MIR `UnionType`)

Date: 2026-06-27 Status: accepted

## Why this decision matters

HIR carries `UnpackedUnionType` (LRM 7.3) as a faithful source construct. HIR-to-MIR must give it a
generic programming-language representation or reject it. The sibling decision
[unpacked-struct-representation](unpacked-struct-representation.md) settled the unpacked struct (a
value product, MIR `TupleType`) and explicitly left the union open: "an untagged unpacked union (LRM
7.3) is overlapping storage, neither a product nor a sum ... this decision does not anticipate their
shape." The `mir/type.hpp` type catalogue likewise records the union as "a separate representation
problem." This entry settles it.

## What an untagged unpacked union is (LRM 7.3)

- A single piece of storage accessed through one of the named member types; only one member is
  usable at a time.
- No required representation for how members are stored.
- Reading a member other than the one last written is a type loophole whose result is undefined (LRM
  7.3). This is not relied upon.
- Default value is the first member's Table 7-1 default.
- Value semantics: a whole-union assignment copies, independent of its source (LRM 7.6, Table 7-1).
- Union members cannot carry per-member initializers (LRM 7.2.2).
- Dynamic and chandle members are illegal in an untagged union (only in a tagged one, LRM 7.3.2).

It is not a product: a product holds all members at once. It is not a sum: a sum is a tagged,
type-checked variant read through tagged expressions and pattern matching (the tagged union, LRM
7.3.2 / 11.9), a distinct concept. An untagged unpacked union is the C `union` -- N views over one
storage, type-punned, with cross-view reads undefined.

## The axis that decides it: one member at a time, cross-reads undefined

Because a union holds one member at a time and a cross-member read is undefined, the well-defined
content of a union value is exactly the pair "(active member, that member's value)". Nothing more is
observable to a conformant program. That pair is the union's value-level semantic. A product's value
is the full tuple "(v0, v1, ..., vN)"; the two value spaces are genuinely different, so the union is
not a product with a different label.

## Decision

1. **HIR untagged `UnpackedUnionType` lowers to a new MIR `UnionType`**, whose component types are
   the member types in declaration order. A **tagged** union is rejected at the HIR-to-MIR gate: a
   tagged union is a sum type, a distinct generic-language concept (an SV-visible tag, type
   checking, pattern matching, LRM 7.3.2 / 11.9) that will be its own future MIR type, not
   `UnionType` with a flag. `UnionType` carries no `tagged` field -- per `mir.md` invariant 7 the
   type is the classification, and there is no second union concept hiding behind a flag.

2. **Member access is positional, by declaration-order index**, identical to struct / tuple. Field
   names are dropped at HIR-to-MIR; the index is the carrier.

3. **A union value is "(active member index, that member's value)" at the MIR semantic level.** This
   is a semantic statement, not a storage-layout claim; how a backend stores it is a realization
   concern owned by the backend contract.

4. **Member read is an access primitive `UnionGetExpr{union, index}`**, parallel to `TupleGetExpr`.
   It is read-only: it never appears as an lvalue or a writable place.

5. **Member write is whole-union replacement.** `u.f = v` lowers to assigning the union _place_ a
   freshly built single-member union value: `UnionExpr{index, value}`. The member access never
   survives as a writable place; the write targets the union one level up. This is the same shape as
   a string element write, which has no element lvalue and realizes as `putc` against the string
   place (LRM 6.16.2). A compound `u.f op= v` reads the active member through `UnionGetExpr`,
   applies the operator, and rebuilds the union -- the read side uses `UnionGetExpr`, the write side
   uses `UnionExpr`, and the two are never conflated. Taking a place or a `ref` / `output` binding
   to a union member is rejected for now.

6. **Default initialization synthesizes `UnionExpr{0, <member 0's Table 7-1 default>}`** at each
   default-construct site, never stored on the type. As with the struct, the interned `UnionType`
   carries component types only; per-member defaults would leak source-level initialization into
   generic type identity and break canonicalization.

7. **The runtime realization is `lyra::value::Union<Ts...>`, an active-member value** (an active
   index plus that member's value). It is not a byte overlay: the `lyra::value` types are rich C++
   objects -- `PackedArray` carries an X/Z plane (and a heap vector for wide values), `String` owns
   heap storage, `Real` is a `double` -- none can share one storage and be reinterpreted, so a
   C-style overlay is unrealizable. The active-member realization is sufficient because cross-member
   reads are undefined. It satisfies `LyraValue` by delegating to the active member: `==` / `!=` are
   "same active index and equal active value", `IsBitIdentical` likewise, and `HasUnknown` delegates
   to the active member; the default holds member 0. There is no cross-member equality or
   conversion.

8. **An inactive-member read returns that member's Table 6-7 default.** This is a deterministic Lyra
   fallback for an operation SystemVerilog leaves undefined, not a guarantee. SV gives no reliable
   semantics to a cross-member read, so compiler optimization, tests, and user programs must not
   depend on the returned value.

## Rejected alternatives

- **Desugar the union to `TupleType` (reuse the struct representation).** A product holds all
  members; a union holds one. They differ in default initialization (all members vs the first) and
  in write semantics (an independent slot write vs replacing the active member). A consumer reading
  `TupleType` could not tell them apart, violating `mir.md` invariant 7 (the type is the
  classification). It also contradicts the recorded struct decision ("the union does not share the
  struct representation") and stores every member when only one is live. The observable contract for
  conformant programs happens to coincide -- because cross-reads are undefined -- but the type
  identity and the semantics differ, and conflating them is the inverse of the
  type-is-classification invariant.

- **A `tagged` flag on `UnionType`.** A tagged union is a sum type, a different generic-language
  concept. A flag beside the type that selects between two concepts is the forbidden "classification
  outside the type system" shape. Tagged is rejected at the gate and reserved for a future distinct
  type.

- **A byte-overlay C union at runtime.** The `lyra::value` types are not POD bytes; they cannot
  share storage and be reinterpreted. The active-member realization is the only workable one and is
  sufficient under undefined cross-reads.

## Consequences

- A new MIR type variant (`UnionType`) and two expression primitives (`UnionExpr`, `UnionGetExpr`),
  parallel to the tuple pair.
- A new runtime value type `lyra::value::Union<Ts...>`, composing `LyraValue` from its active
  member.
- `struct-union.md` N1-N3 build on this. A declared union must default-initialize to run, so the
  first-member default is part of the first cut rather than a separable later one.
- Whole-union copy is the existing whole-cell write (`u2 = u` assigns the cell a copied union
  value), and member-in-expression use is the existing member read; neither needs union-specific
  lowering.

## Cross-references

- `../architecture/mir.md` -- `TupleType` as the single product type; invariant 7 (the type is the
  classification); the forbidden flag-beside-the-type shape.
- [unpacked-struct-representation](unpacked-struct-representation.md) -- the sibling product
  representation, which left the union open.
- [value-type-concepts](value-type-concepts.md) -- the `LyraValue` lattice the runtime
  `Union<Ts...>` composes, and the observable read / write pattern member access reuses.
- LRM anchors: 7.3 (unions), 7.3.2 (tagged unions), 7.2.2 (member-initializer restriction), 7.6
  (aggregate assignment), Table 7-1 (defaults and nonexistent-entry reads), Table 6-7 (default
  initial values), 11.4.5 (equality on any data type), 6.16.2 (string element write as `putc`, the
  no-element-lvalue precedent).
