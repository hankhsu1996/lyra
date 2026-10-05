# Unpacked union is overlapping storage realized as an active-member value (MIR `UnionType`)

Date: 2026-06-27 Status: accepted; the write encoding is superseded by
[value-descent-as-named-calls](value-descent-as-named-calls.md), under which reading a member and
reaching one are the same pair of entries a product component uses, differing only in which value
domain realizes them. The representation this entry settles, an active-member value, is unchanged.
The `Get` / `GetRef` spelling of point 4 is superseded by
[a-part-is-named-by-how-it-is-selected](a-part-is-named-by-how-it-is-selected.md).

Revised 2026-10-05: this entry was derived from the premise that every cross-member read is
undefined, which LRM 7.3 does not say. Its "special provision" defines one: where the members are
unpacked structures sharing a common initial sequence and the union holds one of them, the common
initial part may be read through any of the others and reads what was written. An active-member
value has no such part to read, so point 8 now refuses an inactive-member read on both backends
rather than answering a default, and the overlay rejected below is what the provision needs. Its
reason for rejection was that values are rich objects that cannot share storage, which is a state of
the value layer rather than a condition of the problem, and
[a-value-is-its-machine-data](a-value-is-its-machine-data.md) removes it.

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
- Reading a member other than the one last written is defined only where the members are unpacked
  structures sharing a common initial sequence: the common initial part then reads what was written
  through the live member (LRM 7.3). Any other cross-member read has no meaning the standard gives.
- Default value is the first member's Table 7-1 default.
- Value semantics: a whole-union assignment copies, independent of its source (LRM 7.6, Table 7-1).
- Union members cannot carry per-member initializers (LRM 7.2.2).
- Dynamic and chandle members are illegal in an untagged union (only in a tagged one, LRM 7.3.2).

It is not a product: a product holds all members at once. It is not a sum: a sum is a tagged,
type-checked variant read through tagged expressions and pattern matching (the tagged union, LRM
7.3.2 / 11.9), a distinct concept. An untagged unpacked union is the C `union` -- N views over one
storage, with the same common-initial-sequence guarantee C11 6.5.2.3 gives.

## The axis that decides it: one member at a time

Because a union holds one member at a time, the content of a union value is the pair "(active
member, that member's value)"; a common initial part read through another member is a view of that
same value, not further content. That pair is the union's value-level semantic. A product's value is
the full tuple "(v0, v1, ..., vN)"; the two value spaces are genuinely different, so the union is
not a product with a different label.

## Decision

1. **HIR untagged `UnpackedUnionType` lowers to a new MIR `UnionType`**, whose component types are
   the member types in declaration order. A **tagged** union is a sum type, a distinct
   generic-language concept (an SV-visible tag, type checking, pattern matching, LRM 7.3.2 / 11.9),
   and is its own MIR type rather than `UnionType` with a flag. `UnionType` carries no `tagged`
   field -- per `mir.md` invariant 7 the type is the classification, and there is no second union
   concept hiding behind a flag.

2. **Member access is positional, by declaration-order index**, identical to struct / tuple. The
   index is the carrier of the access, and the type carries no member names: what the language
   renders a value by (LRM 21.2.1.6) is built from the source type before MIR
   ([rendering-a-value-by-its-type](rendering-a-value-by-its-type.md)).

3. **A union value is "(active member index, that member's value)" at the MIR semantic level.** This
   is a semantic statement, not a storage-layout claim; how a backend stores it is a realization
   concern owned by the backend contract.

4. **Member access is a matched read / write access pair, named `Get` / `GetRef` like a struct
   member** (`std::get<I>` is the standard accessor for both the `std::tuple` and the `std::variant`
   that back struct and union). The read is `UnionGetExpr{union, index}` (yielding the active
   member's value; point 8 for any other member); the write is `UnionGetRefExpr{union, index}` (a
   reference to the active member's storage). They are two access forms, not one, because a union
   member's read realization (a value) differs from its write realization (an activating reference)
   -- whereas a struct member's read and write are one `T&`, so a struct collapses to a single
   `Get`. They are distinguished by index at compile time, so each is a node rather than a callee.
   There is no union-specific reference concept: the write form is an ordinary reference into the
   active member's storage.

5. **Member write and compound assignment are uniform with every other lvalue.** `u.f = v`,
   `u.f op= v`, and a nested `u.f.g = v` lower to `AssignExpr` over `UnionGetRefExpr`, exactly as an
   array-element or struct-member write does; there is no whole-union-replacement desugar and no
   read-modify-write at the lowering. The backend renders the write target as a reference to the
   active member's storage (making the member active first if it is not), so a nested member or
   element write composes on it as on a struct member; "evaluate the left-hand side once" (LRM
   11.4.1) is the backend's job on the single target (see
   [compound-assignment-write-location](compound-assignment-write-location.md)). `UnionExpr` builds
   a whole union value (default initialization, point 6); it is not the member-write shape.

6. **Default initialization synthesizes `UnionExpr{0, <member 0's Table 7-1 default>}`** at each
   default-construct site, never stored on the type. As with the struct, the interned `UnionType`
   carries component types only; per-member defaults would leak source-level initialization into
   generic type identity and break canonicalization.

7. **The runtime realization is `lyra::value::Union<Ts...>`, an active-member value** (an active
   index plus that member's value). It is not a byte overlay: the `lyra::value` types are rich C++
   objects -- `PackedArray` carries an X/Z plane (and a heap vector for wide values), `String` owns
   heap storage, `Real` is a `double` -- none can share one storage and be reinterpreted, so a
   C-style overlay is unrealizable while they are. The active-member realization serves every
   program that reads only the member it last wrote. It satisfies `LyraValue` by delegating to the
   active member: `==` / `!=` are "same active index and equal active value", `IsBitIdentical`
   likewise, and `HasUnknown` delegates to the active member; the default holds member 0. There is
   no cross-member equality or conversion.

8. **An inactive-member read is refused as not yet supported**, on both backends. The one such read
   the standard defines, a common initial sequence, needs the live member's bits where the other
   member's view expects them, which an active-member value does not have; answering a default
   instead would be a wrong value there, and refusing keeps the two backends one answer.

## Rejected alternatives

- **Desugar the union to a product (`TupleType` or `StructType`).** A product holds all members; a
  union holds one. They differ in default initialization (all members vs the first) and in write
  semantics (an independent slot write vs replacing the active member). A consumer reading a product
  could not tell them apart, violating `mir.md` invariant 7 (the type is the classification). It
  also contradicts the recorded struct decision ("the union does not share the struct
  representation") and stores every member when only one is live. It would also give a common
  initial part read through another member the default rather than what was written.

- **A `tagged` flag on `UnionType`.** A tagged union is a sum type, a different generic-language
  concept. A flag beside the type that selects between two concepts is the forbidden "classification
  outside the type system" shape. Tagged is rejected at the gate and reserved for a future distinct
  type.

- **A byte-overlay C union at runtime.** The `lyra::value` types are not POD bytes; they cannot
  share storage and be reinterpreted. That is a state of the value layer, not of the problem: an
  overlay laid out from the type is what LRM 7.3's common initial sequence needs, and it becomes
  realizable once a value is its machine data.

## Consequences

- A new MIR type variant (`UnionType`) and two expression primitives: one that builds a whole union
  value by naming its live member, and one that reaches that member.
- A new runtime value type `lyra::value::Union<Ts...>`, composing `LyraValue` from its active
  member.
- A declared union must default-initialize to be usable, so the first-member default lands together
  with member read and write rather than as a separable later step.
- Whole-union copy is the existing whole-cell write (`u2 = u` assigns the cell a copied union
  value), and member-in-expression use is the existing member read; neither needs union-specific
  lowering.

## Cross-references

- `../architecture/mir.md` -- the product beside the value that holds one member at a time;
  invariant 7 (the type is the classification); the forbidden flag-beside-the-type shape.
- [aggregate-names-are-type-content](aggregate-names-are-type-content.md) -- the argument that put a
  union's member names on its type for a time, and why it no longer holds.
- [unpacked-struct-representation](unpacked-struct-representation.md) -- the sibling product
  representation, which left the union open.
- [value-type-concepts](value-type-concepts.md) -- the `LyraValue` lattice the runtime
  `Union<Ts...>` composes, and the observable read / write pattern member access reuses.
- LRM anchors: 7.3 (unions), 7.3.2 (tagged unions), 7.2.2 (member-initializer restriction), 7.6
  (aggregate assignment), Table 7-1 (defaults and nonexistent-entry reads), Table 6-7 (default
  initial values), 11.4.5 (equality on any data type), 6.16.2 (string element write as `putc`, the
  no-element-lvalue precedent).
