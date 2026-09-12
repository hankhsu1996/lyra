# A declared aggregate's member names are content of its type

Date: 2026-09-10 Status: accepted

## Why this decision matters

[unpacked-struct-representation](unpacked-struct-representation.md) settled that an SV unpacked
structure is a value product and lowers to MIR's `TupleType`, and said of the names:

> **Member access is positional, by declaration-order index.** Field names are dropped at
> HIR-to-MIR, exactly as a packed struct drops its fields to bit offsets. The index is the carrier
> [...] any source / debug presentation that needs them (`%p`, `$typename`) reads them from separate
> source-schema metadata, never from a field on `TupleType`.

[closure-environment-and-activation-frame](closure-environment-and-activation-frame.md) restated it
in its consequences: "an unpacked struct of the source language is a `TupleType`, and composing one
is the structural product's literal, not a nominal one." The packed family went further -- the type
catalogue recorded that a packed structure and union have no MIR shape of their own at all, only the
single vector their members project onto.

That leaves no layer below the front end holding a member's name. This entry reopens the naming half
of those decisions, because a requirement of the language turns on it.

## What falsified it

**LRM 21.2.1.6 renders a value by the names its type declares.** "For unpacked structure data types,
it shall print the value as an assignment pattern with **named elements**"; a packed structure
prints the same way; a union prints only its first declared element; a tagged union prints
`tag:value`; and an enumeration prints the name its type declares for the value. The clause's own
example prints `'{sw:OFF, s:"switch10"}`.

So the names are not presentation of the compiler's output. They are observable behaviour of the
simulated program, and `%p` is the operation that reads them.

**The escape hatch does not hold, and the reason is the interning rule.** Side metadata has to be
keyed by something. `TupleType` is shape-interned, so two structures that differ only in their
member names are one `TypeId` -- there is no key to hang the names on. Keyed per print site instead,
the names would be re-derived at every use of a fact that belongs to the type, and two values of one
MIR type would render differently. That is the direct negation of `mir.md`'s rule that the type is
the classification.

**The formatter reaches elements the compiler cannot enumerate.** `$display("%p", q)` over a queue
of structures formats each element at run time, so no text composed at the lowering can reach them:
the count is not known until the value exists. Whatever carries the names has to travel with the
type into the runtime, which settles that they are type content rather than call-site content.

## The axis that decides it: nominal versus structural, which is separate from value versus reference

The superseded decision chose between `TupleType` and `ObjectType` on the value-versus-reference
axis, and chose correctly: an unpacked structure is a value (LRM 7.2.2, Table 7-1), not a managed
reference. That axis says nothing about whether the type is nominal.

LRM 6.22.2(b) answers the other axis outright: "An anonymous `enum`, unpacked `struct`, or unpacked
`union` type is equivalent to itself among data objects declared within the same declaration
statement and no other data types." A declared aggregate is nominal. A shape-interned product states
the opposite.

The field agrees, and separates the two halves the same way. Clang's AST carries a `RecordType` over
named `FieldDecl`s. LLVM IR distinguishes an _identified_ struct type, uniqued by name, from a
_literal_ one uniqued by shape, and a C structure becomes identified -- while field access is a
`getelementptr` at a numeric index. Rust's `struct` is a nominal `AdtDef` and its tuple is
structural, yet Rust MIR projects a field of either by `FieldIdx`. GIMPLE's `COMPONENT_REF` names a
`FIELD_DECL`; SIL's `struct_extract` names a `VarDecl`.

The consistent shape is: **the type's identity is nominal, and reaching a member stays positional.**

## The project already recorded the general principle

[enum-representation](enum-representation.md) states it, for the same problem one type over:

> **A semantic type may retain nominal identity through MIR/LIR without owning a distinct runtime
> value representation.** This is a general compiler principle; the enum is the current important
> example.

An enumeration keeps a MIR type carrying its member table while every value operation runs on its
base integral, and the table is read by the LRM 6.19.5 methods and by `%p`. A declared aggregate is
the same shape with a different projection.

## Decision

**An aggregate the source declared is a MIR and LIR type that names its members, and its
value-domain projection is the representation it shares with a type that names nothing.**

1. **Five source aggregates keep a type of their own.** An unpacked structure, an untagged unpacked
   union, a tagged unpacked union, a packed structure, and a packed union each carry their members
   in declaration order, each member a name and a type and nothing else. Where a packed member
   physically sits follows from the kind of aggregate and the widths before it, so it is derived
   where it is needed; carrying it on the type would be derived data in the interning key, which
   [mir-type-interning](mir-type-interning.md) rules out, and would be a second statement of a
   placement the LRM already fixes.

2. **`TupleType` is the anonymous product a lowering composes for itself** -- an associative entry's
   `(key, value)` pair, a task's output pack, a scan target list -- and is never what a declared
   aggregate lowers to. It remains the one _structural_ heterogeneous product, which is what
   `mir.md` was saying; what stops being true is that it is the only heterogeneous aggregate.

3. **The projection is what every value operation reads.** An unpacked aggregate projects to the
   product: one realization, `lyra::value::Tuple<Ts...>` monomorphized or `RuntimeTuple` erased, and
   the value representation is unchanged by this entry. A packed aggregate projects to the single
   vector its members are placed in, reached through the same `IsIntegralPacked()` / `PackedShape()`
   pair an enumeration already answers, so it is integral everywhere an integral is asked for.

4. **Reaching a member stays positional.** A member is named by its declaration-order position, the
   selector a product component and a union member already share; no access node gains a name. What
   the names are for is rendering.

   The packed union is one type for the tagged and the untagged form, where the unpacked family
   keeps two. That is not an inconsistency: a packed tagged union's tag is bits of the same vector,
   so no operation below the front end reaches its members except through that vector, and nothing
   at this layer has to tell the two apart. Where something does -- LRM 21.2.1.6 prints them
   differently -- the answer is a second type, never a flag on this one, for the reason
   [unpacked-union-representation](unpacked-union-representation.md) gives for the unpacked pair.

5. **Member names are part of the interning key; a declaration identity is not added.** Two
   declarations with the same member names and types share one `TypeId`, which is sound because
   every use that could tell them apart is type-checked in the front end and nothing below renders
   them differently. Per [mir-type-interning](mir-type-interning.md)'s own disposition for the enum,
   an explicit declaration id is added when a consumer needs an identity the members do not already
   carry, not speculatively.

6. **A member's declaration initializer stays off the type.** LRM 7.2.2 lets a member carry its own
   initializer, which takes precedence over the Table 7-1 default; it is a value the source states,
   composed at each site that default-constructs, exactly as a class's property initializers run in
   its constructor. This is unchanged from the superseded entry, and is why point 5 is sound in the
   other direction too: nothing observable hangs on telling two content-equal declarations apart.

## Rejected alternatives

- **Side metadata keyed by the MIR type.** There is no key: the product is shape-interned, so the
  two structures whose names differ are one type. This is the superseded entry's own escape hatch,
  and it is what a reader will re-propose.

- **The names as an operand of each print site, composed at the lowering.** Works only where the
  shape is static, so a container of structures -- whose element count exists only at run time --
  cannot be reached by it. It also re-derives at every use a fact that belongs to the type, and lets
  two values of one MIR type render differently. An enumeration written as the whole operand is
  rendered this way today, and works only there, which is the same limit seen from one type over.

- **A distinct runtime value type per declared aggregate, carrying its names.** Reverses point 3 for
  no gain the projection does not already give, and the execution backend cannot hold it: that path
  erases a product to one runtime type, so per-source-type names cannot live in the value there.
  Carrying the names to the formatter is a separate mechanism, and it belongs to the print operation
  rather than to the value.

- **Reusing `StructType`, the nominal named aggregate MIR already has.** That category is the
  compiler-generated promoted scope: storage with run-time identity, reached through a `Shared<>`
  wrapper and realized as a generated record. A declared aggregate is a value that is copied whole
  and has a value domain. Fusing them needs a discriminator every consumer then re-reads, which is
  the shape [closure-environment-and-activation-frame](closure-environment-and-activation-frame.md)
  rejected when it split the closure back out of that same type.

## Consequences

- Every site that asked "is this integral" by testing for the plain vector answers for a packed
  aggregate again only because it now asks `IsIntegralPacked()`. Seven such sites existed --
  `$sscanf` / `$fscanf` sources, `$readmem` index and element types, a sampled value's bit-select
  type, a part-select's natural type, and the owned-value wrap -- and each was a latent wrong answer
  the moment a packed aggregate stopped being spelled as a vector.
- Two questions are asked of the type rather than by matching alternatives: whether a type is a
  product, and whether it is a union. A consumer that matched one alternative to mean the concept
  now names the concept.
- A cast between an integral type and the type it shares a representation with is stated by the
  types differing, not by either one being an enumeration.
- The names now exist below the front end, which was the precondition for `%p`. Carrying them into
  the print operation was the step that remained, and it is
  [rendering-a-value-by-its-type](rendering-a-value-by-its-type.md): a callable synthesized per
  type. That entry also answers point 4's open half without the second type it anticipated -- the
  rendering is keyed by the SystemVerilog type, where a packed tagged union is still told from an
  untagged one, so nothing below the front end has to tell them apart.

## Cross-references

- `../architecture/mir.md` -- the declared aggregate beside the anonymous product, and the
  value-domain projection.
- [unpacked-struct-representation](unpacked-struct-representation.md) -- the value-versus-reference
  half, unchanged, and the naming half this entry supersedes.
- [unpacked-union-representation](unpacked-union-representation.md) -- the active-member value,
  whose member names this entry restores.
- [enum-representation](enum-representation.md) -- the general principle this applies: nominal
  identity without a distinct runtime value representation.
- [mir-type-interning](mir-type-interning.md) -- what a type's key encodes, and when a declaration
  identity is added.
- LRM anchors: 6.22.2 (equivalent types), 7.2 / 7.2.1 (structures), 7.3 / 7.3.1 / 7.3.2 (unions),
  21.2.1.6 (assignment pattern format).
