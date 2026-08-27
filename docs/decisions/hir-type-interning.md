# HIR type identity is structural and the unit's own, never the frontend's

Date: 2026-08-26 Status: accepted

## Context

A unit's HIR type pool was an append-only arena. Nothing about it was canonical: the same type could
occupy several entries, and what kept the duplication small was a memo on the side, keyed by the
frontend's canonical type pointer. Two spellings of one type landed on one entry when, and only
when, the frontend had already merged them -- which it does for integral vectors and does not for a
struct, an enum, or a typedef, each of which gets its own symbol per declaration.

Two consumers turned that from untidy into wrong.

**A type published by one unit and read by another.** A unit's signature states the type of what it
publishes, and a consuming unit takes that type into its own pool. Nothing in the transfer can carry
the producer's identity, so what crosses is structure. With no canonical identity on the receiving
side, a type arriving from a signature becomes a second entry beside the identical one the consuming
unit already held, and a third when a second child publishes the same port type. The consuming unit
then holds several ids for one type, and which one a construct carries depends on the route it
arrived by.

**Anything that outlives the frontend.** The memo's key is a pointer into the frontend's elaborated
graph. That graph is one table shared by the whole design and it dies with the frontend object. An
identity resting on it therefore cannot be the identity a cached unit artifact is read back with,
and cannot be established without every other unit present -- which is what independent and
incremental compilation forbid (`architecture/north_star.md`).

MIR settled the same question for its own pool in [mir-type-interning](mir-type-interning.md),
driven by a different consumer: a class that references itself needs its forward reference and its
definition to share one `TypeId`.

## Decision

A unit's HIR type pool is a **structural-equality interner**. Constructing a type answers with the
canonical identity of that structure in that unit; a second request for the same structure answers
with the same identity, whichever route asked.

1. **Identity is the pool's, not the frontend's.** The frontend-keyed memo remains, and is only a
   shortcut past the translation work for a type already translated. Whether a spelling took the
   shortcut cannot change which identity it reaches.

2. **The key is the type's structure, including the identities its structure names.** A class handle
   is keyed by the class reference it names -- a declaration identity, local or by declaring unit
   and name -- never by the class's members, since two classes with identical members are different
   types. No other HIR type carries a nominal identity to lose: HIR records no source name for a
   struct, a union, or an enum, so structure is all there is to key on, and folding two entries that
   agree on it merges nothing a consumer could have told apart.

3. **A type crossing a unit boundary is interned on arrival.** Import walks the published subgraph
   and interns each node, so a type the consuming unit already holds is not copied. This is what
   makes the boundary transfer idempotent, and it is the property the whole signature workstream
   rests on.

4. **`TypeId` equality means type equality within one unit, and nothing across units.** Two units
   compiled in one build give the same type different ids; identity across a boundary is the
   structure, transferred, never the number.

## Rejected

- **One type pool for the whole design.** Removes the transfer entirely, and costs both properties
  the pool exists to protect: every unit's lowering would write one shared table, so no two units
  lower in parallel, and the numbering would depend on every unit in the design, so a change
  anywhere invalidates every cached unit.

- **Keep the frontend's canonical pointer as the identity.** It is not structural except for
  integral vectors, it is design-global, and it does not survive the frontend object. Legitimate as
  a per-unit memo inside one lowering run, which is what it now is.

- **Types as values rather than identities.** Copying a type tree at every use makes the boundary
  transfer free and gives up sharing and cheap comparison everywhere else, at every layer that has
  since been built on identity.

- **Accept the duplicates.** Nothing observable breaks today, which is exactly why it would have
  been left standing: the property "structurally equal is the same type" would be false at the one
  layer that publishes types across a boundary, and every later consumer would have to know not to
  rely on it.

## Consequences

- A type published by one unit and read by another lands on the entry the reader already had.
- Each `TypeData` variant states which of its fields determine identity, by the same key rules MIR's
  pool follows.
- The pool holds one extra copy of each type's data as its key. The types one unit names number in
  the hundreds, so this is not on any axis that matters; a heavier key would be.
- The interner is unit-local, mutable during that unit's lowering only. It is not shared between
  units, which is what keeps unit lowerings independent.

## Cross-references

- [mir-type-interning](mir-type-interning.md) -- the same shape one layer down, decided for the
  recursive-class consumer.
- [unit-signature](unit-signature.md) -- what a unit publishes, and why a published fact must mean
  something without the producer's storage.
- [arena-reference-lifetime](arena-reference-lifetime.md) -- the storage primitive beneath the
  interner, whose `Get` / `Add` contract is unchanged.
- `architecture/identity_and_ownership.md` -- unit-local typed identity; nominal identity follows
  declaration, not structure.
