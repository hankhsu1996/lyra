# Identity and Ownership

Three names the compiler writes fall short of invariant 7. A class the lowering builds for its own
use is named by its position among the classes its unit builds so, a closure by the position its
body's LIR counted it at, and a unit's body by the order its lowerings come out in
([`specialization_model.md`](specialization_model.md) invariant 7), so adding one early renames
every later one of its kind. The first two reach no other unit; a body's name reaches emitted file
names and the design root's tables. None follows the construct that made it, as the invariant
requires. And three containers AST-to-HIR keys on front-end pointers are iterated, against invariant
10: the class bodies still pending when every scope is lowered, read to report the ones left over;
the specializations placed under each generic, each list then put in order of its names; and the
units grouping has met, then put in order of their hierarchical paths. None lets the allocator's
order reach what is compiled, the first only reaching a report of a compiler fault.

## Purpose

Define the rules for identity and ownership across the compiler. Identity is how one entity
references another; ownership is which entity holds a given piece of state. The requirements behind
both are the ones [`north_star.md`](north_star.md) invariants 4 and 5 name: a unit compiles alone
and in parallel, so no identity may need a table outside its scope, and a result is reused across
builds, so nothing that outlives one lowering may shift when an unrelated entity is added.

## Owns

- The rule that each compilation-unit-local entity has a typed, compilation-unit-local id, resolved
  by the pool that owns it.
- The rule that an entity another unit defines is named inside this unit by this unit's own
  declaration of it, and across units by a stable key, never by an identity of another unit's
  numbering.
- The rule that architecture-bearing state is owned by the entity it belongs to, not recovered from
  a side table.
- The rule that an identity is never a rendering, and how a declaration of a unit is identified from
  outside the unit's pools.

## Does Not Own

- The specific id kinds per IR layer (see [`hir.md`](hir.md), [`mir.md`](mir.md)).
- The hierarchy and generate ownership model (see
  [`hierarchy_and_generate.md`](hierarchy_and_generate.md)).
- How a cross-unit reference is resolved to a concrete target (see
  [`reference_resolution.md`](reference_resolution.md)).
- How a unit and a specialization are named (see
  [`specialization_model.md`](specialization_model.md)).

## Core Invariants

1. Every compilation-unit-local declaration carries a typed id. References inside the unit use that
   typed id.
2. An entity another unit defines is named inside this unit by this unit's own declaration of it --
   an entry of the same table its own entities are entries of, saying which unit defines it -- and
   never by an identity of the other unit's numbering; across units it is known by a stable key, the
   declaring unit and the published name ([`object_model.md`](object_model.md) invariant 2,
   [`callable.md`](callable.md) invariant 11). A unit then depends on what another published and on
   nothing the other numbered, which is what lets each compile alone
   ([`north_star.md`](north_star.md) invariant 5).
3. Ownership is direct. An entity that logically owns another holds it as a typed field or member,
   not as an entry in a global map.
4. An identity is not a rendering. A name, a path string or a coordinate is how an identity is
   spelled for a reader or a target, and a spelling may drop whatever its reader recovers by other
   means, while an identity may drop nothing. So what must distinguish -- a class of another unit,
   an argument a specialization was bound to, a declaration of a unit -- is held as its parts and
   compared as them, and composing a name from it is the last step before the name is written, done
   by whoever knows the target's spelling rules.

   A program-wide symbol is that last step, taken in one place: a category saying which kind of
   declaration it names, then parts that each carry their own extent and say whether they are an
   identifier the source wrote or a position counted where the source wrote none. A SystemVerilog
   identifier admits every printable character (LRM 5.6.1), so no character is free to separate
   parts and no word is free to mean something the source did not write; with self-delimiting parts
   a declaration the source named never composes the symbol of one it did not, and the unit emitting
   a declaration and every unit reaching it compose the same string from the names each already
   holds, with no table between them. A type enters a symbol as what it is -- its kind, what fixes
   it and the types it is made of -- and a declaration it names enters as that declaration's own
   symbol, so every unit naming one type composes one part for it.

5. Side tables exist only as caches of directly owned state. A cache never becomes the authoritative
   source.
6. Identity is sufficient for resolution within its scope. A typed id is resolved by the pool that
   owns its entity and by nothing else; if resolution needs a second table beside that pool, the
   identity is incomplete. A unit's one table of classes, or of callable declarations, each entry
   saying who declares it, is that pool, as a Rust `DefId` is resolved by the crate's definitions.
7. What outlives one lowering follows ownership, never traversal order. A symbol, a cache key, a
   fingerprint and a name another unit computes are spelled from names and from the position an
   owner gives its own children -- the parent and a disambiguator among same-named siblings, as
   rustc's `DefKey` states -- so inserting, removing, or reordering one entity does not shift what
   identifies an unrelated one. A pool's sequential id is a handle inside the lowering that minted
   it, and never reaches any of those.
8. Whether an identity may precede its content is a property of the entity, and the pool that holds
   it states the answer. An entity something can reference before its content exists -- a
   declaration a peer may name, whatever order the two are reached in -- is held in a pool that
   mints the identity separately from filling it, and what identifies it beyond the pool -- a
   class's declaring unit and path -- is held with the identity from the moment it is minted, so
   asking it never waits for the content. An entity built and consumed within one walk, which
   nothing names before it exists, is held in a pool that mints identity and content together. The
   question that settles which is "can anything name this before it is complete", asked of the
   entity, never of the pass that happens to build it. _Consequence: a pass that must name what a
   later pass will build never keeps a private counter, a parallel table, or an
   append-in-the-right-order agreement checked after the fact -- it takes the identity from the pool
   that will answer to it, or, where that pool does not exist yet, from a typed allocator for it._
9. An identity that is meaningful only within a scope travels with that scope wherever it can meet
   another one. Invariant 6 makes an identity sufficient for resolution within its scope, which says
   nothing about what it means outside; so code that may be handed one from elsewhere has to be able
   to ask whether this one is its own, and it can ask only if the identity arrives paired with what
   it indexes. _Consequence: the pass that mints such an identity records what it indexes alongside
   it, and a consumer's "is this mine" is an equality against that, never a property of the target
   reconstructed from its surroundings. Where every such identity in a layer is unit-wide, this
   costs nothing and states itself._
10. A set reaches this compiler in an order the design decides, and nothing downstream reorders it.
    A container keyed on where its entries were allocated enumerates in an order that is a fact
    about the run rather than about the design; carried into what is compiled, it makes the same
    design compile to two different things, and two copies of one text state two things. Giving each
    entry its identity is not a place to repair that, because it is done one entry at a time and
    allocates as it goes: whatever it creates is already positioned by the order the entries came
    in, and a position is what two forms are compared on. So the order is settled where the set is
    built, by keeping the order the text produced its entries in. _Consequence: the component that
    builds a set keeps it in the order its entries first arose; a consumer walks it and does not
    sort; and a container keyed on a front-end pointer is answered by lookup, never iterated._
11. A declaration of a unit is identified from outside the unit's pools by a path of steps rooted at
    the unit, each step a kind and what tells it from its siblings: an application of a generate
    block by its label, which one it is among blocks of that label, and what distinguishes its
    instances where the naming needs it; a class by its name and, for a specialization, the digest
    of its arguments; a subroutine, a named block or a clocking block by its name; an enumeration, a
    structure or a union by the name it answers to in its scope (LRM 6.22.1); and a block or a type
    the source left unnamed by its position among its scope's declarations. This is LRM 23.6's
    hierarchical path rooted at the unit rather than the design, so it is the same for every
    instance. The steps are never composed into one name, since an escaped identifier may hold any
    character (LRM 5.6.1) and two paths would compose alike.
12. An identity distinguishes exactly what the layer below distinguishes. A key standing for
    compiled artifacts separates two inputs wherever the layers below would compile them differently
    and is free to merge them otherwise, and that is the statement to check of it. A declared type
    -- an enumeration, a structure, a union, a class -- is its declaration, which the layer below
    reads through a printed type name (LRM 20.6.1) and a type comparison (LRM 6.23) even where two
    declarations are laid out alike. Where one variant arm serves two concepts, the arm is split
    into one per concept rather than told apart by inspecting its payload.

## Boundary to Adjacent Layers

- Each IR layer defines its own id kinds and ownership edges. This doc constrains the shape of those
  definitions but does not enumerate them.
- The runtime uses its own object-level identity (object pointers, handles). Compile-time ids and
  runtime ids do not alias.
- [`incremental_build.md`](incremental_build.md) builds its stable keys on invariant 7.

## Forbidden Shapes

Each shape below fails an invariant above, and through it [`north_star.md`](north_star.md) invariant
4 or 5.

- `SymbolId` or any frontend-global id used as the identity of a compilation-unit-local entity. It
  fails invariant 7: a front-end address or number is a fact of one run, so nothing kept across
  builds can be spelled from it.
- A resolver keyed on `(instance_id, local_symbol)` at compile time. It fails invariant 3 and makes
  what a unit compiles follow its instances.
- A central registry or driver-owned map that holds the authoritative relationship between two
  architectural entities. It fails invariant 3: the relationship belongs to the entity that owns it.
  A unit's own pool of its declarations is not this; it is where they are owned.
- A design-global coordinate, ordinal, or path string used as semantic identity. It fails invariant
  7: it shifts when an unrelated entity elsewhere in the design is added, and every unit would
  depend on it.
- An identity of another unit's numbering held where a unit's own id belongs. It fails invariant 2
  and [`north_star.md`](north_star.md) invariant 5: the unit would depend on another's numbering,
  and an edit there that moves nothing it publishes would still recompile every unit naming it.
- A compilation unit reaching out to a design-level or external table to answer a question about its
  own declarations. It fails invariant 6.
- Duplicate ownership: the same relationship represented in two authoritative places. It fails
  invariant 3: two places are held in step by nothing.
- An id that cannot be resolved without a second lookup table beside the pool that owns its entity.
  It fails invariant 6.
- A pool's sequential id carried into a symbol, a cache key, a fingerprint or a name another unit
  computes. It fails invariant 7: inserting one entity then renames every entity after it.
- A test for whether an identity belongs to the asking code's own scope, answered by walking outward
  from the target to something near it -- an enclosing scope, a frame, a parent. It fails invariant
  9: such a test is a correlate of the question rather than the question, agreeing with the true
  answer for the case it was written against and diverging silently everywhere else.
- A raw integer standing in for an identity anywhere between the pass that reserves it and the pool
  that answers to it. It fails invariant 8: the counter a pass keeps to predict a later pass's
  insertion positions is an identity allocator with its type erased, and erasing the type is what
  lets the two drift until a separate check catches them.
- A declaration held in a pool that mints identity and content together, when a peer can name it
  before that content exists. It fails invariant 8: the staging is then re-created outside the pool
  -- a pre-built copy seeded in, or the content parked on a neighbouring node -- and the declaration
  stops being the one place that says what it is.
- An identity stored as the composed name it renders to, or composed in a layer that has no target.
  It fails invariant 4: a target's spelling rules, applied to some occurrences and not others,
  produce a defect no test of the composed string can see.
- A key that merges what the layer below compiles differently, or splits what it compiles alike. It
  fails invariant 12: the first runs one instance's code for another, the second compiles one thing
  twice.

## Notes / Examples

If a bug is fixed by adding a new lookup through a shared map, the ownership is probably wrong. The
correct fix is usually to move the data onto the entity that needs it, so the lookup disappears.

A wrong rendering is loud, because a compiler or a person reads it; a rendering standing in for an
identity is silent, because it renders fine and only fails to tell two things apart. Two classes
named `Box` in two packages are two identities however a target spells them; a class of another unit
is the pair of its declaring unit and its path, and a backend composes the one spelling it emits for
both the type name and the include, so mangling a character the target forbids reaches both alike.
