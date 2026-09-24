# A type's readings exist because the type does

Date: 2026-09-16 Status: accepted; narrowed 2026-09-24 to the assignment-pattern text

**Narrowed.** An enumeration's readings are no longer functions per type:
[enum-representation](enum-representation.md) now states its members once as a run-time description
and puts the questions in the library. A description is data rather than a body, so the second cost
below -- a walk no program had written going unbuilt -- has nothing to apply to, and it is interned
where a use names it, as every other type description is. What follows stands for the
assignment-pattern text, which is still a function per type; read "enumeration" below as the case
that was answered this way until then.

## Why this decision matters

Two clauses give a declared type a reading that no value of it carries: LRM 6.19.5 has an
enumeration answer with the name it declares for a value and step through its member order, and LRM
21.2.1.6 has any type read as the assignment pattern its declaration names the parts of. Each is a
function over one value and no object.

Those functions were built at the first site that asked for one, and shared by every site after it.
That is one decision -- **when the function comes into existence** -- and it had been carried
implicitly beside a different one, the sharing, whose reasons do not reach it.

**What it gets wrong is what a declaration is.** Declaring an enumeration does not only introduce a
type; it introduces the fixed set of operations the standard grants that type, as surely as writing
a member introduces a member. Those operations take no parameter of their own -- each is one
concrete function of one concrete type -- so there is nothing about them left to be decided by a
later use. A declaration is the point at which what it brings comes into existence, and compiling
what it brought is not a favour done for whoever calls it.

Every language that writes this down explicitly does exactly that, and the line is the same one in
each: **what is parameterized is instantiated on use, and what is written down and concrete is
compiled where it is written.** A Rust derive expands into an item at the declaration, is
type-checked there, and reports its errors there whether or not the type is ever printed; a C++
member function you write is compiled at its definition. Only generics and templates wait, because
their candidate set is not a set until a use names a member of it. SystemVerilog spells none of this
-- a `typedef enum` mentions no method -- and that silence is what made the use site look like the
only place the work could be triggered from. It is a property of the surface, not of the semantics.

The identity cost follows from that rather than standing beside it. A function built by whichever
site the walk reached first takes its position from the program text around it, so inserting a `%p`
early in a unit shifts the identity of every reading after it, and the emitted symbol with it,
though nothing about those types changed. `../architecture/identity_and_ownership.md` names that
shape outright, and `../architecture/incremental_build.md` rests its whole reuse story on identity
not moving when an unrelated body is edited. A reading that exists because its type does has the
position its declaration has, and nothing a body is written near can move it.

It also costs what nobody sees. A body built in the middle of another body's lowering is reached
only when some program writes the construct that reaches it, so a walk no case had ever written was
never run: a packed tagged union with a `void` member (LRM 7.3.2) read a run its member does not
occupy, and a container of interface instances was walked as though an instance were a value. Both
had been in the tree since the rendering was, and both surfaced the day every type got one.

## The fork

**When does a per-type function come into existence?** The field is not actually split on this; it
is split on whether the thing is parameterized, and the systems below are read for where they put
each half rather than for a verdict.

- **rustc collects lazily, by reachability -- for instantiations.** `rustc_monomorphize::collector`
  offers two strategies. Lazy "means that items will only be instantiated when actually used. The
  goal is to produce the least amount of machine code possible", and it is the default. Eager "is
  meant to be used in conjunction with incremental compilation where a stable set of mono items is
  more important than a minimal one" -- it builds drop glue for every drop-able type "even if no
  drop call for that type exists (yet)". What it collects is generic instantiations, whose candidate
  set is unbounded, so reachability is the only thing that terminates it. What a `derive` produces
  is not in that set at all: it is expanded into an item where the type is declared, checked there,
  and reported there, and what removes it when nothing calls it is the linker.
  <https://doc.rust-lang.org/nightly/nightly-rustc/rustc_monomorphize/collector/index.html>
- **Verilator marks at the use site and materializes in a later pass.** A `%p` of an aggregate sets
  a flag on the type and a whole-design flag beside it; a separate pass runs only if that flag is
  set, walks the marked types and what their members reach, and emits the bodies, reporting the
  count as an optimization statistic. Its enumeration tables are memoized per type and attribute at
  the first site that asks and placed in `$unit`. It has no layer below its front end at which a
  later pass could drop unused work, so the saving has to be taken where the decision is made.
- **Go emits per type and prunes at link time.** A descriptor is emitted for every type and the
  linker's dead-code pass keeps what is reachable -- except that a `reflect` call with a
  non-constant method name switches the pruning off, because the linker can no longer tell what is
  called. <https://github.com/golang/go/issues/25081>

**A reading of a type is on the declared-and-concrete side of that line**, and the old shape had put
it on the other one. It has no parameter, so there is no instantiation for a use to pick; a unit's
types are interned into a pool complete and closed before any of that unit's bodies is lowered, so
there is not even an open set to terminate. What remains is where the two systems above put the
_removal_, and that is the only thing our conditions change. Units here compile independently with
only signatures crossing, while `../architecture/incremental_build.md` makes a callable's key a
function of ownership rather than of traversal, so the collection step Verilator and Go both put
_after_ the bodies has to happen _before_ them here. Before the bodies, the only thing there is to
work from is the unit's own declarations.

Go's caveat has an exact analogue that confirms the direction rather than complicating it: LRM
21.3.3 lets a format string be computed, so at such a call no directive is known until run time and
every operand's type has to carry its readings whether or not a `%p` ever appears. Under collection
by use site that is an over-approximation the lowering has to make and then explain; under this
decision there is nothing to over-approximate, because the readings are already there.

## Decision

**Declaring a type declares the readings the standard grants it, so they come into existence where
the unit's other declarations do -- before any body is lowered, for every type the unit names,
whether or not anything in it reads a value of that type. Which readings those are follows from the
type alone.**

1. **The type universe is the unit's own interned pool, walked in identity order.** A composite is
   interned after the types it names, so one forward walk reaches a type's parts before the type.
   This is the same pool and the same walk that already answers what each type became.

2. **Identities first, then bodies.** Every reading takes its identity in one pass and every body is
   filled in a second, because one reading's body calls the readings of the types it reaches and a
   type may name one interned after it. This is the staging
   [declarations-before-bodies](declarations-before-bodies.md) states, read one level down: a
   reading is named by bodies other than its own, so it is a declaration and not a body.

3. **Asking for a reading is a lookup that cannot fail.** A site that has established the type owns
   one finds it; a site that has not is asking a question it has no answer for, which is the
   compiler's own invariant broken and not a diagnostic. The failure path every asking site used to
   carry is gone, along with the memo that decided whether to build.

4. **What the clause converts is a value of a declared representation, and a type fixing none has no
   text.** An instance, a scope, a `void`, and a wildcard index type (LRM 7.8.1) each fix none --
   the last names where an index goes rather than what one is made of -- so nothing holding one has
   a text either, and the property composes outward through members and elements. Fixing no
   representation is not the same as never being held: an index does reach that position, carrying
   whatever width the expression was written at. The one exception is the clause's own: LRM 7.3.2's
   `void` tagged-union member is all information in its tag, so it asks nothing of a type with no
   text and does not deprive its union of one.

5. **A type the standard gives no way to enumerate is refused by name at the site that asks.** An
   associative array with a wildcard index is the case: LRM 7.9.4 through 7.9.7 each withhold
   `first`, `last`, `next` and `prev` from that index type and LRM 7.8.1 withholds `foreach`, so the
   entries LRM 21.2.1.6 asks for have nothing to be walked with. Building a reading for every type
   is what makes this a stated refusal rather than a walk nobody had written yet -- the refusal has
   to move to where the type is classified, because that is now the only place that still knows the
   program did not ask.

6. **Nothing classifies a type twice.** How a value of a type reads is one answer read off the type,
   and which walk builds its text is read off the same type at the point the body is built. Neither
   is derived from the other, and no answer is recorded beside the type.

## Rejected alternatives

- **Keep building at the first site, and take the identity from somewhere stable instead.** The
  identity would then be minted by a scheme running beside the pool that answers to it, which
  `../architecture/identity_and_ownership.md` names as the shape that lets the two drift. It also
  keeps a body being built in the middle of another body's lowering, which is what left two walks
  unreached for as long as they existed.

- **Collect the demand while bodies lower, then build in a pass after them.** This keeps the
  identity stable and removes the re-entrancy, and it is what Verilator does. It is rejected because
  the collection is a fact gathered over the whole unit before anything can be emitted for it, which
  `../architecture/lowering_boundaries.md` distinguishes from a stage that emits a declaration per
  entity; and because the demand set is an over-approximation wherever a format string is computed,
  so the pass would be answering a question it cannot answer precisely and does not need to ask.

- **Build only what is reached, and accept the identity moving.** The cost is invisible until
  something caches a unit's output across edits, at which point an unrelated insertion invalidates
  work that did not change. Deferring it means the scheme that has to be replaced is the one
  everything downstream already names bodies by.

- **Drop what nothing reaches, in the same change.** Measured on a 47-unit design, building every
  reading produces 337 functions where the sites ask for none, so something should drop them. It is
  not this decision: `../architecture/north_star.md` invariant 3 and `../design-process.md` both
  separate what a layer states from what an optimizer removes, and a pass that drops unreachable
  unit-internal bodies is correct or not on its own terms. What makes it cheap is already here -- a
  body no identifier answers to is exactly one no other unit can name.

## Consequences

- A `%p` and an enumeration method are each a plain call to a function that already exists, so the
  lowering of a print operand has no failure path of its own left.
- The two constructs stop having a cache between them and the unit; what remains is a table from a
  type and a reading to the function answering it, filled once.
- A design declaring a type nothing prints pays for that type's text. The count is bounded by the
  types a unit names, which is hundreds, and the work is generic MIR rather than emitted code until
  a backend realizes it.
- A walk that no program had ever written is now built by every design that names the type, so a
  defect in one surfaces at once rather than at the first user who writes the construct.

## Cross-references

- [rendering-a-value-by-its-type](rendering-a-value-by-its-type.md) -- what kind of thing a
  rendering is; this entry revises when it exists.
- [a-type-owned-computation-has-no-object](a-type-owned-computation-has-no-object.md) -- who owns
  it, and why a body no identifier reaches is reached by position.
- [declarations-before-bodies](declarations-before-bodies.md) -- the staging this is an application
  of.
- [hir-type-interning](hir-type-interning.md) -- why the type pool is the unit's own, complete and
  closed before its bodies lower.
