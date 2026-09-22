# Referenceability does not imply observability

Date: 2026-09-11 Status: accepted

## Context

[reference-binds-a-cell](reference-binds-a-cell.md) made every referent one cell kind, and that cell
kind is the observable one: "a cell's address crosses as one `void*`, every cell entry reads it as
the same type, and the trigger entry already requires that address to serve as the observable. One
cell kind behind a reference is what keeps that `void*` meaning one thing."

The consequence was visible in the runtime's own commentary, which stated the coupling and its cost
in one sentence: a cell "is what a local whose storage is lent by reference gets, because a
reference reaches storage through a cell and through nothing else; **nothing subscribes to a
procedural local**, so the update event a write raises wakes no one."

So a borrowed automatic local carries a subscriber record -- one embedded `Registration`, 48 bytes
-- and every write to it copies the old value, compares, and walks an empty waiter list, to wake
nobody. That is not a semantic requirement. It is an ABI requirement, read backwards into the
storage.

Three facts make the coupling accidental rather than necessary:

- **Observability is decided per declaration site, never by type and never by demand.** The lowering
  wraps a design-hierarchy variable in an observable cell unconditionally and a class-method static
  local unconditionally not, and neither branch asks whether anything subscribes.
- **Two non-observable storage kinds already exist and are in use**: the activation value cell a
  cross-suspension local gets, which the runtime documents as "no subscriber wakeup: a procedural
  local is not observable", and the inline value a class property lives in.
- **Nothing in the value layer is observable at all.** Exactly two types carry subscriber metadata
  -- the variable cell and the resolved net -- so no struct member, array element, queue element or
  associative entry has ever had any, and the language has not been violated by that.

## Decision

**D1. Referenceability does not imply observability.** A storage object does not carry event or
subscriber metadata because something may pass it by reference. What a reference needs from storage
is a stable identity; that is unrelated to whether any process waits on it.

**D2. Plain storage and observable storage are two forms, not one.** A plain storage object is the
value; an observable one is the value plus the metadata a waiter registers on. Which form a
declaration gets is a property of that declaration, decided where the declaration is lowered.

**D3. Observability is a simulator concern separate from storage identity.** Lyra may satisfy the
language's event semantics with plain storage plus independent dependency machinery, and use an
observable storage form only where that is the better implementation -- not because a variable
happens to be referenceable, and not because it happens to appear in an event expression.

**D4. A class property is not observable on these grounds.** That `@(p.status)` must work does not
make a property's storage carry a subscriber list. LRM 9.4.2 requires that changing an object data
member "shall cause the event expression to be reevaluated", and explicitly permits an
implementation to reevaluate "even if the members are not referenced by the method or function" --
so the granularity of the dependency mechanism is the implementation's, bounded only by the rule
that a change of an operand without a change in the expression's result "shall not be detected as an
event". Coarser tracking is conforming; a per-property subscriber is one option among several.

## Invariants

1. A storage object's form is decided by its declaration, not by what any expression elsewhere does
   with it.

2. Lending a storage object by reference changes nothing about it. If a reference cannot name a
   form, that is a fact about the reference's representation and is fixed there.

3. An event is reported only where the waited expression's result changes. How often the expression
   is reevaluated is unconstrained above that, so a dependency mechanism may be coarser than the
   storage it tracks.

## Rejected

- **One unified cell for every storage that might be lent.** The shape in place. It is the ABI
  constraint of [reference-binds-a-cell](reference-binds-a-cell.md) applied to storage, and its own
  reopening already identified the direction of the error: "a constraint on how a place is lent was
  allowed to determine how the place itself is represented." This entry is that correction on the
  observability axis, as [storage-owns-its-value](storage-owns-its-value.md) was on the ownership
  axis.

- **Making a class property observable so that `@(p.status)` works.** It reads the clause as
  requiring a mechanism where the clause requires a result. The same clause grants latitude the
  reading throws away, and the cost of taking it is 48 bytes and a change-detection pass on every
  property write in every class, for a construct that no corpus case uses and that neither backend
  currently lowers.

- **Deciding observability from whether an expression anywhere waits on the variable.** Attractive,
  because it would make the metadata demand-driven. Rejected as a _general_ rule for the same reason
  the per-declaration rule is kept: under separate compilation another unit may wait on a published
  member, so "nothing waits on this" is not decidable where the declaration is lowered. It remains
  available where the whole program is in view.

## Consequences

- **A borrowed automatic local stops being an observable cell.** It keeps stable storage, because a
  reference needs that; it loses the subscriber record and the change-detection work on every write.

  **This did not hold, and what replaced it argues the other way.**
  [a-declared-variable-is-one-storage](a-declared-variable-is-one-storage.md) gave every one of a
  body's variables one storage decided by its declaration alone, and read LRM 9.4.2 as granting
  every variable the property of being waitable -- the event expression takes an expression with no
  restriction on what declared it -- so giving only some of them that property would be a saving
  taken from reading the whole body, which is the shape that entry removes. Every body variable is
  therefore an observable cell today, borrowed or not. What this entry establishes is untouched: the
  form is still decided by the declaration and never by what an expression elsewhere does, and a
  class property is still not observable. What is open is whether the subscriber record every
  procedural variable now carries is worth what it costs, which is a measurement nobody has taken.

- **A reference must be able to name both forms.** That is a requirement on the reference's
  representation, and it is settled in
  [reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md).

- **The three identities of `../architecture/storage.md` gain their implementation counterpart.**
  Storage identity is the storage object; update and event identity are the dependency machinery's;
  this entry is what stops the first from being required to carry the other two.

- **What a class property's event mechanism is, is not decided here.** D4 says only that the
  property's storage need not carry it. A design for dependency tracking -- per object, per
  expression, or otherwise -- is separate work, and nothing in the corpus reaches it yet:
  `@(p.status)` fails to build on the C++ backend and is refused on the execution backend.

## Cross-references

- [reference-binds-a-cell](reference-binds-a-cell.md) -- the coupling this breaks, and the reopening
  that named the error's direction.
- [storage-owns-its-value](storage-owns-its-value.md) -- the same correction on the ownership axis.
- [reference-is-a-tagged-pointer](reference-is-a-tagged-pointer.md) -- how a reference names both
  forms.
- [cross-suspension-value-storage](cross-suspension-value-storage.md) -- the non-observable
  procedural cell, which this entry generalizes rather than introduces.
- `../architecture/storage.md` -- storage, update and event identity as three things.
- `../architecture/scheduling.md` -- where the dependency machinery D3 leaves open belongs.
