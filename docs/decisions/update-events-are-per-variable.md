# An update event is raised on the declared variable, and the compiler emits it

Date: 2026-09-11 Status: accepted

## Context

[storage-owns-its-value](storage-owns-its-value.md) D3 made a component write land in the
component's own storage, rather than rebuilding the whole aggregate and storing it back through its
owner. That removed the path by which a component write reached the containing variable's cell --
and with it the only thing that raised the containing variable's update event.
`../architecture/storage.md`'s invariant that "a component's write is an event on the variable that
contains it" was left stated and unmechanised: after the storage decisions, `s.a = 5` no longer
passes through anything that would wake a process waiting on `@(s)`.

The obvious repair is to give each component storage a pointer to its containing observable and walk
it on every write. It is the wrong repair, and the standard says something better.

**LRM 4.3, p.62** puts the update event on the variable: "Every change in state of **a net or
variable** in the system description being simulated is considered an update event. Processes are
sensitive to update events."

**LRM 9.4.2, p.232-233** puts the filter on the expression: a non-edge implicit event "shall be
detected on any change in the value of the expression", and "a change of value in any operand of the
expression without a change in the result of the expression **shall not** be detected as an event".
It also permits an implementation to reevaluate "even if the members are not referenced by the
method or function".

**LRM 13.5.2, p.349** settles what a component is not. Its exhaustive list of what may be passed by
reference names "a variable" beside "a member of an unpacked structure" and "an element of an
unpacked array", so a component is not itself a variable in the standard's own vocabulary.

Together these answer the question that looked like it needed a containment rule. Writing `s.a.b`
changes the state of one variable, `s`. `@(s)`, `@(s.a)` and `@(s.a.b)` are three expressions over
that one variable, each reevaluated and each reporting only if its own result changed. There is no
ancestor chain in the language, only one variable and several expressions reading it.

## Decision

**An update event is raised on the declared variable a write reaches, once, whatever depth the write
landed at. Which variables a write updates is known at compile time and emitted by the compiler.**

**D1. The update is per declared variable, not per component.** A write anywhere inside a variable
is one update of that variable. No event is raised for the intermediate components on the path.

**D2. A wait is an expression, and its filter belongs to the subscription.** A subscription holds
the variables it depends on and the previous result of its expression; an update of a dependency
reevaluates it and an event is reported only if the result changed. The previous result is not the
value of any storage, so it is held by the subscription and nowhere else.

**D3. Storage does not discover its observers.** A component storage object carries no pointer to a
containing observable, no ancestor link and no event metadata of any kind. The compiler knows the
source-level place a write targets and emits the notification the place implies.

**D4. Finer granularity is an optimization, never a requirement.** 9.4.2 permits reevaluating more
often than the members are referenced, so a narrower notification -- per element, per component --
is admissible where it pays and is never owed. Nothing may report an event without a result change;
that is the only hard bound.

**D5. The storage operation and the publish operation are two operations.** A write stores, and a
publish raises an update on a variable. That they are fused today in one cell's store is a property
of the current realization, not of the model.

**D6. The class property is out of scope.** `observability-is-not-a-storage-property` D4 left what
makes `@(p.status)` work undecided, and this entry does not decide it. A class property is not a
variable in 13.5.2's vocabulary either, and the object it belongs to is not one, so the rule above
does not reach it.

## Invariants

1. **The storage graph is not the update propagation graph.** Physical containment says where bytes
   live; it never says which variable's update a write is.

2. A storage object carries no event state by virtue of containing, or being contained by, another.

3. An event reaches a process only where the waited expression's result changed. How often the
   expression is reevaluated above that is unconstrained.

4. The set of variables a write updates is a compile-time fact derived from the place being written,
   never a run-time walk.

## Rejected

- **A parent pointer on component storage, walked on each write.** The direct repair for the hole.
  Rejected because it puts source-level containment and event propagation into the physical storage
  representation, which
  [observability-is-not-a-storage-property](observability-is-not-a-storage-property.md) removed them
  from. It also does not stop at one level: `a[i].x.y = v` would need a chain, and every plain
  storage object on that chain would start carrying event metadata to support it.

- **Raising an update for each component on the path.** The reading where `s.a.b = 1` updates `b`,
  then `a`, then `s`. It has no textual support -- 4.3 puts the event on a variable, and 13.5.2's
  enumeration says a component is not one -- and it would multiply notifications by the nesting
  depth for no observable difference, since a process waiting on any of those expressions is reached
  by the one update of `s` anyway.

- **Deriving the update set at run time from the write's address.** It would need the containment
  structure reachable from the storage, which is the parent pointer under another name, and it would
  answer at run time a question the place already answers at compile time.

- **Keeping the whole-value store so that the existing notification path survives.** The cheapest
  way to close the hole: undo the component write. Rejected -- that store is what
  [storage-owns-its-value](storage-owns-its-value.md) removed, on evidence that it answers legal
  programs wrongly, and the notification is recoverable without it.

## Consequences

- **The hole is closed, and closed without new storage state.** `s.a = 5` stores into the component
  and publishes an update of `s`, both emitted by the lowering from a place it already has.

- **`Var<T>`'s fused store-detect-wake separates.** A cell that stores, detects a change and wakes
  subscribers in one operation is the shape that made observation an intrinsic property of storage.
  Under D5 a store is a store and a publish is a publish; the current class remains a workable
  realization for the case where the expression is one variable, and stops being the architecture.

- **Compound event expressions become expressible.** They are refused today, and the refusal is
  exact: a trigger whose sensitivity list has more than one entry is rejected. The reason is that
  the previous result of such an expression is not any storage's value and there was nowhere to put
  it. D2 puts it in the subscription, which is what unblocks them.

- **A write to one element of a large array reevaluates every waiter on that array.** One variable,
  one update, every dependent expression reevaluated and filtered. That is conforming by 9.4.2's
  explicit permission and it is where a finer notification would earn its keep -- as an optimization
  under D4, once something measures it.

- **The physical shape of observation is deliberately not decided.** Where a subscriber list lives,
  and which variables need observation machinery at all, are performance questions that this entry
  is written to precede rather than settle.

## Cross-references

- [storage-owns-its-value](storage-owns-its-value.md) -- the component write that opened the hole
  this closes.
- [observability-is-not-a-storage-property](observability-is-not-a-storage-property.md) -- that
  observation is separate from storage, which this entry gives its propagation rule.
- [value-projection-write](value-projection-write.md) -- the whole-value store whose removal took
  the old notification path with it.
- `../architecture/storage.md` -- storage, update and event identity as three things; this entry is
  the second and third of them.
- `../architecture/scheduling.md` -- the region an update event is delivered in, which this does not
  change.
