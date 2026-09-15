# An object is an event source, and a leaf depends on what its evaluation reached

Date: 2026-09-11 Status: accepted

## Context

[event-subscription-model](event-subscription-model.md) D5 wrote the vocabulary as event sources
rather than declared variables, on the grounds that the class property already showed a variable is
not general enough, and then defined only the declared-variable kind.
[update-events-are-per-variable](update-events-are-per-variable.md) D6 put the class property out of
scope, and [observability-is-not-a-storage-property](observability-is-not-a-storage-property.md) D4
refused to make a property's storage observable while quoting the clause that permits coarse
tracking. This entry defines the second kind of source, and the dependency rule that reaches it.

**LRM 9.4.2, p.233** decides most of it in four sentences.

- "If the event expression is a reference to a simple object handle or `chandle` variable, an event
  is created when a write to that variable is not equal to its previous value." So `@p` is an
  ordinary leaf over a declared variable, compared by handle identity.
- "Non-virtual methods of an object and built-in methods or system functions for an aggregate type
  are allowed in event control expressions as long as the type of the return value is singular and
  the method is defined as a function, not a task."
- "Changing the value of object data members, aggregate elements, or the size of a dynamically sized
  array referenced by a method or function shall cause the event expression to be reevaluated."
- "An implementation may cause the event expression to be reevaluated when changing the value or
  size **even if the members are not referenced by the method or function**."

The standard's own example on that page is this exact problem:

```systemverilog
Packet p = new;  // Packet 1
Packet q = new;  // Packet 2
initial fork
  @(p.status);   // Wait for status in Packet 1 to change
  @p;            // Wait for a change to handle p
  # 10 p = q;    // triggers @p
  // @(p.status) now waits for status in Packet 2 to change,
  // if not already different from Packet 1
join
```

That comment settles two things. The dependency on the object is rebound when the handle is written.
And the rebinding reevaluation is an ordinary comparison against the previous result -- "if not
already different" means the write to `p`, with no write to any `status` anywhere, can itself
satisfy `@(p.status)`.

**The same shape is not confined to class handles.** LRM 25.9, p.802 allows a virtual interface's
components in procedural statements while forbidding them in sensitivity lists, and its own example
is `@(posedge bus.grant)` inside a class method, where `bus` is a class property holding the virtual
interface. One event expression there reaches a property of an object and then a declared variable
of an interface instance, both resolved at run time.

**Two established systems solve the two halves of this.** In MobX a derivation declares no
dependencies: the runtime evaluates it, records which observables that evaluation read, subscribes
to exactly those, and drops the ones the new run did not read -- the dependency set follows the path
the evaluation took rather than the syntax. .NET's `INotifyPropertyChanged` is the other half: one
event per object rather than one per property, carrying the property's name so that every subscriber
filters for itself. Coarse source, evaluation-collected dependencies; both halves are what the
clauses above ask for.

**Nothing here is expressible today.** A sensitivity leaf names a cell resolved from a named
declaration in a scope, and a class property has no such cell, so `@(p.status)` cannot subscribe to
the property at all -- and no conformance case anywhere in the corpus puts a member path inside an
event control.

## Decision

**A class object is an event source. A leaf's dependencies are the event sources its evaluation
reached, recollected every time it is evaluated.**

**D1. An object carries one event source, covering all of its properties.** A write to any property
publishes it. Property storage stays plain: it gains no subscriber list, no event metadata, and no
pointer to the object.

**D2. A write publishes to the innermost object its place dereferenced, and to the declared variable
where its place dereferenced none.** `p.status = 5` publishes the object `p` names; `s.a.b = 5`
publishes `s`, which is `update-events-are-per-variable` D1. One rule with two kinds of source, not
two rules.

**D3. A leaf depends on what its evaluation reached, not on what its syntax names.** Evaluating
`p.status` reads the variable `p` and dereferences one object, so the leaf depends on both. Reading
the dependency set off the expression tree instead gets `@(p.get_x())` wrong the moment the method
reads a member of some other object, which the third sentence above requires to reevaluate.

**D4. Reevaluation rebinds, and a rebind is not exempt from the filter.** Every reevaluation
recollects the set: sources no longer reached are dropped, newly reached ones are added, and the
result is compared against the previous result like any other. The handle write in the standard's
example fires the leaf exactly when the new object's `status` already differs.

**D5. Coarse is the model; per-property is an optimization.** The fourth sentence permits
reevaluating for members the expression never reads, so one source per object is conforming. A
narrower source is admissible wherever it pays and is never owed -- the same standing
`update-events-are-per-variable` D4 gives every other granularity question.

**D6. A statically known dependency set is a compile-time specialization, not a second kind of
dependency.** Where an evaluation provably reaches the same sources every time -- which is every
event control that dereferences nothing -- the compiler emits the set once and no collection happens
at run time. That covers essentially all RTL. It changes what is computed, never what the model says
a dependency is.

**D7. A non-virtual method in an event expression needs no dependency extraction.** The coarse
source of every object the evaluation reached already covers every member that method read,
including members of objects it reached transitively. The method being non-virtual bounds what an
implementation that did want to analyse the body would have to consider; this decision does not need
to.

**D8. A null handle is not a case this model defines.** LRM 8.4, p.182: accessing a non-static
member via a null handle "is illegal", its result "is indeterminate", and implementations "may issue
an error". Evaluating `@(p.status)` with `p` null is that illegal access, whether at first
evaluation or at a reevaluation after `p = null`; it is a defect in the program rather than a state
the subscription must represent. A virtual interface is stricter -- LRM 25.9 makes use of a null one
"a fatal run-time error" -- and neither needs a rule here.

## Invariants

1. An object has exactly one event source and it covers every property. No property's storage
   carries event state.

2. A leaf's dependency set is what its most recent evaluation reached. A source the evaluation no
   longer reaches is not retained, so a stale object cannot wake a waiter that has moved on.

3. A write's publish target is named by its place at compile time and resolved at run time. It is
   never found by walking from storage to something that contains it.

4. A reevaluation caused by rebinding reports an event only where the leaf's result changed. Nothing
   treats a rebind as an event in itself.

## Rejected

- **A per-property event source.** It answers `@(p.status)` precisely and it is what the language
  would need if the fourth sentence of 9.4.2 did not exist. It does exist, and the cost is paid by
  every property whether or not anything waits on one: observation metadata on storage that
  `observability-is-not-a-storage-property` D1 removed it from, on the one category the language
  gives the most members. It survives as D5's optimization.

- **Making a class property observable storage.** The direct route, and it reverses the storage
  decision for the category where plain inline storage was hardest won. A property would become a
  cell rather than a value, and a reference to one would stop being an ordinary interior pointer.

- **Deriving the object dependency from the syntax** -- "the object currently reached through the
  handle expression". It is the natural reading and it is what D3 replaces, because a non-virtual
  method reading `other.x` reaches an object no part of the event expression names, and 9.4.2
  requires that member to cause reevaluation.

- **Two categories of dependency, static and dynamic.** The shape this entry started from. Splitting
  them puts a distinction into the model that belongs to the compiler: the language has one
  relation, and whether its members are known before the program runs is a property of the
  expression, not of the relation. D6 keeps the specialization and drops the category.

- **Depending on the handle alone.** What the current lowering would produce if a member path ever
  reached it, and it answers the standard's own example wrongly in both directions: it misses
  `p.status = 5`, and it reports an event on `p = q` whether or not the new object's `status`
  differs.

- **A pointer from property storage back to its object.** Not needed. A write to a property has the
  object's address in hand already, because dereferencing the handle is how it reached the property.

## Consequences

- **`@(p.status)` becomes expressible, and it is not expressible at all today.** So does the virtual
  interface form, which is how a class-based testbench reaches design signals.

- **Every object either carries a source or acquires one when first subscribed to.** Which of the
  two is a performance question this entry deliberately leaves open, alongside where a subscriber
  list physically lives and which variables need one.

- **A write to any property wakes every waiter on that object, and each filters.** That is the
  permission being spent, and it is where a per-property source would earn its keep once something
  measures it.

- **The compiler gains one question to answer per leaf**: does this evaluation reach anything
  through a run-time-resolved path. The answer picks D6's emitted set or D3's collection, and for an
  event control that dereferences no handle it is always the former.

- **`update-events-are-per-variable` invariant 4 is sharpened rather than reversed.** The set of
  sources a write publishes to is still a compile-time fact about the place, and the walk it forbids
  is still forbidden. What this entry adds is that the place may name a target whose identity is
  resolved at run time, which is already true of a write through a reference: the reference carries
  the cell it designates, and the callee publishes through it without knowing which variable it is.

- **The two remaining holes in the subscription model are now performance, not semantics.** Where
  the subscriber list lives, and which sources need one at all.

## Cross-references

- [event-subscription-model](event-subscription-model.md) -- the leaf, whose D5 named the event
  source and defined only its first kind.
- [update-events-are-per-variable](update-events-are-per-variable.md) -- the declared-variable
  publish rule this generalizes, and whose D6 deferred the class property to here.
- [observability-is-not-a-storage-property](observability-is-not-a-storage-property.md) -- that a
  property is not observable on referenceability grounds, which is what made a coarse source
  necessary.
- [storage-owns-its-value](storage-owns-its-value.md) -- the property's plain inline storage this
  entry leaves untouched.
- `../architecture/storage.md` -- storage, update and event identity as three things.
