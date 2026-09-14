# An event subscription owns expression leaves, each with its own result and gate

Date: 2026-09-11 Status: accepted

## Context

[update-events-are-per-variable](update-events-are-per-variable.md) put the update on the declared
variable and the filter on the expression, and left the filter's holder undescribed: a subscription
holds the dependencies and the previous result. Describing it properly turns out to settle more than
it looked, because walking every legal form of an event expression against that shape breaks it in
two places and confirms it everywhere else.

**LRM 9.4.2.1, p.233** makes `or` an OR of _events_: "the occurrence of any one of the events
triggers the execution of the statement that follows it". So `@(a or b)` fires when `a` changes even
where `a | b` does not, and a shape with one dependency set and one previous result answers it
wrongly.

**LRM 9.4.2.3, p.235** makes `iff` a gate rather than an operand: "The event expression only
triggers if the expression after the `iff` is true ... **This type of expression is evaluated when
`a` changes and not when `enable` changes**." It also gives `iff` precedence over `or`, so the gate
belongs to one leaf rather than to the whole control.

Everything else the language admits is covered by reevaluate-and-compare over declared variables,
with one exception. `@(s.a)` depends on `s`; `@(arr[i])` depends on `arr` and on `i`, since the
index is an operand; `@(v[3:0])` depends on `v`; `@(q.size())` depends on `q`, which 9.4.2 requires
by naming a change of a dynamically sized array's size as a cause of reevaluation. The exception is
`@(p.status)`: the same clause requires that changing an object data member reevaluate the
expression, and an object is not a declared variable, so a dependency set of variables cannot
express it.

One existing construct is evidence rather than a case to handle. `wait (cond)` is lowered to a loop
around a value-change wait on the condition's reads, and the corpus exercises
`wait (first_term && second_term)` -- a compound condition, running on both backends -- while
`@(a + b)` is refused. **The same read set is safe under `wait` and unsafe under `@`, and the
difference is exactly the comparison against a previous result**: the loop retests the condition
where the event control does not.

## Decision

**A subscription owns a list of event-expression leaves. Each leaf owns what it depends on, what it
compares against, how it compares, and what gates it.**

**D1. The subscription holds the waiter and a list of leaves.** An event control is satisfied when
any one of its leaves reports an event. Nothing collapses the list into a single expression.

**D2. A leaf owns four things**: the set of event sources it depends on, the previous evaluated
result of its expression, the predicate that decides whether a reevaluation is an event (any change,
or an edge on the least significant bit), and an optional gate.

**D3. A dependency's update reevaluates the leaf, and only the predicate reports an event.**
Reevaluation may happen more often than an event is reported; reporting without the predicate being
satisfied is forbidden (LRM 9.4.2).

**D4. A gate is read and never depended on.** It is evaluated only after the leaf's own predicate
has triggered, and a change to what it reads causes nothing. It belongs to one leaf, because `iff`
binds tighter than `or`.

**D5. A dependency is an event source, not a declared variable.** A declared variable is the
principal kind and the only one this entry defines; the vocabulary is written as event sources
because the class property already shows a variable is not general enough.

**D6. Edge, bit offset and bit width are fast paths, not primitives.** A bit projection is the
expression `v[3:0]`, and an edge is a comparison of the old and new least significant bit. Keeping
them as specializations that avoid materializing a result is welcome; building the model on them is
what made two expression forms look like the whole language.

**D7. A level-sensitive `wait` needs no leaf kind of its own.** It is a loop around change leaves
with the truth test as ordinary generated control flow, which is how it is realized today. Its
predicate is "the condition is true", tested by the loop, rather than anything the subscription
holds.

## Invariants

1. The previous result and the gate are per leaf. No consumer may treat a multi-leaf control as one
   expression over the union of its dependencies.

2. A gate is never a dependency. Nothing subscribes to what a gate reads.

3. No event is reported unless the leaf's predicate is satisfied against its own previous result.

4. A leaf names event sources and an expression. It names no storage, no address and no component.

## Rejected

- **One dependency set and one previous result per subscription.** The shape this entry started
  from. It answers `@(a or b)` wrongly, because 9.4.2.1 ORs events rather than operands, so a change
  to `a` must fire whether or not the disjunction's value moved. The list is not an implementation
  convenience; it is the semantics.

- **Putting the `iff` operand in the dependency set.** The natural reading of a conditional event
  control, and the standard forecloses it in a sentence: the expression "is evaluated when `a`
  changes and not when `enable` changes". A dependency there would wake a process on a gate change,
  which is an event the language does not have.

- **Keeping edge and bit range as the architecture's primitives.** They are what the runtime has
  today, and a model built on them can express `@(posedge clk)` and `@(v[3:0])` and nothing else.
  The general form subsumes both; the specializations survive as fast paths under D6.

- **A separate subscription kind for `wait`.** It looks like a third predicate -- level rather than
  edge or change -- and it is not one: the existing lowering already reduces it to change leaves
  plus a loop, and that reduction is what makes a compound `wait` condition correct today.

- **Typing dependencies as declared variables.** It would be true of everything this entry defines
  and false of the first thing added after it. D5 costs nothing now and avoids a vocabulary change
  later.

## Consequences

- **The outer runtime structure survives unchanged.** A trigger is built per leaf and a wait takes a
  span of them, which is already the list of D1. What changes is what a leaf holds, not how leaves
  are assembled or how a process parks on them.

- **A leaf gains state and the wake path gains work.** The previous result is a value rather than a
  bit range, and a dependency's update evaluates an expression rather than comparing a projection.
  That is the cost of covering the language, and D6's fast paths are where it is bought back.

- **Compound event expressions become expressible.** They are refused today because the previous
  result of such an expression is not any storage's value and there was nowhere to keep it. There is
  now.

- **The class property is the one hole, and it is named precisely.** What it needs is not
  component-level event identity -- nothing else in the language wants that -- but an event source
  that is not a declared variable, and something that publishes to it when an object data member is
  written.

- **`wait` is evidence the model is aimed correctly.** That one read set is safe under a retesting
  loop and unsafe under a one-shot event control is the same distinction D2 draws between a
  dependency and a previous result.

## Cross-references

- [update-events-are-per-variable](update-events-are-per-variable.md) -- the update side, whose
  filter this entry gives a holder.
- [observability-is-not-a-storage-property](observability-is-not-a-storage-property.md) --
  observation as a mechanism beside storage, of which this is the subscriber half.
- `../architecture/scheduling.md` -- the suspension protocol a subscription parks a process on.
- `../architecture/storage.md` -- storage, update and event identity, of which a leaf is the third.
