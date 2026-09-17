# A Settled Access Is Ordinary Operations

## Date

2026-09-17

## Status

Accepted. Completes
[structural-access-on-an-opaque-object](structural-access-on-an-opaque-object.md) D2's third
coordinate kind, which that entry named and left without a sealed value, and **reverses its D5**,
whose concern this entry keeps and whose mechanism it replaces. The last section records the shape
this project had already built and what implementing the third kind proved about it.

## Why this decision matters

A class a module or an interface declares is a type of that element's instance (LRM 6.22), so no
signature carries it and a referrer outside the declaring scope has no name for it (LRM 23.9). A
hierarchical name landing on a property or a behavior of such a class therefore resolves where the
design elaborates, and what that resolution produces is a value the compiled body applies.

What the compiled body cannot do is say "the member at position N of this record". It has no name
for the record, so it is in no position to make that statement at all -- and a layer that makes it
anyway hands every consumer below a name whose meaning none of them can check. What the body can say
is what it would have taken to reach such a member, in operations whose meaning does not depend on
knowing the layout: ask for the address, read that address as the type the access already knows,
reach the storage through it.

That was not how either half was built. The behavior half had a dispatch alternative meaning "a
position settled while the design elaborated"; the property half had a member-reference alternative
meaning the same. Both were hidden behind the same thing: the one backend that produces a program
refused every unit containing either, so a whole class of legal programs had no program at all and
nothing was checking the two backends against each other.

## What the three questions are

A referrer that cannot name the class asks the class itself, and there are exactly three questions,
separated by how much of the answer the object still gets to decide:

| The source wrote     | The class answers with | The object then decides |
| -------------------- | ---------------------- | ----------------------- |
| a property           | a storage position     | which storage it is on  |
| a virtual behavior   | a dispatch position    | which body fills it     |
| a behavior it is not | the body               | nothing                 |

The third row is the one this entry adds. LRM 8.14 settles it: a call that names no dispatch
position runs what the class the access names declares, whatever the object turns out to be. Nothing
is left over, so the answer is the body's own address rather than a position to find one at.

**All three are reached the same way**, and that is the entry's other half. Each is a runtime call
answering something, a conversion reading the answer as the type the access already knows, and then
either entering it or reaching through it. A generic language has all three operations; none of them
is "a member at a position" or "a dispatch at a position", which are the two statements a body
without a name for the class cannot make.

## Where other systems put this

Every system surveyed keeps a non-overridable call off its dispatch table, and resolves it to a
callable rather than to a slot.

- **The Itanium C++ ABI**, section 2.5 Virtual Table Layout, enumerates what a virtual table holds:
  virtual function pointers, vcall and vbase offsets, the offset-to-top, and the typeinfo pointer. A
  non-virtual member function has no entry, and a call to one is a direct call to its mangled symbol
  (https://itanium-cxx-abi.github.io/cxx-abi/abi.html).
- **The JVM** separates the two at the instruction level, JVMS chapter 6.5. `invokevirtual` invokes
  a method "based on the class of the object", distinguishing the method _resolved_ from the
  constant pool from the one _selected_ on the object's run-time type; `invokespecial` invokes the
  resolved method with no selection step at all, resolution having happened during linking (JVMS
  5.4.3.3). Two instructions exist precisely because the non-overridable case needs no table
  (https://docs.oracle.com/javase/specs/jvms/se21/html/jvms-6.html).
- **Swift** advises `final` where a declaration need not be overridden, because "this implies that
  the compiler can emit direct function calls instead of indirect calls" -- the overridable members
  are what a class's vtable is for
  (https://github.com/swiftlang/swift/blob/main/docs/OptimizationTips.rst, "Dynamic Dispatch").

**Where our conditions differ, and it is the load-bearing sentence.** In all three, the direct call
is resolved _by name_ -- a mangled symbol, a constant-pool reference, a module-level declaration --
because the caller can name the callee. Here the caller can name nothing about the class, which is
the whole subject. So the resolution has to happen at elaboration, where the instance fixes the
class, and what it produces is the callable itself. That is the JVM's `invokespecial` split with its
resolution moved from class loading to design elaboration, for the same reason: the earliest moment
at which the callee is known.

## Decisions

### D1. The third question's answer is the body, not a coordinate

A class answers a name it declares no dispatch position for with the address of the body. There is
no position to count and nothing to count it against: a position exists so that every class in a
lineage can agree on one identity for a behavior several of them answer, and a call the object gets
no say in has exactly one answering class.

The class record therefore carries a third by-name table beside the two it already has, read while a
reference resolves and never on the simulation path, and the walk it drives is the same one: start
at the class the access names, then ask what it extends. Starting there rather than at the object's
class is what LRM 8.14 requires, and is why a derived class declaring a method of the same name does
not capture the call.

### D2. A settled access is stated as the operations it is, with no alternative of its own anywhere

Neither a settled call nor a settled property access gets an alternative in any layer. Each is
written out of operations every layer already has:

```text
handle = <the object reference the source wrote>

-- a property
address = PropertyAt(handle, coordinate)
place   = deref( address as <the type the access states>* )

-- a behavior, whichever kind
code    = BehaviorAt(handle, coordinate)     -- one the object decides
        | <read the body slot>               -- one it does not
call    = (code as <the prototype the call states>)(ObjectOf(handle), arguments...)
```

A call, a conversion, a dereference. Three consequences, and each is a reason:

**A member reference cannot say this, because a body without a name for the record has no member to
refer to.** Giving it one means the layer states a name whose meaning depends on a layout it does
not have, and every consumer below then carries an alternative that exists only to hold what the
name could not: the position as a value, and the type it reaches -- the two things a call and a
conversion already carry.

**A callee form cannot say it either, for a sharper reason: it has one receiver and the operation
needs two values.** The class the handle's object is of answers which body; the object is what the
body runs on. A handle refers to an object without being one and nothing recovers either from the
other, so a single receiver is wrong whichever of the two it carries. That is not a preference
between shapes; it is a shape that cannot express the operation, and it is what the two backends had
already come to disagree about.

**It leaves each backend nothing to compose.** Every backend realizes a settled access the same way
-- ask the runtime, then use the answer -- so from a node stating neither step, each wrote both out
for itself. Stating them removes the duplicate, and with it the second place for two consumers to
answer one question differently.

The third of these is a simplification rather than a contract fix, which is worth saying because it
looked like one: composing the lookup out of the shared runtime-entry declaration is what the
contract's value-emission rule permits, so neither alternative was in breach of it. What they cost
was a rendering each backend had to get right alone.

### D3. The handle is bound once, because the call reads it twice

The source wrote one expression and the call needs two readings of it. The handle is therefore bound
to a local before either, so a receiver with side effects -- a call answering a handle, an element
of a queue the design is also writing -- happens once, as the source says.

### D4. What an erased entry takes is the object, and each target fills its own table

Every entry in a class's record is a code address with its prototype erased, so one table holds
bodies of every shape. What such an entry takes first is the **object**, which is what a body of any
class runs on. It is not the handle: putting a reference into the first parameter of every dispatch
entry would make each entry pay a recovery it does not need, and would stop a target from putting
the body itself in the table.

A target that lays out its own objects supplies a small entry per body that reads the object back as
the class it belongs to; a target whose objects the runtime lays out puts the body in directly,
every object of every class being the same shape there. Both are reached identically and neither
target ever reads the other's table, which is what erasure means here.

**What the entry is handed is the most-derived object, and the reading back is the target's own
conversion from that.** The walk that finds a body answers with the body alone and never converts,
so the entry belonging to a base receives an address of a class extending it. Where the target's
conversion from one to the other is not the identity -- a class answering a contract it holds
alongside its base, whose subobject sits at an offset -- the entry would need the object as the
class it belongs to rather than as the most-derived one, and the walk would have to convert along
the way. Dispatching on a behavior an interface class states is refused for a separate reason today
and is the only way to reach that case, so closing it belongs with whatever lifts that refusal.

### D5. A coordinate is a value the operations take, and is a target form nowhere

`structural-access-on-an-opaque-object.md` D5 makes the coordinate "an operand of the access, not a
second kind of access", taking "its place in the closed set of target forms". Its stated concern is
that no consumer should have to work out which operation it is looking at, and that concern is
right. Its mechanism is what this entry reverses.

D5 reasoned about how a member access names its target, and never asked whether the thing is a
member access. The requirement says it is not: a body with no name for the record has no member to
refer to. So the coordinate stays exactly what D5 called it -- a value, settled once where the
instance is known -- and stops being a form in any closed set, because the sets it was put into
belong to operations the body is not performing.

D5's concern is met more completely this way than by its own mechanism: there is no set left to
inspect. And the mechanism carried a cost D5 could not have seen from where it was written. Its
closing sentence -- "A backend that cannot realize the coordinate form refuses it; it never falls
back to another form" -- is what licensed one backend to refuse every unit containing such an
access, which is what kept the two backends from checking each other for as long as the alternatives
existed.

## Rejected alternatives

**Give every method a dispatch position, so a non-overridable call is a dispatch nothing
overrides.** It would be correct -- the lineage walk would find no takeover and land on the
declaring class's own entry. Rejected on two counts. It encodes a constant as a lookup, paying a
walk at every call to learn what elaboration already knew. And it makes "introduces a dispatch
position" mean something other than what LRM 8.20 means by it, which every reader of that table
would then have to be told. The survey is unanimous against it.

**Keep a settled dispatch node and let each backend recover the object its own way.** The node
carries the handle; one target opens it before the call, the other writes a recovery into the
emitted text. Rejected because the two targets then disagree about what the call's first argument is
while MIR states one thing, which is the shape that had already produced a silently wrong answer on
one of them.

**Put the handle in every dispatch entry's first parameter, so one value serves both halves.** It
works for a target that generates an entry per body, and not for one whose table holds the bodies
themselves -- the body takes the object, and nothing can wrap it after the fact. Rejected for making
the table's contents a per-target choice where the point of the table is that they are not.

**Resolve the name on the object at each call.** Rejected by
`../architecture/reference_resolution.md`'s standing prohibition on a per-access lookup, and for the
usual reason: the access sits wherever the design wrote it.

## What this falsified

Two alternatives had been built, one per half, each meaning "a position settled while the design
elaborated". Implementing the third question is what showed the shape could not hold: a
non-overridable call has no alternative to hide its second value in, so writing it forced both into
the open -- and the same two were needed by the dispatch position sitting beside it, where one
receiver had been serving both and the two backends had quietly come to different answers about
which value that was.

The property half then failed the same reading for its own reason, and it is the one a reader can
check without any of this context: a member reference that emits a call, a conversion and a
dereference is not a member reference. The alternatives had been kept apart by an argument that a
property access is a place and so cannot be a call, which the emitted text refutes -- the place is
the dereference, and the call is a call.

Recorded here because the shape was reached by a decision entry that anticipated three kinds and
described only two of them fully. The third was built later, from the outside, and the mismatch was
invisible from either end alone.

## Consequences

- A legal program that reaches a property or a behavior of a class no signature publishes has a
  program on every backend, so no path refuses a unit for containing one.
- A class's runtime record carries three by-name tables, read only while references resolve.
- No layer has an alternative meaning "settled while the design elaborated": not the member
  references, not the callee forms, not the dispatch references below them. Each set holds what a
  generic language has and nothing that names an elaboration.
- A member reference emits a member access in every backend, so reading the emitted text is enough
  to tell that no node is standing for operations it did not state.
- A method a class declares with no dispatch position becomes reachable by name from outside its
  unit, which is what the third table makes possible and what nothing else offered.
- An interface class's behaviors are unaffected: they answer dispatch positions, so they take the
  second row, and reaching one through a settled position stays refused where it already was.

## Cross-references

- `structural-access-on-an-opaque-object.md` -- the three coordinate kinds, the resolution split,
  and D5, which this entry completes and narrows.
- `dispatch-position-is-a-lineage-coordinate.md` -- what a dispatch position is, and why a
  non-overridable call is not one.
- `class-declared-in-a-structural-scope.md` -- the class whose type is per instance, which is what
  makes the referrer's lack of a name permanent.
- `runtime-entry-naming.md` -- the shape a runtime entry's signature takes from the values crossing.
- `../architecture/backend_contract.md` -- what a value-emission entry may name, which is what makes
  the two backends' hand-written lookups legal and still duplicated.
- `../architecture/object_model.md` -- one access operation per property and per behavior, and the
  class that lives on the object.
