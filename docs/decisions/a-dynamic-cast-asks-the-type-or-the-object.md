# A dynamic cast asks the type or the object

Date: 2026-09-17 Status: accepted

## Why this decision matters

A program can ask, while it runs, whether a particular value may become a value of a named type, and
take the answer either as a value it branches on or as a failure reported against the design (LRM
6.24.2). Nothing in the compiler had a shape for that. The construct was refused in the front end on
both targets, no conformance case wrote it, and the progress file that routed it named a home that
never carried it -- so it was invisible to the coverage record by construction rather than by
oversight.

What makes it a decision rather than a feature is that the question has two answers living in two
different places, and which one applies is fixed by something permanent rather than by convenience.

## What the construct is

Three shapes, one skeleton. The middle line is the only thing that differs:

```
answer = <may this value become a value of this type>
if (answer) destination = (Dest)value
if (!answer) report                          // only where the source asked to be told
```

```
// 1. The declared types settle it -- the destination is the source's own class
//    or one it extends, or two types the standard converts between.
answer = 1

// 2. The destination is an enumeration: the type answers.
answer = <value is a member of Dest>

// 3. The destination is a class: the object answers.
answer = <the object this handle refers to is of Dest, or of a class extending it>
```

## The decision

### D1. The construct is a run of steps ending in the answer

The value is settled once, the question is asked of it, the destination takes it where the answer is
yes, and the answer is what the whole expression is. One value is asked about and stored, so the
question and the assignment cannot be about different values.

### D2. The failing spelling is the same steps plus a report

LRM 6.24.2 states this itself: "use of `$cast` as either a task or a function determines how invalid
assignments are handled". So the two spellings differ in one statement and nothing else, and nothing
below the lowering that decides it hears about which one the source wrote.

The report is a severity report rather than a lowering's refusal or an abort: the design asked for
an assignment the standard calls invalid, which is
[run-time-failure-is-not-an-outcome](run-time-failure-is-not-an-outcome.md)'s second kind. It does
not end the run, because the standard's own sentence -- the destination "is left unchanged" -- is
about what the program sees afterwards, and only means something to a program that continues.

### D3. Which check is made follows from who fixes the values the destination accepts

An enumeration fixes its members where it is declared, so the type is what answers, as a function of
the type and one value with no parameter of its own. That is the shape
[a-types-readings-exist-because-the-type-does](a-types-readings-exist-because-the-type-does.md)
established for a type's other readings, and it is the same walk `name` already makes: LRM 6.19.5.6
requires the empty string for a non-member, so the type already owns the question of which values
are members.

**The classes that may extend a given one are open across compilation units.** A class declared
anywhere may extend a class declared here, so no unit holds that set and no unit can test membership
in it. The object carries its own class and is the only thing that can answer. This is the design's
load-bearing sentence, and it is a condition rather than a gap:
[`north_star.md`](../architecture/north_star.md) invariant 5 makes independently compilable units
with explicit dependencies a first-class constraint, so it stays true however much gets built.

### D4. Where the declared types settle it, the answer is a constant

Two of the four situations need no run-time question at all, and one of them needs no assignment
either: where the standard defines no conversion between the two types, the destination is written
on no run and there is nothing to write it with. Stating an assignment there would mean inventing a
conversion the language does not have, and the site would then refuse a program the standard
accepts.

### D5. No semantic layer gains an alternative

HIR gains the construct, because the construct is what the source wrote. Below it the whole thing is
nodes that already exist: a run of steps ending in a value, a conditional, a store, a conversion, a
call, and the severity report every tool-issued report already goes through. The two questions are a
per-type function and a runtime entry, neither of which is a node.

What did gain an alternative is LIR, and it is a gap this reached rather than a shape this invented:
a body can now name a class's record. The execution backend already reads a class off an object type
for a member projection and for a dispatch target; only a body naming one had no spelling, so the
record was reachable from a static constant and from the other backend and from nowhere else.

## What the field does, and where our conditions differ

The split is unanimous and it is about **when the set is fixed**, which is exactly D3.

- **The JVM** has `instanceof` push 0 or 1 and `checkcast` throw, and says outright that the second
  "is very similar to" the first, differing in "its behavior when its test fails". The symbolic
  class reference is resolved _during instruction execution_, because the hierarchy is not fixed
  when the class file is written. (JVMS SE21 chapter 6.5, `checkcast` / `instanceof`.)
- **The Itanium C++ ABI** puts the check in the runtime --
  `__dynamic_cast(sub, src, dst, src2dst_offset)`, returning null on failure -- while the compiler
  handles the cases it can settle inline and passes what it knows as a static hint. Even with the
  whole hierarchy in view, the decision that is not statically settled goes to the runtime.
  (<https://itanium-cxx-abi.github.io/cxx-abi/abi.html>, the `__dynamic_cast` entry point.)
- **Go** spells the same pair as one construct with two forms: `x.(T)` alone panics, and the
  comma-ok form yields a boolean beside the value. (Go specification, Type assertions.)
- **Ada** checks a conversion into a constrained subtype against that subtype's constraint at run
  time and raises `Constraint_Error` -- emitted by the compiler rather than asked of a runtime,
  because the constraint is compile-time data. (Ada RM 4.6, Dynamic Semantics.)

Ada and the JVM are the two halves of D3 and they differ exactly where the set's owner differs. Our
conditions do not differ from either, so both answers are taken as they stand.

The front end confirms the same classification independently: slang's own `$cast` binding sorts the
argument pair into "the enum case, where the value is checked", "both singular, so cast
compatibility decides", "the destination is already a supertype, so it always succeeds", and "two
unrelated classes, so it never can" -- and uses it only to warn, never to reject.

## Rejected alternatives

- **One checked-cast node whose realization each backend picks from the type pair.** The two
  realizations are different operations rather than two spellings of one -- a membership test
  against compile-time data, and a question asked of an object -- so a backend reading the pair
  would be deciding rather than translating. [cast-is-a-pair-of-types](cast-is-a-pair-of-types.md)
  admits a pair exactly where it admits one conversion, and this is not that.

- **A per-class table of the subclasses a destination accepts, tested by membership.** This is the
  shape that makes the check look symmetric with the enumeration's, and it cannot be built: the set
  is open across units, so any unit's copy is a closed-world answer to an open-world question. It
  would also be wrong only for programs spanning units, which is the hardest way to be wrong.

- **Modelling the destination as an output argument of a call.** It is how the front end binds it,
  and it is not what the construct does: an output argument is written back unconditionally, and the
  standard requires the destination to be left holding what it held.

- **Answering with the object seen as the destination class, the way `__dynamic_cast` returns the
  adjusted pointer.** The adjustment and the check are one walk there because C++ has multiple and
  virtual inheritance to adjust for. Here "the same object seen as another class" is already a cast
  whose realization each backend owns, so folding it into the answer would make the two halves of
  this construct different shapes at the site and buy nothing.

- **Refusing a pair of types that can never succeed at compile time.** LRM 6.24.2 says the function
  spelling "will never issue a run-time or compile-time error", and the front end accepts such a
  program. The answer is 0.

## Consequences

- A destination's declared type decides which question is asked, and the lowering that knows both
  types is where that is settled; no backend classifies anything.
- A handle referring to no object answers no with no branch written for it: the walk starts at
  nothing and ends having found nothing, which is LRM 8.16's own reading of its second case -- it
  requires "an object that is assignment compatible", and a null handle refers to none.
- Every enumeration a unit names now owns a third reading. It is built where the others are, so a
  design declaring an enumeration nothing casts to pays for it, on the same terms the other two are
  already on.
- The execution backend can reduce a machine integer to a predicate. It could not before -- it asked
  for the value domain of an operand that has none -- which no program had reached because no
  runtime entry had handed a body a plain answer to branch on.
- Which spelling the source used is decided at the positions that discard a call's answer, and there
  are two: a statement, and a for-loop step (LRM A.6.8). Only the first can carry the void cast that
  says the source called the function anyway, because the grammar admits a subroutine call as a step
  and no cast around one. Stating it at one of the two and defaulting the other is what the first
  attempt did, and the step took the wrong spelling silently; the rule now sits in the one place
  both positions lower through, so a third such position would have to name its own answer rather
  than inherit a wrong one.

## Cross-references

- [run-time-failure-is-not-an-outcome](run-time-failure-is-not-an-outcome.md) -- which of the three
  kinds of failure a design's invalid assignment is, and why the severity is ours to choose.
- [a-types-readings-exist-because-the-type-does](a-types-readings-exist-because-the-type-does.md) --
  when a per-type function comes into existence, which the enumeration's half of this is an instance
  of.
- [cast-is-a-pair-of-types](cast-is-a-pair-of-types.md) -- the conversion this performs once the
  answer is yes, unchanged by this entry.
- [structural-access-on-an-opaque-object](structural-access-on-an-opaque-object.md) and
  [a-settled-access-is-ordinary-operations](a-settled-access-is-ordinary-operations.md) -- the
  record an object carries, which is what the class half of this asks.
- IEEE 1800-2023: 6.24.2 (`$cast`), 6.22.3 / 6.22.4 / 6.22.5 (assignment compatible, cast
  compatible, type incompatible), 6.19.5.6 (`name` of a non-member), 8.16 (casting between class
  handles), 8.26 (interface classes), 20.10 (severity tasks).
