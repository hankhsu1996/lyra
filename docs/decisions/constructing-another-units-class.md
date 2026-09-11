# A construction reaches its constructor by the identity it reaches the class by

Date: 2026-09-10 Status: accepted

## Status

Accepted. Applies [entering-a-class-construction](entering-a-class-construction.md) D2 to a class
this unit did not declare, and leaves the promise
[reaching-past-a-published-class](reaching-past-a-published-class.md) D1 enumerates as it stands;
reverses neither.

## Context

Bringing an object into existence and initializing it are two operations, and the second is a call
to the class's constructor. Which class that is has two spellings -- a declaration this unit
compiles, or the pair of names another unit's signature carried -- and the two operations did not
agree about which spellings they accept. The allocation named the class through the same mechanism
either way, because the definition it takes is reached by the name the declaring unit links it
under. The constructor was reached only through a local class id.

Nothing refused. A construction whose class another unit declares fell out of the construction
lowering entirely and became an ordinary call to the allocation with the constructor's arguments
still attached, so a class with a constructor taking arguments aborted on an arity mismatch at load
time, one taking none left every property holding nothing -- a packed read of it aborted, and a
`string` read answered the empty string and the run finished at exit 0. The two backends compiled
the same MIR and printed different answers, which is the failure the agreement between them exists
to make impossible.

**An operation available under one naming and not the other is what makes this worth a record rather
than a fix.** The allocation and the constructor call are one construction; a reader adding a third
operation to it will reach for whichever of the two mechanisms is nearer, and the near one is the
class id.

## Decision

**A construction reaches its constructor by the same identity it reaches the class's declaration by.
Which unit declares the class decides where each answer is read and nothing after it.**

### D1. The constructor is named the way every other method of that class is

A class of another unit is linked under the name that unit composes from its own name and the
class's; its constructor is that name and one more segment. Both sides compose it from the same
parts, so they agree with no table between them -- the rule already in force for every cross-unit
method, and a constructor is not an exception to it because a constructor is not dispatched
(`entering-a-class-construction` D1) and being named outright is what is left.

The segment naming the constructor is minted here rather than written by the source, so it is
composed in one place and both the unit that emits the body and the unit that enters it take it from
there.

### D2. A construction reads nothing about the constructor it enters

The source wrote the call, so the front end bound it against the declaration and filled in whatever
it left to a default. Every argument the constructor takes is therefore stated by the construction
itself, and the class's identity is the whole of what the lowering needs. This is what keeps the
promise out of the construction path: a unit that constructs another unit's class and reaches
nothing on it consumes no promise about it and needs none.

The assertion the local path used to make -- that the call states one argument per formal -- goes
with this. It compared two things one unit produced from one declaration; the other naming has only
one of them, and a rule that holds under one spelling of a class and not the other is the shape this
entry exists to remove.

### D3. A class that extends another states the arguments its base construction carries, always

A base construction is entered whether or not the source wrote the call, and there are three ways
its arguments arrive: an explicit `super.new(...)`, the arguments written on the extends specifier,
and the base constructor's own default values where the source wrote neither (LRM 8.7, 8.17). The
class reading its own declaration is where all three are answerable, so that is where the argument
list is settled -- completely, or not at all.

**What made this a decision rather than a detail is that the list was allowed to arrive empty and
mean two things**: a call the source wrote with no arguments, and a call nobody had computed yet.
One representation for two states, and every consumer below paid for it -- one backend composed a
call with the wrong arity from it, the other needed a discriminator carried across the unit boundary
to tell the two apart, and the arguments of an extends specifier were dropped on the floor because
nothing asked for them.

So the class states the complete list, and a class that cannot be given one is refused where the
declaration is read. Nothing below re-derives it, nothing asks a promise about it, and the two
backends receive the same complete list or never see the class at all. The refusal that remains
names one thing -- a base constructor formal left to its default -- and it is the same refusal
whichever unit declares the base, because the default value is what is missing either way.

### D4. Extending another unit's class consumes that unit's promise only where it reads one

`reaching-past-a-published-class` D3 makes reading a promise what records the dependency, so the
question is which references read one. Naming a class as a base reads nothing on its own: what its
construction carries is settled from the declaration, and where a lineage position or a property is
reached, that reference consumes the promise where it is made.

**Committing to an interface class is the same, and the difference from extending is worth stating
because the two look alike.** A class commits to an interface rather than extending it, so nothing
of the interface class is entered, positioned, or counted through by the committing class itself.

The failure this avoids is worth keeping: reading a promise for every reference that merely _names_
another unit's class broke a case that did exactly that and nothing more. A promise is consumed by
the reference that reads it, never by the reference that names it -- which is the same rule D2
states for a construction, one layer up.

## Rejected alternatives

- **Put what a base construction needs on the promise, as a count of its formals or as a flag saying
  whether it takes any.** Both were built and both were wrong, and the way they were wrong is the
  reusable part. The list a base construction carries is settled from a declaration, so a referrer
  needs nothing about it; a fact that has to cross a unit boundary to answer a question about _our
  own_ class is a sign the question is being asked at the wrong layer. The count also named a list
  the promise does not carry, which invites the reading that it should. The tell, before either of
  those: the same callee, entered by the same symbol, needed a fact in one case and not in the
  other, and what differed was only who wrote the call.

- **Let the empty list stand for both a call with no arguments and a call nobody computed, and have
  each backend cope.** The shape that was in place. Rejected under D3: the C++ backend coped by
  composing a call with the wrong arity, which its host compiler rejected, and the execution backend
  coped with a refusal that could not see the difference either.

- **Fill a missing default from the declaring class's scope, so the list is complete even there.**
  The right end state, and what removes the one refusal that remains. Left out because a default is
  an expression of the base's own scope: carrying it means deciding how an expression crosses a unit
  boundary and where it is evaluated, which is a subject rather than a step. Swift compiles each
  default into its own named function for exactly this reason, which is the shape to survey first.

- **Give LIR a call target of its own for a cross-unit constructor.** Rejected because the existing
  one already means "a function this unit does not compile, reached by its linkage name"; a second
  spelling of that would describe the same call twice.

## Consequences

- A class a package declares is constructed and initialized on the execution backend as one this
  unit declares is, and the two backends agree again on a program that reached neither's refusal.
- A class extending another unit's class enters its base's construction, and so does one extending a
  class of its own unit whose arguments were written on the extends specifier -- those arguments had
  been dropped, silently on one backend and as uncompilable output on the other.
- What a base construction carries is stated once, where the class is read. Both backends receive a
  complete list or never see the class, and neither carries a rule the other does not.
- One refusal remains and it is the same in every naming: a base constructor formal left to its
  default value. What it waits on is a default value crossing a unit boundary, not anything about
  classes.
- A unit that constructs another unit's class, reaching nothing on it, records a dependency on that
  unit through the symbols it names and not through a promise it consumed.
- The word qualifying a constructor's symbol is composed in one place. A class's own unit and every
  unit entering its construction read it from there.

## Cross-references

- [entering-a-class-construction](entering-a-class-construction.md) -- allocation and construction
  as two operations, and the survey behind it; D2 there is what this applies across the unit
  boundary.
- [reaching-past-a-published-class](reaching-past-a-published-class.md) -- what a published class
  promises, which D3 here adds one property to, and the rule D4 here obeys.
- [unit-signature](unit-signature.md) -- D3 there, that a signature's field list is exactly the
  facts a unit's lowering reads about another unit, which is the test both D2 and D3 here answer to.
- [calling-a-subroutine-on-another-units-object](calling-a-subroutine-on-another-units-object.md) --
  the cross-unit call whose naming rule D1 here follows.
- [class-declared-in-a-structural-scope](class-declared-in-a-structural-scope.md) -- why a published
  class never carries a declaring instance, so its construction has one shape.
