# Reaching Past a Published Class

## Date

2026-09-10

## Status

Accepted. Extends `unit-signature.md` D2 with what a published class promises about its lineage, and
brings the consumed-signature set into line with `../architecture/incremental_build.md` invariant
11; reverses neither. D3 here supersedes `publishing-an-owned-instance.md` D3, which bounded what a
lowering may read: that bound is removed rather than widened a second time.

## Why this decision matters

A referrer must be able to name anything a legal program can reach on another unit's class, and most
of what a program reaches is inherited. `pkg::Derived` answers behaviors `pkg::Base` introduced and
carries properties `pkg::Base` declares, and a unit holding a `pkg::Derived` handle writes `d.tag()`
and `d.value` without knowing or caring which class in the lineage those came from.

So the referrer has to turn a source name into a coordinate that names some class. Which class it
names is the decision: the one the source wrote, or the one that actually declares the thing. The
two differ only for inherited members, which is to say almost always, and the difference decides
whether the declaring class's unit is a dependency of the referrer at all.

Getting it wrong is not loud. A coordinate naming the wrong class still compiles, still links, and
answers with whatever sits at that position in some other class's layout.

## The tension this addresses

Two constraints pull in opposite directions, and a third settles it.

- **A promise cannot restate what a class inherited.** Computing what `pkg::Mid` answers means
  reading what `other::Base` promised, and `incremental_build.md` invariant 8 forbids a signature's
  derivation from reading anything outside its own unit -- the property that leaves the signature
  stage unordered, with no dependency graph and no cycle to detect in front of it.
- **A referrer cannot count through what it may not read.** The members and behaviors a class keeps
  to itself still occupy positions, so a unit that counted past them would be reading what it was
  not told.
- **What settles it:** a promise may name the class it extends. That is a fact about the class
  itself, so deriving it reads nothing outside the unit -- and it gives the referrer somewhere to
  go.

## Decisions

### D1. A published class promises what it declares and the class it extends, never what it inherited

The properties it declares, in the order that fixes their slots; the behaviors it introduces, in the
order that fixes their ordinals; the class it extends, named by declaring unit and canonical name;
and whether it is an interface class. Nothing that came from an ancestor appears, because the
promise is derived from the unit's own declarations and an ancestor's may be elsewhere.

Whether it is an interface class is on the promise for the same reason its name is: a class commits
to an interface rather than extending it, so a behavior an interface class states sits on no lineage
and has no position counted through one (LRM 8.26). A referrer that could not tell would build a
coordinate no value carries.

### D2. A referrer resolves anything inherited by walking that chain, and the coordinate names where the walk landed

A property is named by the class that declares it and the slot that class gave it; a behavior by the
class that introduced it and which of that class's introductions it is. Both are found by starting
at the class the source wrote and following what each promised about the class it extends.

The coordinate therefore often names an ancestor of the class the source named, which is what makes
it an identity rather than a route: every class extending the introducer answers the same behavior
under the same name (LRM 8.20), and which storage an access reaches is fixed by the class the access
names rather than by the object it runs on (LRM 8.14). The class the source happened to write is
neither of those.

### D3. Reading a promise is what records the dependency, so the walk makes a transitive dependency real

A unit that reaches a behavior introduced two units up depends on the unit that introduced it, and a
change to what that class introduces re-emits it. This is not a cost the design tolerates; it is the
dependency being what it is. The referrer's emitted coordinate names that unit's class, so the two
are not independent and a graph that said otherwise would be wrong.

It follows that the set of promises a unit consumed cannot be decided before its bodies lower. A
class first met inside a body -- a local whose type is another unit's class -- is reached after any
such set is fixed, and a name is on a promise or not whether or not the reader declared anything in
advance. `incremental_build.md` invariant 11 already states the rule this obeys: a unit's dependency
on another is exactly the set of signatures it consumed, enumerable rather than discovered.

## Where other systems put this

Two established answers, and they are opposites, which is what makes our own condition worth
stating.

**Java records the class the source wrote and resolves the chain at run time.** A class file's
`CONSTANT_Methodref` names the class at the call site; JVMS 5.4.3.3 then "attempts to locate the
referenced method in C and its superclasses ... step 2 of method resolution is recursively invoked
on the direct superclass of C". The client's artifact never names the introducer, so the
introducer's class file is not a dependency of it.

**Objective-C's non-fragile ABI records the declaring class and lets the runtime place it.** An
instance variable is reached through a per-ivar symbol `_OBJC_IVAR_$_ClassName.IvarName`, and for an
inherited ivar the symbol is the declaring class's, not the subclass's -- so the declaring class's
image is a link dependency of the client, while the offset itself is filled at image load and a
private addition to the base moves nothing. The same ABI publishes a symbol only for what a client
may name: `@private` and `@package` ivars export none, `@public` and `@protected` do.

**Where our conditions differ, and it is one sentence:** Java resolves by name because its class
files are compiled at different times and may be replaced between compile and load, so the
introducer must not be baked in -- while our units are compiled and linked by one session against
promises derived in that same session, and nothing can be replaced in between. The independence
Java's by-name resolution buys is independence from a change that, in our model, should re-emit the
referrer. So the Objective-C split is the one available to us: the coordinate names the declaring
class, and the position is fixed when that class is realized.

## Rejected alternatives

- **Flatten at publish: the promise lists everything reachable, each naming its declarer.** The
  referrer then needs no walk and no transitive dependency. Rejected because deriving it requires
  reading the base's promise whenever the base is elsewhere, which is exactly what
  `incremental_build.md` invariant 8 forbids -- and that invariant is load-bearing rather than
  stylistic: it is what leaves the signature stage with no order among units, so giving it one would
  put a dependency graph and a cycle diagnostic in front of the one stage that has neither, and
  every later stage would inherit the ordering.

- **Name the class the source wrote and resolve the chain at elaboration, as Java does.** The
  referrer stays independent of the introducer's unit, and adding a behavior to a base high in a
  lineage re-emits nobody. Rejected because the interaction is real whether or not it is declared: a
  unit calling a behavior `other::Base` introduced depends on `other`, and a dependency the graph
  does not have is invisible until a cached result is reused, at which point it is a stale result
  rather than an error. It also buys an independence our compilation model does not need, since
  nothing is replaced between compile and link.

- **Keep the set of consumed promises pre-computed from the unit's declarations.** It bounds what a
  lowering may read, which reads like the stronger property. Rejected on two counts. It cannot be
  complete: a class first named inside a body arrives after the set is fixed, so its promise is
  absent and the reference is refused for a reason that is not about the program. And the absence
  was standing in for a different question -- whether the target published the name, which decides
  between compiling against a promise and resolving during elaboration, and which does not depend on
  who is asking.

  `publishing-an-owned-instance.md` D3 widened that bound rather than dropping it, and rejected
  dropping it because with it goes "the property that a unit cannot read a signature unrelated to
  anything it declares". Two things answer that rationale. The bound is over reading, while the
  theorem that makes a stale artifact impossible is `unit-signature.md` D3's, stated over what a
  lowering _consumes_ -- a promise read and not used records nothing and changes no output, so the
  bound guards nothing the theorem rests on. And the widening it chose closes over the units a
  signature names as published objects, which reaches instance nesting and cannot reach a type named
  only in a body; reaching those means walking the bodies in the declaration pass, which the same
  entry rejects one alternative later for deciding with less information what the later pass
  discovers anyway. The two rejections meet once a class type crosses, and the bound is what gives.

- **Publish every property in declaration order, including the ones a class keeps to itself.** Then
  no ordering rule is needed and both sides count the same list. Rejected because a `local` property
  (LRM 8.18) declared ahead of a public one would move it, so adding one re-emits every referrer and
  a stale one reads the wrong storage -- the fragile base class problem, inside a single class. What
  a class publishes sits in a fixed prefix of its own storage instead, exactly as a unit's published
  members sit in a prefix of its object.

## Consequences

- A unit that reaches a property or a behavior introduced further up a published lineage consumes
  that unit's promise and depends on it. A change to what that class declares or introduces re-emits
  the referrer; a change to what it keeps to itself re-emits nobody.
- The coordinate a referrer emits names the class that declares or introduces the thing, so it is
  the same coordinate whichever class the source reached it through, and two units reaching one
  behavior through different intermediates emit the same identity.
- A behavior an interface class states is refused rather than given a lineage position, uniformly,
  whether the interface class is this unit's or another's. Before the promise carried what a class
  is, the cross-unit case could not be told apart and answered with whatever sat at that position.
- Nothing decides in advance which promises a unit may read. What it read is what it depended on.

## Cross-references

- `unit-signature.md` -- what each unit kind publishes, and the two ways a reference reaches into
  another unit; D1 here fills in what one published class carries.
- `dispatch-position-is-a-lineage-coordinate.md` -- a behavior is named by the declaration that
  introduced it and an ordinal within it; this entry is how a referrer finds that declaration when
  it is in another unit.
- `published-member-placement.md` -- the position is the promise's order, computed on both sides and
  carried by neither, which D2 applies to a class's own members.
- `publishing-an-owned-instance.md` -- the bound on what a lowering may read, which D3 here removes;
  the rest of that entry, including the demand-driven record, is untouched.
- `../architecture/incremental_build.md` -- invariant 8 on what deriving a signature may read, and
  invariant 11 on consumption recording a dependency.
- `../architecture/compilation_unit_model.md` -- the unit boundary and the signature as its only
  cross-boundary surface.
