# The Signature of a Compilation Unit

## Date

2026-08-26

## Status

Accepted. Widens the segment classifier of `hierarchical-reference-routing.md` D2 and
`front-end-semantic-boundary.md` D3; neither is reversed.

## Why this decision matters

A unit compiles against what the units it references promise. That promise was written as "name and
signature (parameters and ports)" -- the right idea with a module-shaped set. Two workstreams have
already had to work around it: a package's promise is its declarations rather than its ports, and an
interface's promise is its members, which is the entire reason the construct exists.

The cost is not only wording. Every cross-unit access to a signal is realized as a by-name lookup
during elaboration, including access to a name the target unit published and to which the referrer
already holds a typed handle. A referrer that renames nothing still compiles; the failure surfaces
at elaboration instead of at compile time, and an unchecked cast stands between the lookup and the
use. Meanwhile the same compiler already reaches a package callable, a package variable, a class
method, and a static property across the unit boundary by name resolved where the referrer compiles.
An instance's data member is the one member of that family left out.

This entry fixes what a unit publishes, how the answer is known to be complete, the two ways a
referrer reaches into another unit, and what each makes recompilable.

## The model

A unit publishes a **signature**: the set of declarations other units may name. Two properties
follow from a signature being a promise rather than a description.

- It is **derived by the unit from its own contents**. A separately authored signature can disagree
  with the unit; a derived one cannot.
- It is **the whole of what a referrer sees**. If a referrer can reach past it, the promise is not
  the boundary.

What a signature contains is a property of the language, read off each unit kind, not a choice the
compiler makes.

SystemVerilog admits reaching past a signature: a hierarchical reference names a declaration the
target unit never published (LRM 23.6). So there are exactly two ways into another unit, and which
one applies is decided by whether the target published the name -- a question the target answers
about itself.

The term is the one the compilation-unit contract already used for this content, and it extends the
existing `CallableSignature`: what a caller must know about a callable, and what a referrer must
know about a unit, are the same kind of fact at two scales.

## Decisions

### D1. A unit's signature is what its kind exists to publish

| Unit kind            | Signature                                   |
| -------------------- | ------------------------------------------- |
| module               | its ports                                   |
| package, and `$unit` | its declarations (LRM 26.2)                 |
| interface            | its ports and its members (LRM 25.2, 25.10) |

A `modport` is a named, direction-carrying subset of an interface's signature (LRM 25.5). Selecting
one narrows which names a referrer may use and in which direction; it changes nothing about how a
name is reached, and it never restricts hierarchical access (LRM 25.10).

An interface publishes its members because that is what the construct is for: a named bundle other
units are written against. A module publishes no internal declaration for the same reason in reverse
-- a module exists to be instantiated and wired, and its internals are its own.

**The module row is derived, not enumerated.** LRM 23.2.1 states what a module header defines:

> the name of the module; the port list of the module; the direction and size of each port; the type
> of data passed through each port; the parameter constants of the module; a package import list of
> the module; the default lifetime (static or automatic) of subroutines defined within the module.

Two filters reduce that list, and both have reasons rather than preferences:

- **Drop what faces inward.** A package import list decides how names resolve inside the body, and a
  default lifetime governs subroutines defined within the module -- LRM 23.2.1 says so in the item
  itself. An instantiator observes neither.
- **Drop what specialization already consumed.** Distinct parameter bindings are distinct
  specializations with distinct identities, so a parameter's value reaches a referrer through the
  unit's identity rather than through its signature. A changed default yields a different
  specialization, so the dependency still holds.

What survives is the name plus, per port, its direction, size, and data type. A module header is
therefore the source-level shape of the same idea, and the right way to explain a signature to a
SystemVerilog reader -- but it is not the same set, which is why the compiler's term is not
"header".

### D2. A signature is two-level: the unit, then each class it publishes

A cross-unit reference already names its target as a unit, optionally a class within it, and a
member within that. A signature is organized the same way, because that is the shape the reference
vocabulary already has: an entry per published class, each carrying its own members, methods, and
type-associated storage, beside the unit's own namespace-level declarations.

A published class carries **its own name**. Which class a referrer instantiates is stated by the
signature, never inferred from a class whose name matches the unit's or from a position in a list. A
unit's name, the name of the class it builds, and where a backend places the emitted code are three
separate facts; collapsing any two of them makes one declaration mean different things at different
reference sites.

### D3. Completeness is a property of the consumer, not of a checklist

A field is missing from a signature in one of two ways, and only one of them is visible.

- A referrer that needs the fact to emit at all **cannot emit**. Loud, and self-correcting.
- A referrer that needed the fact only to stay correct **keeps its stale output**, because the
  signature it hashed did not change. Silent, and it is exactly a flaky incremental build.

The second is not fixed by a more careful list. It is fixed by making omission impossible to
express:

> If a referrer's emission is a pure function of its own IR and the signatures it consumes, then "a
> fact absent from every signature cannot invalidate a referrer" is a theorem rather than a hope.

So the enforcement is the lowering's inputs. A lowering that can reach another unit only through its
signature cannot depend on anything else, and the signature's field list is then defined rather than
guessed: **it is exactly the set of facts a unit's lowering reads about another unit**, which is
enumerable by inspection instead of by recall.

The window matters. Before compiled units are cached, a missing field is the loud case, because
nothing is reused. Once they are cached, the same omission becomes the silent case. The purity
property must therefore hold before caching lands, not after.

### D4. A reference to a signature member is named where the referrer compiles

A referrer that names a member of another unit's signature carries the target unit, the class, and
the member's name, resolved against that unit's signature at the referrer's compile time. The
reference reaches the member's own access protocol, exactly as an intra-unit member reference does.

This is the shape the compiler already uses for every other cross-unit reference: a package callable
and a package variable named as `unit::name`, a class method reached through a receiver, a static
property named as `unit::Class::name`. An instance member reached through a receiver joins that
family and introduces no vocabulary of its own.

Naming is not a weaker identity here. A name resolved against a declared signature at compile time
is checked by the compiler that consumes it; a name resolved against a run-time registry is not. The
two share a spelling and nothing else.

### D5. A reference past a signature resolves by name during elaboration

A hierarchical reference to a declaration the target unit did not publish has no signature to
compile against. It carries the name, resolves once during Resolve through the runtime SDK, and
seals. This is not a weaker realization of D4 -- it is the only realization available, because the
target promised nothing.

The two are a total classification of cross-unit references, with no third case and no case a
consumer must decide by inspection.

### D6. A signature is an artifact, separate from the unit's code

A unit specialization produces two artifacts: its **signature**, derived from its declarations
alone, and its **code**. Deriving a signature requires no body lowering, so it completes long before
the code does, which is what lets a unit's referrers compile while its own code is still being
emitted.

Emitting unit U requires U's own MIR, the signatures of the units U references, and the runtime SDK.
U's artifact never consumes another unit's code, and never consumes a signature belonging to a unit
U does not reference.

A target-language header is one backend's spelling of a signature, not the signature itself. The C++
backend renders one as a declaration-only header and borrows that language's name resolution, member
placement, and linking; a backend that emits machine code reads the signature directly and performs
all three itself. A backend that renders bodies into the same file it renders the signature into has
published its code as its promise.

### D7. What a change re-emits follows the relation, not the file

| The referrer's relation to unit U | What it consumes of U | What re-emits it |
| --------------------------------- | --------------------- | ---------------- |
| instantiates U                    | signature and layout  | either           |
| holds a handle to a U instance    | signature             | signature        |
| names a U namespace declaration   | signature             | signature        |
| reaches past U's signature        | nothing               | nothing          |

A change confined to a unit's bodies changes no signature and re-emits no referrer. A change to a
signature re-emits every unit that consumes it, which is the dependency being real rather than the
mechanism being coarse.

The last row is the honest price of the escape hatch: a reference past a signature survives any
change to the unit it reaches, including one that removes its target, and reports the failure at
elaboration. That is what a reference to something never promised can offer.

## Shape

A signature carries what a referrer must know and nothing that would re-emit it needlessly. The
published members are ordered, and that order is what fixes their placement.

```text
UnitSignature   { unit name; published classes; namespace-level callables and variables }
ClassSignature  { its own name; published members in declaration order; methods; static storage }
```

The content of each entry is settled by D3 rather than by enumeration: it is what a referring unit's
lowering reads about the unit it references. Applied to the cross-unit references the compiler
already forms, that yields more per entry than a reader would name unaided.

- A **port** carries its external name and one or more parts. The external name is what a referrer
  connects to and the LRM lets it differ from the name of whatever the port reaches inside the unit,
  so what is published is the port and never the declaration behind it. A part is a point a
  connection reaches individually, and each carries a direction, the type of what crosses, the
  default an omitted connection materializes (LRM 23.2.2.4), and which published member of the
  unit's object it reaches. Each of those changes what a referrer emits.

  A part names its member rather than restating what that member is. Whether the storage is a net
  and under which net type is a property of the storage, so it is stated once, on the member, and a
  connection reads it there -- otherwise a referrer that wants it reaches past the port into the
  declaration behind it, which is the one thing the external name exists to prevent. The part's own
  type stays separate because the two genuinely differ: a port expression (LRM 23.2.2.2) connects
  part of an internal name, so what crosses is narrower than what it lands on, and a part whose type
  is not its member's whole type has a projection standing between them.

  Parts exist because a port may bundle several internal names (LRM 23.2.2.1), which carry data
  under directions that need not agree -- the port's own direction is only the most restrictive of
  theirs and says nothing about which way any one of them runs, so a direction belongs to a part and
  not to the port. A port bundling nothing has exactly one part, which is that shape with one entry
  rather than a case of its own. A part whose direction is absent is an interface port, which names
  a scope rather than carrying data (LRM 25.3).

- A **namespace-level callable** carries its name, its result type, whether it is a function or a
  task -- the call protocol, since a task enable suspends its caller (LRM 13.3) -- and per formal a
  direction and a type, which is what shapes the marshalling an output or `ref` actual rides back
  through.
- A **namespace-level variable** carries its name and the type of its one program-global cell.
- A **published class** carries its canonical (specialization) name; per method a name, a result
  type, its formals, and whether it is virtual (LRM 8.20), which decides whether a call site
  dispatches statically; per property and per type-associated cell a name and a type.

Member placement follows one rule stated once, below the execution IR: **a published member sits in
a fixed prefix of its object, ahead of everything the unit did not publish.** Producer and consumer
then derive the same placement independently from the same signature, and a declaration a unit never
published cannot move one that it did.

## Forbidden shapes

- A signature authored beside a unit rather than derived from it, or one a unit's code can
  contradict.
- A signature defined as one fixed set of declaration kinds for every unit kind. The set is per
  kind, read off the language.
- A field admitted to a signature because a reader named it, rather than because a lowering reads it
  about another unit. The consumer's inputs define the set (D3).
- A lowering that reaches a fact about another unit through anything other than that unit's
  signature. It is the leak that makes D3's theorem false, and after caching lands it is a stale
  build rather than an error.
- A published class identified by name equality with its unit, or by its position in a list. The
  signature states which class a referrer instantiates.
- An artifact that publishes a unit's bodies as part of its signature, so that editing a body
  re-emits the unit's referrers.
- A run-time by-name lookup used to reach a name the target unit published. The name is on the
  signature, so it resolves where the referrer compiles; the lookup pays for an independence the
  declared dependency has already spent.
- An unchecked cast standing between a cross-unit lookup and the use of its result. A signature
  member carries its type; a target that did not publish the name cannot be reached typed at all, so
  the cast has no honest form in either case.
- A modport treated as a runtime entity, a distinct handle type, or a second reach mechanism. It
  narrows a signature and nothing else.
- A design-wide index, ordinal, or numbering standing in for a signature member's identity. Producer
  and consumer derive the signature independently, so its members are identified by name, which both
  derive from the same source.
- A referrer's artifact naming a unit's internal declaration -- a member the unit did not publish,
  an internal type, a child it owns. Reaching those is D5's by-name form.

## Consequences

- `compilation_unit_model.md`'s cross-boundary surface is the unit's signature, per unit kind, not
  "parameters and ports".
- `reference_resolution.md`'s per-segment classifier is whether the segment's target is on a
  signature the referrer consumes, not whether the emitting artifact owns both classes. The previous
  pair of definitions did not tile the space: a segment crossing into another unit but stopping on
  its signature matched neither.
- `emission_model.md` gains the signature as an emitted artifact distinct from code, and its list of
  reference relations gains the one that holds a handle to another unit's instance.
- `mir.md` gains a reference to a signature member reached through a receiver, completing the family
  its four other cross-unit reference kinds already form. A textual name remains forbidden as the
  identity of an in-artifact segment, and remains correct for a signature member, where it is the
  identity both units derive.
- The interface port needs no vocabulary of its own: its member is a borrowed handle to an
  external-unit object, and a member access through it is D4.
- The by-name machinery narrows to D5's escape hatch. Every reference on a signature loses a
  registry entry, a lookup, a stored endpoint, and a cast.

## Cross-references

- `../architecture/north_star.md` -- independent and parallel compilation as first-class
  constraints, and end-to-end iteration time as the target D6 and D7 serve.
- `../architecture/compilation_unit_model.md` -- the unit boundary and the signature D1 defines.
- `../architecture/reference_resolution.md` -- the route and per-segment classification D4 and D5
  refine.
- `../architecture/emission_model.md` -- the artifact rules D6 extends.
- `../architecture/lir.md` -- logical storage topology at the execution IR, with placement derived
  below it, which is where the prefix rule lands.
- `hierarchical-reference-routing.md` -- D2 there classifies segments per segment rather than per
  lexical form, which this entry keeps; only the classifier's definition widens.
- `front-end-semantic-boundary.md` -- D3 there states the classifier operationally over slang scope
  kinds, which this entry widens the same way.
- `specialization-identity.md` -- a specialization's identity, computed independently by producer
  and consumer, which is the same independence D1 requires of a signature and the reason a parameter
  is not on one.
