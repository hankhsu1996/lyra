# An Identity Is Never a Rendering

## Date

2026-08-28

## Status

Accepted. Sharpens `specialization-identity.md` and `cross-unit-class-translation.md` on one point
each; reverses neither.

## Why this decision matters

Three separate defects turned out to be one. A specialization key that silently compiled two
different classes as one artifact. A cross-unit class whose emitted type name and emitted include
disagreed, so a legal program failed to build. A pointer whose pointee was resolved into a layout
the referrer had no dependency on. In each, something that had to **distinguish** was stored as
something that had been **spelled**.

The failures share a shape that makes them expensive to find: a rendering that is wrong is loud,
because it is read by a compiler or a person. A rendering standing in for an identity is silent,
because it renders fine -- it only fails to tell two things apart, and nothing looks at it to check
that. The specialization defect produced a wrong simulation result with no diagnostic anywhere.

## The tension this addresses

A rendering and an identity are both strings, and for most values they are the same string, so
storing one and using it as the other costs nothing and reads as a simplification. It stops being
free at exactly the point where a name is ambiguous: two classes named `Box` in two packages, a unit
name carrying a character the target language does not admit, an object named without its layout
being known.

Pulling them apart looks like ceremony at every individual site. What settles it is that each
carries a different contract. A rendering must be readable in one target and is allowed to drop
whatever that target recovers by other means -- scope, mangling, structure. An identity must
distinguish, everywhere, forever, and may drop nothing. A producer that has an identity and stores a
rendering has thrown away the part it did not need _yet_.

## Decisions

### D1. What must distinguish is stored as its parts, never as a composed name

Wherever a fact identifies something -- a class of another unit, an argument a specialization was
bound to, an object a pointer points at -- the parts that identify it are what is stored. Composing
them into a name is the last step before the name is used, done by whoever knows the spelling rules,
and never earlier.

Concretely, a class another compilation unit declares is the pair (declaring unit, class name), the
same pair every other cross-unit reference already carries; it is not the string `unit::class`. A
lowering that composes that string has decided a target language's spelling in a layer that has no
target, and any transformation the real backend applies -- mangling a character the target forbids
-- is then applied to some occurrences and not others. That is a defect no test of the composed
string can see, because the string is correct; what is wrong is where it was made.

### D2. An identity distinguishes exactly what the layer below it distinguishes

A key that identifies a compiled artifact must separate two inputs whenever the layers below would
compile them differently, and is free to merge them otherwise. That is a checkable statement, and it
is the one to check: not "does this encoding look canonical" but "does it split exactly where the IR
splits".

The IR's own type identity is the reference. Lyra's types are structural -- two identical packed
structs from two packages are one type and correctly share one artifact -- with one exception: a
class is identified by its declaration (LRM 8.3), so its identity is the pair from D1. A
specialization key therefore reproduces the frontend's structural rendering, which is faithful about
shape, and supplies the declaring unit of every class the argument reaches, which is the one thing
that rendering drops.

The rule generalizes without re-deriving: when the IR gains a second nominal type, the key gains its
identity too, and the question to ask is always "where does the layer below split".

### D3. Naming something and holding its contents are two facts, and the vocabulary spells both

A referrer names another unit's object when it points at one, and holds what that unit published
when it reaches a member on one. The two are different, and the second implies a dependency the
first does not: holding what a unit published means having consumed its signature, which is what a
declared dependency is.

So the IR distinguishes them, and the type that carries members is reachable only where the members
were consumed. A published member that the referrer does not reach through contributes its
representation -- a pointer is one machine word -- and not the identity of what it points at.
Otherwise a referrer names units transitively without bound, pulls their artifacts, and re-emits
when they change, none of which its own output depends on.

### D4. A conflated arm is split, not disambiguated by inspection

Where one variant arm was serving two concepts -- a class of the runtime library and a class of
another unit, told apart only by which string the producer happened to supply -- the arm is split
into one per concept. The alternative, a consumer that inspects the payload to work out which case
it has, puts the producer's knowledge in every consumer and is wrong the first time one of them
forgets.

## Rejected alternatives

- **Apply the target's name mangling where the qualified name is composed.** It fixes the observed
  build failure in one line. Rejected because it moves a second backend's spelling rules into a
  lowering that has no backend, and the next target inherits the first one's mangling.

- **Keep one arm and tag it.** A tag beside a string is the same shape as the string alone once a
  consumer forgets to read the tag, and nothing makes it read it. Per-kind arms make the invalid
  case unspellable and break every consumer until each says what it means.

- **Encode a specialization argument by converting it to the IR's own type and hashing that.** It is
  the most direct reading of D2. Rejected because the conversion belongs to a unit and needs that
  unit's pools and class registry, while a specialization must be named before any unit is lowered
  -- both the unit naming itself and the parent naming it compute the name from the frontend alone.
  Reproducing the conversion outside a unit would be a second type translation with no consumer to
  keep it honest.

- **Refuse an argument whose identity cannot be stated exactly.** Loud rather than silent, which is
  the right instinct. Rejected because it is unnecessary once D2 is applied: the encoding has to
  split where the IR splits, and it does, so there is nothing left to refuse.

- **Record every unit reachable through another unit's published members.** The shorter fix for D3's
  failure. Rejected because it makes a referrer depend on units its own output is independent of,
  which is both a forbidden artifact dependency and a dependency that is not real -- the change that
  invalidates it changes nothing the referrer emits.

## Consequences

- A specialization key splits exactly where the IR does. Two classes of the same name in different
  packages are two artifacts; two structurally identical packed structs remain one, which they
  always should have been.
- A specialization key is a value with its parts, and a name is what it renders to. Equality never
  goes through bytes, so a delimiter cannot decide whether two units are the same; and the set of
  things a parent may fix is a closed one, so adding another breaks every reader until each says
  what it means -- which is the check that was missing when the interface axis was added and the
  type axis silently disagreed.
- A backend composes every cross-unit name it emits, so a unit whose name carries a character the
  target forbids emits a consistent include and type reference. The `$unit` file-set scope is the
  case that exposed this, and a class declared there now compiles.
- A referrer's artifact names the units it reaches into and no others, so what a change re-emits
  stays the set that actually consumed it.
- The layer a name is composed in is the layer that knows the target, which is what lets a second
  backend spell the same identity differently without a lowering changing.

## Cross-references

- `specialization-identity.md` -- the identity function D2 constrains, and the independence that
  makes producer and consumer agree.
- `cross-unit-class-translation.md` -- where a reference to another unit's class is classified; D1
  is what that classification carries from there down.
- `unit-signature.md` -- consuming a signature as the act that declares a dependency, which is what
  D3 keeps a naming pointer from doing.
- `interface-port-binding.md` -- the port whose member exposed D3, and the unit identity D2 widened.
- `../architecture/emission_model.md` -- what one unit's artifact may depend on, and the by-name
  cross-unit access both D1 and D3 rest on.
