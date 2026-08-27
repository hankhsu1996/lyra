# A unit's signature as an artifact

Tracks making a compilation unit's signature a first-class artifact, separate from the unit's code,
and routing every reference that lands on a signature through it rather than through a name the
runtime answers while the design elaborates.

Done when a unit's signature is derived from its declarations alone and emitted apart from its
bodies; when a referrer consumes only the signatures of the units it references; when both backends
realize a reference to a signature member as a direct member access; and when a change confined to a
unit's bodies re-emits none of its referrers.

## Contracts

This workstream reasons from these and does not restate them:

- `../decisions/unit-signature.md` -- what each unit kind publishes and how that set is known to be
  complete, the two ways a reference reaches into another unit, the signature as an artifact, and
  what a change recompiles.
- `../architecture/compilation_unit_model.md` -- the unit boundary and the signature each kind
  publishes.
- `../architecture/reference_resolution.md` -- routes classified per segment by whether the referrer
  has a declaration to compile against.
- `../architecture/emission_model.md` -- two artifacts per unit specialization and the inputs one
  unit's emission may depend on.
- `../architecture/lir.md` -- logical storage topology at LIR; physical placement derived below it.

## Why the artifact has to come first

The two halves cannot land in the other order, and the reason is not preference. A referrer that
names a signature member needs, at the point it compiles, whatever fixes where that member sits. One
backend can get that from the target language: the referrer already includes the declaring unit's
emitted header, so its own compiler resolves the name and places the member. The other backend
lowers each unit against that unit alone, by design, and therefore has nothing to resolve the name
against -- so it cannot realize a member access at all until a signature it can read exists.

Turning the reference typed for one backend alone makes the other refuse every design carrying a
port connection. The signature artifact is what lets both realize the same reference, which is why
it is the first sub-step rather than a later one.

## Only a self-contained fact can be published

A signature is read by a unit that shares no arena with the one that produced it, so every fact on
one has to mean something without the producer's storage. A name and an enumerated property do; an
identity that indexes the producer's own pool does not, and neither does an expression held in the
producer's tree. That is what divides the sub-steps below into the ones that only need the fact
moved and the ones that first need a representation it can cross in.

A type is the first fact that needed one. It crosses by the signature carrying its own pool, so an
identity on a signature indexes storage the signature holds; the consuming unit takes the structure
in and answers with its own identity for it. What that in turn required is that a unit's pool decide
identity by structure rather than by a key borrowed from the frontend -- otherwise a type arriving
from a signature becomes a second entry beside the one the reader already had.

It also explains why the frontend has been the carrier all along: its elaborated graph is the one
representation both units share, so a lowering that wanted a fact about another unit had nowhere
else to read it from. That graph is also design-global and dies with the frontend object, which is
why nothing a signature states may rest on it.

## Sub-steps

- [x] S1 -- Every unit's signature is derived before any unit's body is lowered, and a unit's body
      lowering reaches another unit's published declarations only through the signatures it is
      handed. Deriving one reads nothing outside its own unit, so nothing orders that pass and no
      cycle among units can arise. A unit that publishes nothing yields an empty signature rather
      than none.
- [x] S2 -- A published port states its external name and its parts, each part carrying the
      direction data flows across it, including whether writing through it is permitted. The
      external name is what another unit connects to and the LRM lets it differ from the name of
      whatever the port reaches inside the unit, so what is published is the port and never the
      declaration behind it. A port bundling several internal names (LRM 23.2.2.1) has a part per
      name, since their directions need not agree; one bundling nothing has exactly one part.
      Consuming a signature walks its parts in step with the connections an instance makes rather
      than searching for each, so the two cannot disagree about which point is which.
- [x] S3 -- A type crosses a unit boundary on its own terms: a signature carries the types of what
      it publishes as a self-contained graph, and a unit that consumes one takes those types into
      its own pool. Producer and consumer then agree on a published type without either reading the
      other's storage or re-deriving it from the frontend. A port's type crosses this way; a
      callable's formals and result, and a published member's type, cross the same way once those
      are published.
- [ ] S4 -- A published port states the storage it reaches: whether that storage is a net and under
      which resolution its drivers combine. A net type the compiler does not model is still
      published as a net, since the unit declared one, and the consumer that cannot realize it says
      so. What consumes this is not the refusal at the connection -- the declaring unit's own walk
      refuses a net type it cannot model anyway -- but the route a reference to that port's storage
      carries: a route states the cell it ends at, and for a cross-unit target the net half of that
      is read off the other unit's declaration today. So this sub-step lands in the routed-reference
      recipe, beside the leaf type it already carries, rather than at the connection.
- [ ] S5 -- A published port states the default value an omitted connection materializes (LRM
      23.2.2.4). The default is written in the declaring unit and resolves there, so what crosses is
      the value it folded to rather than the expression that produced it. Publishing the value is
      the easy half; the consumer also has to know that this connection was omitted, which is a
      property of the connection rather than of either unit, and the frontend does not expose it --
      so a consumer recovers it by comparing the connection's expression against the declaring
      unit's initializer. Closing that needs the frontend to answer "was this connection written",
      which is the first place this workstream wants something the frontend does not offer.
- [ ] S6 -- A unit's lowering is handed the signatures of the units it references, not every
      signature in the design. Both are pure, so the purity property holds either way; what only the
      narrowed form gives is that a unit cannot consume a signature it never declared a dependency
      on. The dependency itself is already recorded -- an instance member names the unit it is built
      from -- so this narrows what the lowering can reach to what its own declarations already say,
      rather than introducing a record of it.
- [ ] S7 -- A signature is organized as the unit's namespace-level declarations beside an entry per
      published class, each carrying its own name and its published members in declaration order.
      Which class a referrer instantiates is stated rather than inferred from a name that matches
      the unit's or from a position in a list.
- [ ] S8 -- A reference whose target is on a signature the referrer consumes carries the declaring
      unit, the class, and the member's name, and is a member access on a receiver that names that
      unit. A reference past a signature keeps the by-name form the runtime answers during
      elaboration. Which of the two a route's leaf takes is settled where the step reaching the
      target's owner is settled, because a route that has already left this unit's layout has no
      declaration to compile either the step or the leaf against.
- [ ] S9 -- The machine-code backend resolves a signature member's position by translating the
      signatures of the units it references into its own type graph, so a cross-unit member access
      is an ordinary member step and the layer below it derives the placement. A published member
      sits in a fixed prefix of its object, so producer and consumer derive the same placement
      independently. Cross-unit member access reports an unsupported diagnostic until this lands,
      never a compiler-bug failure.
- [ ] S10 -- The C++ backend emits a unit's signature as a declaration-only artifact distinct from
      the artifact carrying its bodies, and a referrer consumes only the first. The published prefix
      is expressed so the target language guarantees the same placement rule S9 states. A change
      confined to a unit's bodies re-emits no referrer.
- [ ] S11 -- A referrer that only holds a handle to another unit's instance consumes that unit's
      signature and nothing more. Constructing an instance reaches the declaring unit's own entry
      point rather than requiring its full layout at the instantiation site, so a declaration the
      unit does not publish never re-emits a referrer either.

## Out of scope

- Compiling and linking per-unit artifacts separately. The design still compiles as one translation
  unit; the signature split is what makes the separation possible, not the separation itself.
- Caching compiled units across runs. A signature is the key such a cache would need, so this
  workstream produces its input, but the cache is its own subject. What makes such a cache sound is
  that a lowering reaches another unit only through its signature (S1), and every fact still read
  around one is a hole in it -- so the sub-steps that move the remaining facts must land before a
  cache exists rather than after.
- Which declarations an interface publishes beyond its members, and the modport as a named subset of
  them. Those ride the interface workstream, which consumes S1's answer rather than settling it.
