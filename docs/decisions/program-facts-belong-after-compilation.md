# A program-level fact belongs to the party that runs after compilation

Date: 2026-09-13 Status: accepted

Reverses one point of [dpi-foreign-boundary](dpi-foreign-boundary.md) D4, and the ownership
paragraph of `../architecture/runtime_distribution.md`; neither is otherwise disturbed.

## Why this decision matters

`compilation_unit_model.md` invariant 11 says a unit's emission is a function of its own contents
and the signatures it consumes, and of nothing else. The compiler violated it at exactly one place.
Lowering already hands each unit on and releases it, so no step holds two units' bodies -- but the
design's own link-level unit read a small **record** each unit published rather than a signature,
and three facts sat on that record.

A record like that is not a defect on its own. It is the whole-design read compressed to its
smallest honest form, and it is what made the memory win possible. It is a bridge, and the reason to
name its far side is that a bridge nobody finishes becomes a road.

The three facts and what makes them one subject: the record only disappears when the last of them
moves, so moving them separately keeps it alive across three changes.

## What the standard already assigns

Two of the three are settled by IEEE 1800 itself, in the verbs it chose.

**Initialization order.** The standard states a barrier four times and an order nowhere. LRM 26.2: a
package's "variable declaration assignments shall occur before any initial or always procedures are
started". LRM 6.8 and 10.5 say it of any static variable, and 6.7.3 of a net with a user-defined
nettype. Nothing orders one initialization against another, and LRM 26.3's "the compilation of a
package shall precede the compilation of scopes in which the package is imported" is about
compilation rather than about when an initializer runs.

**So an order is not required, and Lyra chooses one anyway.** Reproducibility is not the reason --
any fixed order gives that, including sorting by name. The reason is that an initializer reading
another package's variable is written expecting that package's value, every established simulator
answers that way, and a design ported to Lyra that got a default instead would be wrong in a way
nothing reports. That is a quality-of-implementation choice, and stating it as one is the point: a
conformance case may not assert it, because a conforming simulator is free to answer otherwise.

**The foreign name space.** LRM 35.4 says an imported subroutine "shall **eventually** resolve to a
global symbol", and that these names "have their own global name space of linkage names, **different
from compilation-unit scope name space**". A name space declared to belong to no compilation unit
was never a thing a compiler assembles the union of. The same clause permits several export
declarations of one `c_identifier` "as long as they are in different scopes and have the equivalent
type signature", and forbids the repeat only within one scope -- so the language states outright
that one name may be declared by more than one party.

## The decisions

### D1. The design's link-level unit reads signatures and nothing else

It is a referrer like any other. What it reads about a unit is that unit's signature: the class an
instance of a top is, and -- because a signature says whether a unit roots an object at all -- which
units are namespaces it must bring up. Being synthesized rather than lowered from source is not a
licence to read more.

### D2. A namespace's initializers order themselves, and no party holds the graph

Each namespace's initialize entry takes that namespace's one bring-up before doing anything, and
then calls the initialize entry of every namespace its own initializers read. The root calls every
namespace in a stable order and the claims make each run once. The order is those calls executed.

Three things fall out, and the third is why this shape rather than a composed one. A cycle
terminates, because the claim is taken before descending. A read naming a namespace is an ordinary
declared dependency, since the initializer already reaches that unit's cells. And the party that
holds the whole graph is nobody: each unit states its own edges as calls, in its own artifact.

The claim is the runtime's rather than a flag in generated code, so a second design brought up in
one process starts with none of them taken.

### D3. A foreign symbol no unit owns is defined by every unit that declares it

A symbol several artifacts may define needs a merge rule, not an owner. Every unit declaring a scope
that exports the name defines the symbol; the definition is generated from the name and the
prototype alone, so every one of them is the same text; and the party that resolves names across
artifacts keeps one. Which party that is follows from what the artifact is: a linker for a backend
that emits object files, an execution session for one that loads modules, and the preprocessor for
one that assembles the program by textual inclusion. The linker is the party exercised today. The
execution backend does not yet publish a foreign entry point at all and states its own rule when it
gains one; nothing above the artifact varies with the answer.

**Which of the two a name is, is stated by where the unit states it, and by nothing else.** A unit
publishes what its own namespace owns and, separately, what it declares of a name whose entries sit
on scopes; a definition goes with the statement it belongs to. That placement is the whole of the
distinction: a consumer emitting a definition from the second reaches for a merge rule and one
emitting from the first does not, and neither asks which kind it is holding.

This reverses the second half of [dpi-foreign-boundary](dpi-foreign-boundary.md)'s rejection of "the
program-global export symbol as its own species beside the callable arena". That rejection argues
from the symbol being "owned by the unit that defines it", which D3 has just shown is false here,
and from such a container leaving the export with no prototype record, which does not arise because
the record holding the definition is the one that already held the prototype. Its remaining cost is
real and is paid: the name is reached by a second walk wherever a unit's definitions are enumerated.
What that buys is the removal of a discriminator every one of those walks had to read and read
correctly -- and a body naming nothing the unit owns does not belong among the unit's own, which is
the same statement from the other side.

### D4. The union of the foreign name space is assembled by the build

Each unit writes what it states of the boundary. The header a foreign source includes names those
fragments and states nothing itself, so assembling it reads a list of files and no unit. Repeating
an identical C prototype is what every C header does.

What cannot be had is the cross-unit agreement check, and LRM 35.4 is why: it requires C naming and
forbids overloading, so no mangling can turn a type disagreement into a link error. The check lands
in the user's own C compiler, which is the one party that sees every fragment. Within a unit the
front end already rejects declarations of one name that disagree.

### D5. Whether a target can realize the design is not a program-level fact at all

A unit is refused as it is rendered, by the backend rendering it, and the run collects every refusal
(`reporting-every-gap-in-one-run.md`). Nothing is folded over the design, and a run that reported
anything produces no program -- which is the same thing a separate-compilation toolchain does when
one of several inputs fails.

## Rejected alternatives

- **Keep the record.** It reads as cheap -- a name and a prototype per unit rather than a unit --
  and that is exactly what makes a bridge permanent. Its cost is not memory; it is that invariant 11
  stays false and nothing else can be built on it being true.
- **Compute the initialization order at compile time, as Go does.** Go's compiler orders packages by
  a topological sort of the declared import graph, and any such order will do. It can, because the
  linker that consumes the result is one Go writes. Both parties here are borrowed -- a system C++
  linker and an execution session -- and neither can be taught to sort a SystemVerilog namespace
  graph, so the only moment both backends share is startup.
- **Initialize a namespace on first read, as Java does for a class (JLS 12.4).** LRM 26.2's barrier
  means every namespace's initializers must have run before any procedure starts, so laziness saves
  nothing and puts a check on every package-variable access.
- **Impose no order at all, which is what the standard permits.** The cheapest answer, and it is
  conforming: bring the namespaces up in any fixed order. It is rejected on the ground above -- a
  design ported from another simulator would read a default where it expects a value, with nothing
  reporting it -- and the ground is worth keeping visible, because it is the only one. Nothing in
  IEEE 1800 is being satisfied here.
- **Take the initializer's reads from the front end's flow analysis rather than from the lowered
  unit.** That is where every other read set in this compiler comes from, and the reason is
  precision: a sensitivity set that over-collects wakes a body that had no reason to run, which
  costs simulation time. Neither half transfers. Over-collecting here adds a call to an entry that
  answers immediately, and the reads being looked for are a closed set of two cross-unit reference
  forms in the lowered body rather than leaves of an arbitrary procedural expression, so no must-def
  or local-symbol reasoning arises. What the walk must not become is a general reader of bodies; it
  reads two node kinds and stops.
- **Register each namespace's initializer with the runtime and let it sort them.** It needs a
  vocabulary for the address of a unit-level callable, a registry, and a topological sort -- to
  express an order the calls already express.
- **`inline` for the shared definition, where the program is one translation unit.** Measured, not
  reasoned: a C-linkage function may be defined only once in a translation unit however it is
  spelled, and an inline definition nothing in that language calls is emitted nowhere at all --
  which is every foreign entry point, since it is called from outside. Three conformance cases
  failed to compile or link. What keeps one definition there is a guard, and the general form of the
  same trap is a linkage that permits dropping an unreferenced definition.
- **A per-symbol owner chosen by rule -- say, the lexically first declaring unit.** It restores a
  single definition without a merge rule, and it needs every unit to know which of them was first,
  which is the whole-design read again wearing a tie-break.
- **Keep the definition among the unit's own callables and tag which kind it is.** The shape this
  started as, and the tag is what condemns it: it says of a body sitting in the pool of what a unit
  owns that this one is not owned, which is a receipt for the wrong placement rather than a fact
  worth stating. It also states the same thing twice, since the name is already on one list and not
  the other, and two statements of one fact are held in step by nothing.

## Consequences

- The record disappears carrying nothing, which is the check on the diagnosis: a fact still needing
  a home would have meant a fourth one was in the wrong party.
- `emission_model.md` invariant 9 stops describing an intention and describes the code.
- A unit's artifact now names the namespaces its own initializers read, so a cross-unit reference it
  already had becomes visible in its emitted output as well.
- Two units that reference each other still emit a C++ project that does not compile, for an
  unrelated reason: that backend puts a unit's declarations and its bodies in one file. The language
  requires nothing there that this design does not already satisfy, and the execution backend runs
  such a design.

## Cross-references

- `../architecture/compilation_unit_model.md` invariant 11 -- the property this makes true.
- `../architecture/emission_model.md` invariants 9 and 10 -- the link-level unit as a referrer, and
  a backend as a choice of linker.
- [unit-signature](unit-signature.md) -- what a referrer may read, which D1 holds the root to.
- [dpi-foreign-boundary](dpi-foreign-boundary.md) -- the boundary model D3 and D4 leave standing
  apart from where the symbol is defined.
- [reporting-every-gap-in-one-run](reporting-every-gap-in-one-run.md) -- the staging D5 relies on.
- [static-initializer-draws-from-its-container](static-initializer-draws-from-its-container.md) -- a
  namespace's own bring-up extent, which D2 leaves around each namespace's own initializers.
- LRM 26.2 (a package's declaration assignments precede any procedure), 35.4 (the global name space
  of linkage names; C naming, no overloading; one name across several scopes), 35.5.4 (declarations
  of one name agree).
