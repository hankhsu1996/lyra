# An elaboration-time value is an input, not a constant

Date: 2026-09-18 Status: accepted

## Context

The front end evaluates every expression it can, whether or not anything asked it to. So at nearly
every point where this compiler reads something the source wrote, an already-computed answer is
sitting beside the expression that produced it, free to take. Taking it looks like nothing: the
program means the same thing, the tests pass, and the emitted code is smaller by one expression.

What it actually does is decide where that value enters the artifact. A value read at lowering is
compiled in, so two elaborations that settled it differently compile to two artifacts. A value left
as an expression is supplied to one artifact when it is built. The two are indistinguishable from
inside the lowering that chooses, and the cost lands somewhere else entirely -- in how many classes
a repeated structure produces, which is the thing that decides whether a large design compiles at
all.

The choice was being made one site at a time, by whoever wrote each lowering, against no stated
rule. The sites were even enumerated once, but under the question "where can a loop index reach"
rather than "where are we settling something nobody asked us to settle", and the difference between
those two questions is why the enumeration attributed a query's folded dimension to the operand's
type, where it belongs for one spelling of the query and not for the other.

## Decision

### D1. The compiled form states the expression, not the answer

A position the language leaves as an ordinary expression is lowered as one. That the front end can
evaluate it is not a reason: evaluating it is an optimization, an optimization belongs to whatever
runs after the shape is fixed, and a layer whose purpose is to say what the user wrote has no
business holding an answer the user did not write.

A clause requiring a constant expression there does not change this. It says the value is known
before the program runs, which is a fact about the source; whether the artifact has to hold it is a
fact about the artifact, and D2 is how that one is answered. **No position in the lowering of a body
is read for its settled value, and the policy check enforcing that carries no exception.**

The exception this entry originally reserved -- a position the standard fixes, read while citing the
clause -- turned out to have one candidate, and the candidate failed D2 on inspection. What is left
of the idea is the discipline: a site that reads a settled value and cannot say what about the
artifact would otherwise differ is settling something on its own authority.

### D2. What decides is whether the value changes the class or a value in it

A position the standard fixes is still an input rather than a constant unless something about the
artifact says otherwise, so a citation settles nothing about where the value belongs. The question
that decides is:

> if this value were different, would this be a different class, or the same class holding a
> different value?

A value that decides which members exist, which types exist, or which unit is instantiated changes
the class, and the class must hold it. A value that only ever ends up as a number a field holds, a
width a descriptor carries, or a count a loop runs to is the same class either way, and it is
supplied at construction.

A dimension query's index is the second kind: its result is an `integer` whichever dimension is
named, and nothing about the artifact changes with it.

**This entry named `$past`'s tick count as the first kind and was wrong.** It said "the count
decides how much history is kept, so two counts are two shapes", which reads as a fact about the
language and is a guess about this compiler. The storage a history keeps is a run-time sequence
filled at the install from a number handed to it, and a read reaches an entry by a number handed to
it as well; neither the count of entries nor the distance back is stated by any type. So two counts
are one shape holding a different number, which is the second kind, and the count is now supplied at
construction like any other.

The lesson is in how the mistake read. Both halves of the sentence were true -- the standard does
fix the count (LRM 16.9.3), and the count does decide how much history is kept -- and the conclusion
still did not follow, because how much of something is kept says nothing about whether the amount is
part of the class. **The question is answered by looking at what the artifact does with the value,
never by how important the value sounds.** Looking took one read of the run-time declaration, and
the entry shipped without it.

**And "same class" is not the whole of the question, because something may be resting on the value
being known where the object is built.** A representation can be one class for every value
_precisely because_ its operations are inline and fold against a value the compiler can see at the
construction, and then two values are one class and still not interchangeable: supplying one at
construction removes what the folding had to fold. So the check has a second half, and it is asked
of the same consumer -- does anything about how this representation pays for being uniform need the
value to be a constant here. Where the answer is yes, the value stays settled however plainly the
class is one class. D3 is that case, and it is the reason D3 comes out the other way from everything
else here.

### D3. A width stays a class fact, because what bounds its cost needs it constant

A declaration's width stays a specialization axis: a declaration whose width an elaboration-time
value decides gets its own class, and a repeated structure declaring one is compiled once per value.
This is the one position in this record that comes out the other way from D1, and it is a derivation
rather than a concession.

What makes it look like the others is a measurement, and the measurement is true. `logic [3:0]`,
`logic [7:0]` and `int` all emit as one storage type; the width travels in a descriptor the artifact
builds by calling the runtime, and two part-selects of different widths emit the same operation
sequence differing in two constants. Read against D2's question that says "same class, different
value", which is what this entry used to conclude.

**It is the wrong reading, and what it misses is that one storage type for every width was chosen
together with the thing that pays for it.** That every integral is one class is settled in
[integral-representation](integral-representation.md), which rejected splitting the emitted type by
width as an unmeasured optimization -- and reserved the performance answer explicitly: the class's
operations are inline, so a compiler that can see the declared width **as a constant at the site
where the object is built** folds the dispatch away without the emitted shape changing at all. The
width being settled where a declaration is built is not an accident of the current lowering. It is
the mechanism that decision named, and the only one it left itself.

So supplying the width where the object is built does not defer that; it removes it. A width the
program computes cannot be folded against, whatever runs later, because there is nothing to fold.
This record's own rule about gaps is what misleads here if the gap is named wrongly: the uniform
storage type is not a gap waiting to be closed by specialization -- it is a decision, with its own
recorded reason, whose cost is bounded by a fast path that needs a constant. Taking the constant
away is not waiting out a gap; it is spending something already committed.

And for a repeated structure the two questions are one question. A shared body is exactly what makes
the width stop being a literal where the object is built, so keeping the fast path reachable and
compiling a class per width are the same decision seen from two sides.

**And this is the shape a parameterization already has.** A unit whose declared width comes from a
parameter compiles once per distinct width, and nobody reads that as a failure of sharing. A packed
dimension written from the index of the structure being repeated says the same thing with the
parameter spelled differently, so a class per width there satisfies the rule that artifact count
follows specializations rather than instances, instead of violating it.

Two arguments this entry previously gave are withdrawn, because both are false and a false reason
for a right answer expires without warning. **The expression is not gone**: the front end keeps the
bound left and right expressions beside the folded range, and a declaration hands them back on
request, so nothing about moving the width is blocked by what the front end discarded. **And rarity
decides nothing here.** How unusual the construct is would matter only if the answer were a
cost-benefit judgement, and it is not; a common construct written that way would get a class per
width too, for the same reason.

What would reverse this is not a design found sitting on it. It is a profile: the fast path that
justifies one storage type per integral is reachable only while the width is a constant where the
object is built, so if that path is measured not to pay, or is abandoned for a split by width, the
reason given here goes with it and the width becomes a construction input like everything else.
Either way the question belongs to whoever owns the value representation, and this record follows it
rather than deciding it.

## Consequences

Emitted code grows where a value that used to be a constant becomes an expression the program
evaluates. Nothing downstream currently folds such an expression back, so the growth is real rather
than theoretical, and it is the price of the shape. The trade is the right way round: a constant
recovered by a later pass costs nothing anyone has to know about, while an artifact per elaboration
is a wall that no later pass can take down.

Positions where the standard is silent on what happens at run time take the reading the same clause
already gives for a question it cannot answer. A dimension whose size varies per outer element has
no single extent, so a query naming it reads as `x`, the way one naming a dimension out of range
does (LRM 20.7, 20.7.1) -- and because the front end rejects every such index it can read before the
program runs, what reaches that reading is a program the standard has already called erroneous.

## Status of the sites

Every position a body states is now lowered, so what follows are the worked examples rather than a
queue.

- A dimension query's index is lowered (LRM 20.7). A loop whose body queries a dimension by its
  index is now one body.
- A loop generate's step is lowered, whichever of the forms LRM 27.4 admits it takes.
- `$past`'s tick count is lowered (LRM 16.9.3), both where a read names its distance and where the
  history is told how many entries to keep. A loop whose blocks reach back by their own index is now
  one body.

Two positions stay where they are, for reasons that are not the same reason. A declaration's width
is chosen to be a class fact, by D3, and the choice would hold even if moving it cost nothing. A
reference's coordinate is a hierarchical lookup's own answer -- the front end resolves the path and
hands back the index rather than the expression behind it -- so it is settled before this compiler
is asked anything, and no position in this record turns on it.

## Where D1 does not reach

A position written in a unit's header or in a type declaration is elaborated once per
parameterization, and a parameterization is already its own artifact. So an expression written there
cannot name the index of a structure being repeated, and reading the value it settled to cannot cost
a second artifact. A port's declared default (LRM 23.2.2.4), a structure member's default (LRM
7.2.2) and a port reference's coordinate (LRM 23.2.2.1) are each of that kind, and each is a
position the standard fixes besides.

This is the line the rule is enforced along, and it is the reason the enforcement can be mechanical
without reading any clause: the lowering of what a body states, where an index reaches, is held to
D1, and the declaration surface, where an index cannot reach, is not. A reason phrased instead as
which of the two spellings a site happens to use -- taking the cached value, or evaluating the
expression a second time -- would separate the same sites today and stop separating them the moment
somebody writes the other spelling.
