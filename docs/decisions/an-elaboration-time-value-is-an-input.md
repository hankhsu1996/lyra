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
is read for its settled value.**

**The policy check enforcing that is a proxy and this entry used to describe it as the thing
itself.** It bans two spellings of taking the answer the front end cached. A parameter's value comes
through a third, its own accessor, so the check never saw a block's parameter being folded to a
literal while this entry said every position had been dealt with. The list being short is not the
defect: no list of spellings is the property, because the next accessor is outside it on the day it
lands. What a site actually owes is the name of what its value varies with, checked against the axes
an artifact's identity has -- here, the unit and its parameterization and nothing else -- and that
is a sentence a reader checks rather than a pattern a script matches. Until a check states it that
way, the two legitimate folds say it in a comment at the site.

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

**And the consumer to read is the one that decides, which is not always the one in front of you.**
Where a value sits inside a type, what the artifact does with it is decided by whoever owns that
type's representation, and this record follows that answer rather than reaching its own. D3 is the
one position of that kind here, and it reads as an exception to D1 only because the answer is
somebody else's to give.

### D3. A declared width is not decided here, and this entry states where it stands

A repeated structure whose packed dimension is written from its own index is compiled once per
index. That is what happens; it is not what this record decided, and an earlier version of this
entry claimed it was.

What it said: the width stays a specialization axis, because one storage type for every integral is
paid for by operations that are inline and fold against a width the compiler can see as a constant
where the object is built, so supplying the width there instead would remove what the folding folds.
The mechanism it named no longer exists. A packed value now carries its width as one of three
scalars, an access naming a position takes the receiver's shape as an operand, and the structure the
value used to hold is not in the value at all --
[packed-shape-belongs-to-the-type](packed-shape-belongs-to-the-type.md) settles that, with the
measurement and the survey. There is no constant-width fast path to protect.

That record also closes the direction the withdrawn argument was defending. Making the width a
compile-time type parameter is rejected there on the top-level objectives: distinct types per width
is compile-time work scaling with the design rather than with its distinct specializations, and it
forecloses the parameterized unit that compiles once, which needs a width that arrives at
construction.

So what is left is mechanical rather than chosen. A declared width sits in the type; two types are
two classes; a repeated structure declaring differing widths is compiled apart. Whether the width
should sit there at all belongs to whoever owns the value representation, and that question is open
in their terms rather than settled in these.

Two further arguments this entry once gave are withdrawn for being false rather than superseded,
since a false reason for a right answer expires without warning. **The expression is not gone**: the
front end keeps the bound left and right expressions beside the folded range, and a declaration
hands them back on request, so nothing about moving the width is blocked by what the front end
discarded. **And rarity decides nothing.** How unusual the construct is would matter only if the
answer were a cost-benefit judgement, and no version of this entry was entitled to make one.

The lesson this leaves is about reading rather than about widths. Every argument withdrawn here was
derived carefully from documents that were current in one checkout, and a checkout announces nothing
about being behind. What settles a question of this kind lands as somebody else's record, so the
answer to "is there a decision about this" is only as good as the last fetch.

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
- A parameter a generate block declares is a declaration of that block holding the expression the
  source wrote (LRM 6.20.4, 27.4), rather than the literal one index folded it to. A loop whose
  blocks name a constant worked out from their own index is now one body. This one was on the list
  as done while it was not, which is what the paragraph under D1 is about: the check could not see
  it, and the entry that said the list was complete was read as the evidence that it was.

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
