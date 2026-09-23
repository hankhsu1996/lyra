# A loop generate's blocks are one body built at every index

Date: 2026-09-17 Status: accepted

## Context

A `for` generate elaborated N blocks into N compiled scope classes. Each one held the same
declarations under a different name, and the enclosing unit held one member and one construction
statement per block, so a unit's emitted text grew with the iteration count. Measured on a one-deep
loop around a trivial leaf at N = 1, 4, 16, 64 and 256: about **4.2 KB of emitted text per
elaborated block in the bodies file and 1.85 KB in the declarations header**, both linear. The
second half is the one that travels -- the declarations header is what every referrer includes and
parses -- so splitting translation units does not touch it.

That is compile-time work scaling with something other than the count of distinct unit
specializations, which `north_star.md` invariant 2 is written against. It is also what
`specialization_model.md` already describes as the answer: a parameter steering a `generate for` "is
a constructor input; the unit compiles once for any N".

The thing standing where that answer goes is
[generate-variable-specialization](generate-variable-specialization.md). Its F3 split is right and
is kept whole: an elaboration-time value that fixes a **static representation** forces a copy per
value, and one that only decides **constructor-time structure** does not. What this entry argues
with is its D4, the conservative default -- absent a proof, explode -- because nothing had been
built that could produce the proof, so the default was the whole behaviour and the optimization it
defers had no path to existing.

## The tension this addresses

LRM 27.4 makes the index an implicit `localparam` of each block whose value is what the index held
when that block elaborated, usable "anywhere within the generate block that a normal parameter with
an integer value can be used" -- and the genvar itself "does not exist at simulation time". So the
index is a value the construction supplies. What stops a class carrying it is not the index at all:
it is that **this lowering bakes elaboration-time answers into constants**, and a constant is the
one thing a class cannot vary per instance.

That reframes the question. "How can two blocks differ" is a question about SystemVerilog, and its
answer is an open set -- every construct is another way. "Where does this lowering bake an
elaboration-time answer" is a question about our own code, and its answer is a closed list that can
be read off:

| What is baked                                  | Can an index reach it                     | Where it is fixed          |
| ---------------------------------------------- | ----------------------------------------- | -------------------------- |
| a packed array's range, into the interned type | yes                                       | the type carries its shape |
| a parameter's value, where it reaches a type   | yes                                       | the type carries its shape |
| a reference route's coordinates                | yes                                       | the front end resolves it  |
| a child's unit specialization                  | yes                                       | a separate artifact        |
| a port's declared default                      | no, it resolves in the child              |                            |
| an enum member's value, a field's default      | only through a type declared in the block |                            |

One entry is the reason the list cannot be emptied here. **A reference's coordinate is gone before
the lowering sees it**: the front end hands back the resolved index and not the expression that
produced it, so `ring[(i+1)%3].v` arrives as `ring[2]`.

**The parameter row said "a parameter's value, folded to a literal" and gave the width's reason for
it, which is the mistake the paragraph below records being made one row up.** A parameter whose
value reaches a type really is fixed by the type; a parameter that only ever ends up as a number a
field holds is fixed by nothing, and the two sat in one row under one reason for as long as the row
stood. A block's parameter is now a declaration of that block holding the expression the source
wrote, so what is left in the row is the half the reason covers. The general question the row should
have been read against is which axis of an artifact's identity the value varies with: a type is one,
and a repetition index is not.

A queried dimension was on this list and is not any more, and what it was doing here is worth
keeping because the same mistake fits several of the rows that remain. It was filed under "a value's
shape lives in its type", which is true of `logic [$size(a,i)-1:0] w` -- there the answer really is
a type, the blocks really do declare different things, and nothing about that is ours to change. It
is not true of `x = $size(a, i)`, whose type is `integer` in every block; the only thing that
differed there was a number this lowering chose to settle. The standard states that dimension as an
ordinary expression and lets its constness decide one thing only, whether the query may stand in a
constant expression (LRM 20.7). So the row held two constructs, and the reason it gave covered one
of them.

The general form is recorded in `an-elaboration-time-value-is-an-input`, because it is not about
loop generates: the front end folds whatever it can, so every position where an elaborated answer is
available is a place this compiler chooses what to bake, and the question to ask of each is whether
a different value there would be a different **class** or only a different value inside one.

The largest row is the first, and the obvious reading of it is wrong: a width is not a type here, so
`logic [i:0]` needs a second class only because the descriptor carrying that width is settled where
it could be computed. The measurement behind that, and why a clause requiring a constant expression
still does not put the value in the class, are in `an-elaboration-time-value-is-an-input`.

## Decision

### D1. A repeated structure is one class, and its index is a parameter of its construction

Where a loop generate's blocks are one body, the unit compiles that body once and the enclosing
scope builds it once per index it counts out, holding one member whose type carries the
multiplicity. The block's implicit `localparam` is a declaration of the block filled from what the
construction supplied, so a name reaching it reads that declaration instead of folding to one
block's value.

The loop itself survives into the compiled constructor -- its initial value, its condition and its
step -- because the construction is what counts the blocks out. The first two are values the loop
reads; the step is the expression the source wrote, placed for its effect on the index, which is
what every form LRM 27.4 admits for one does. So how the step is written decides nothing: the
sharing question is never asked of it.

This sentence used to end differently. It said that a step written in a form the lowering did not
carry kept the blocks apart, costing the sharing and nothing else -- true of what was built, and
built around reading the step for the value it named next, which only some of the forms state
outright. Of the seventeen operator forms the clause admits, twelve were left behind, and among them
the one the clause itself names where it allows a sparse index. What removed them was not a longer
list but the observation that a procedural loop had already answered this, for a step the standard
defines as a superset of this one's.

### D2. Whether the blocks are one body is read off the lowered blocks, and nothing predicts it

The index reaches every block as a value its construction supplies rather than as a constant folded
into it. That is what makes one body possible at all, and it is also what makes the blocks
comparable: two that differ in nothing else then lower to the same scope. Each further place that
stops folding the index admits more bodies, and a place that goes on folding costs sharing and
nothing else.

**So every block is lowered, and the scopes are compared with each other.** All alike and the
construct keeps one of them plus the loop that builds it at every index; otherwise each is a child
in its own right. There is nothing to choose between beforehand, so nothing has to be right about
the source in advance.

The requirement that selects this is that correctness does not depend on the sharing decision: a
correct program is never refused because it could not be shared. A shape that has to predict the
answer cannot meet that, whatever is done about any one hole in the prediction, because an
unpredicted difference has to go somewhere and the only places left are a wrong program or a
refusal. Removing the prediction is what leaves neither.

This costs what the previous shape cost, which is why it is not a trade. Both forms lowered every
block already -- the shared one lowered the rest only to check them and threw the results away -- so
comparing what they produced is the same work with the prediction removed. And the prediction was
not free: it was a list of the ways two blocks might differ, the list belongs to SystemVerilog
rather than to us, and no completeness argument was available for it. The front end settles more
than twenty kinds of deferred member per scope, and at least five were not on the list until its own
source was read.

The comparison is derived from the node definitions rather than written: a field added anywhere
below is compared without anyone remembering to, and a field that cannot be compared breaks the
build rather than being quietly left out. One field takes no part on purpose -- the block's own
hierarchy index, which is what a loop's blocks differ in by definition, so it is stamped on after
the comparison rather than lowered into the scope.

What an expression evaluated to takes no part either, and it cannot, because no position a block
states is kept as a value: each is lowered as the expression it is, and two blocks that wrote the
same expression state the same thing whatever it works out to. That had to be earned and was not
true when this entry was written; the positions the lowering kept are now none.

**What the predict-then-check shape found while it stood, both times through a hole in the
prediction.** A generate block declaring `import "DPI-C"` made two blocks that behave identically
hold different ids for one program-global foreign symbol, because the record was keyed by which
declaration spelled it; the fix was to key the record by what it states. Then a queue bounded by the
loop's own index was refused outright, because the prediction compared declared types with the
relation deciding what a program may assign between and a queue's bound is not part of that. The
first was a real defect the check caught. The second was the check reporting that the prediction
could not be completed, which is what removing the prediction answers.

The field's own answer is the same shape, in both systems that do this. LLVM merges identical
functions as a pass over IR that already exists, comparing instruction sequences, and leaves an
alias or a thunk where it folded one away so every reference still resolves. Verilator creates a
scope per cell usage, relinks every name against the flattened result, and folds identical generated
functions at the end of its optimization pipeline, rewriting the call sites. Neither answers the
question before the bodies exist; in both, a name indirection is what makes folding unable to break
a reference. Ours is that indirection: a reference names the block, and which compiled scope that is
belongs to whatever realizes the construct.

Speed decides none of this, and that had to be measured because it was twice assumed. At 256
iterations the entire semantic and generic-IR pipeline is under half a second, and the elaborated
duplication inside it costs 0.019s; the optimizer and code generation cost 4.2s and compiling the
emitted target 16.8s. Duplication is free everywhere above the artifact, so lowering every block
costs nothing worth weighing.

**What this is not yet.** The semantic layer still states the elaborated blocks where they did not
lower alike, rather than stating the loop the source wrote and letting a body that needs a fixed
value fall back on its own. That is a further step and this one is what unblocked it: a name now
reaches a block rather than a compiled scope, so which form the construct takes is no longer
something any earlier pass has committed to.

### D3. This inverts the conservative default rather than replacing the classification behind it

`generate-variable-specialization` D4 made exploding the answer absent a proof. What D2 changes is
the direction rather than the threshold: the loop is what the semantic layer states, and exploding
is what happens when something the body needs cannot be supplied at construction. So there is no
proof to wait for and no classifier to build -- the question that record poses, "does one generic
Build program reproduce them all", is answered by construction for every value the body reads and by
a refusal for every value it must have fixed.

Its F3 **split** stands: an elaboration-time value that fixes a static representation forces a copy,
one that decides constructor-time structure does not. Its F3 **list** does not. That list names
"packed / unpacked type shape, width, or range" as static representation, and a width is not one
here: every integral emits as one storage type and the width rides in a descriptor the artifact
builds by calling the runtime. What makes a width behave like a static representation today is that
the descriptor is folded, which puts it in the table above rather than in F3's first kind. The same
sentence is in `specialization_model.md` invariant 3, and the conflict is already recorded as an
open question with "resolve before keying on width" against it.

What is withdrawn is that record's consequence that "the exploded, per-block concrete scopes are the
sole generate lowering representation today". They are no longer.

### D4. A construction is entered through the definition, carrying what the class is built with

D1 needs a constructed scope to receive a value, and what construction crosses is a boundary where
the constructing site may hold nothing of the scope it builds but that scope's definition -- which
is all one compilation unit names of another unit's scope. A prototype cannot be restored there, so
one entry type serves every class: the scope, the parent it hangs under and the identity it is
reached by travel as themselves, and whatever values that class alone is parameterized by travel
erased and counted, for the class's own entry to read back.

The alternative is for generated code to call the constructor by name, which is what a
source-language class's construction does and what the object path already proves workable. It is
rejected here on the boundary above: a unit holds the definition of another unit's scope and not its
constructor's name or prototype. That this is survivable today -- a module instance's parameters are
baked into its specialization, so a cross-unit construction carries no values -- rests on a gap
rather than on a condition. The demotion `specialization_model.md` invariant 5 describes turns a
value-only parameter into a constructor input, and the day it lands a cross-unit construction
carries values; an answer that expires when the gap closes is not the one to write down.

A backend whose construction site names the class's own constructor builds it there instead and
supplies no entry at all.

## Rejected alternatives

- **Recover the index from the block's own hierarchy segment.** The segment already carries it,
  because LRM 27.4 indexes the generate block array by the genvar's value, so the entry could take
  nothing and the block could read its own name. Rejected because it makes a value the construction
  knows into something derived from how the object is addressed: the producer states the index once,
  into the name, and the consumer reads it back out. It also ties the parameter to the naming, so a
  block reached by any other identity loses its value.

- **Write the index into the block's member after construction returns.** The member exists and the
  constructing site holds the handle. Rejected by a nested loop: `for (genvar j = 0; j < i; j++)`
  inside the block counts its own blocks out during the outer block's construction, so the index
  must be readable before that construction finishes.

- **One entry per construction arity, or a single trailing value rather than a counted run.** Both
  encode a cardinality nothing guarantees, and every other variable-length crossing of this boundary
  is a pointer with a count.

- **Take the blocks as one body when their declarations agree.** The first shape built, and it is
  the one this entry's D2 exists to correct: it passes a block whose index reached a part-select
  bound, a neighbouring block, or a folded constant, none of which is a declaration. Each was a
  wrong answer rather than a refusal.

- **Keep extending that comparison as each new way to differ turns up.** What replaced it after the
  third one, and it is worth writing down because it looks like diligence: every entry added was
  correct, the tests went green each time, and the thing missing was an argument that there would
  not be a fourth. What ends that regress is not a better list but declining to ask the question:
  the loop is the form, and what bounds it is the one place an elaboration-time value can become a
  constant.

## Consequences

- A unit's emitted text stops being a function of the iteration count for a loop whose body only
  reads its index, which is the loop anyone writes. Measured at N = 256 on a block declaring one
  variable and writing `sink[i] = i`: 9,666 bytes of bodies and 7,072 of declarations, which is what
  N = 4 emits, against 1,064,336 and 765,273 before -- and the host compile of that emitted project
  goes from 16.8 seconds to 3.5. Before this, the rule admitting bodies compared what each block's
  index folded to, so it refused every loop whose body used the thing a loop exists for, and the
  collapse it guarded was never reached.
- `parameter-code-shape-over-approximation` F2's claim that a `generate for` is "O(1) in the
  iteration count even under full folding" becomes true for the shareable case. It was false when
  written and stayed false; it is still false for a loop whose index fixes a static representation,
  which is the case that record's F3 already accepts.
- Every scope's construction on the execution backend now receives what the site states. It used to
  receive only the scope, through an entry typed as if that were the whole prototype, while the
  generated constructor declared the parent and the identity as well. Nothing read them, so nothing
  failed; the index is the first parameter a construction reads.
- A `generate if` and a `generate case` are untouched and need nothing here. One of them produces at
  most one block, so there is no iteration count to collapse and nothing to share it with.

  **That is true of a conditional written outside a loop and false of one written inside**, which is
  elaborated once per index and so has an iteration count like anything else there: one construct
  selects a different alternative at different indices, and blocks alike in everything else are kept
  apart by it. Which alternative stood was therefore a value this lowering baked in -- a position
  the table above never listed, for the reason this sentence gives -- and it no longer is:
  [a-conditional-generate-chooses-at-construction](a-conditional-generate-chooses-at-construction.md)
  carries the condition the source wrote instead of its answer.

- **Every entry in the table is a place a body would have to refuse, so emptying it is what makes
  refusal rare**, and each entry is removed by carrying an expression where a constant is baked
  today. Two rows have since left it. An assignment pattern's designated key was never baked at all
  -- every form of key was already lowered, and the row recorded a suspicion nobody had checked. A
  sampled value's depth was baked, and its reason -- "the standard fixes it" -- was the mistake this
  table invites: a clause requiring a constant says the value is known, never that the artifact has
  to hold it. What remains needs something this lowering cannot do alone. A reference's coordinate
  needed the front end to hand back the expression beside the index it resolved, which the fork now
  does and nothing yet reads. A width needs a type to be allowed to carry a shape it does not fix --
  and that is a gap of ours rather than a property of the language, because a width is already a
  runtime descriptor here and not a type. **A parameter's value was in that sentence and needed none
  of it**: the front end keeps the bound initializer beside the value it folded, and that expression
  names the block's own index, which the construction already supplies. So a block's parameter is a
  declaration of the block holding the expression, blocks writing the same expression state the same
  thing, and what the sentence was really describing was the width alone.
- **The front end is ours, which changes what counts as a reason.** Every sentence in this entry
  that says the front end does not supply something is a gap to be closed rather than a condition to
  design around, and the one such sentence that mattered has already been closed. The shape the
  field uses -- bind a body once against a parameter it has not resolved, as C++ does for a template
  and Rust for a generic -- is not reachable in one cut, but nothing about it is out of reach, and
  every step toward it removes refusals without changing the form above.

## Cross-references

- [generate-variable-specialization](generate-variable-specialization.md) -- the classification this
  supplies the missing half of, and whose default it replaces.
- [instance-array-multiplicity](instance-array-multiplicity.md) -- the same rule on the other axis:
  one member whose type carries the multiplicity, reached by a coordinate the elaboration resolved.
- [parameter-code-shape-over-approximation](parameter-code-shape-over-approximation.md) -- F2's
  claim about iteration count, and F3's accepted cost.
- `../architecture/specialization_model.md` -- concrete elaboration as the baseline and sharing as
  the optimization; the worked example that says a `generate for` count is a constructor input.
