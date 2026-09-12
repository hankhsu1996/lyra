# Design process

How to arrive at a design in this codebase, and how to falsify one. Comment rules live in
`code-comments.md`; doc-writing rules live in `style.md`.

## What you are reading it for, not what you are allowed to read

Read anything to learn what a construct has to do -- the north star, an architecture doc, the LRM,
and the code itself. What decides whether that reading helps is which question you brought to it.

- _What shape is here?_ -- you will copy it, whatever you intended.
- _What must this construct do?_ -- you are reading requirements.

The code is the dangerous one, and not because it is low-level: its comments cite the standard, so
reading it feels like reading the standard. What is actually there is the answer somebody reached
once, with their entity list, their split, and their vocabulary already chosen. Take the facts it
holds and nobody else does -- what runs before what, who calls whom, whether a state is reachable --
and refuse it the question of what the shape should be.

"Read the code last" does not cover this, because reading it last and still letting it answer that
question is exactly what happens.

**The test is on the sentence, not on your intentions.** Write the requirement in one sentence, and
look for a name in it:

> An initialization RNG still lacks its second consumer.

Every noun there exists already, so that sentence is a diff against today's code wearing a
requirement's clothes. The requirement was:

> Every draw comes from whatever is running, and its sequence does not depend on the order other
> things run in.

No existing name appears, so it could have been written against an empty repository -- which is the
property that matters. **A requirement sentence naming a type, a function, or a file is a shape you
read and mistook for a need.**

This is the first step, not a replacement for the ones below: derive from the requirement, then
compare against `decisions/`, and argue explicitly where the two disagree (see the last section).
Deriving without looking is the point; landing without comparing is not.

## Start from the layer's motivation, not from its invariant list

Every architecture doc opens with a Purpose that says why its layer exists. That paragraph is the
design authority. The Core Invariants below it are compressed conclusions someone derived from it
once -- useful as a cross-check, weak as an argument, and occasionally wrong.

So the order is:

1. State the motivation of the layer the change lives in, in one sentence, in your own words.
2. Derive the answer from that motivation and from the requirements in front of you.
3. Only then read the invariants and Forbidden Shapes. Agreement is a confirmation. Disagreement is
   a question -- and the invariant is the suspect at least as often as the derivation.

A design argument that ends "...therefore invariant N forbids it" has not been made. The argument is
the derivation; the invariant is a witness.

## The motivations, in one line each

- **HIR** -- so that "what did the user write" has exactly one answer, and it is SystemVerilog.
- **MIR** -- so that nothing downstream needs to know the source language was SystemVerilog.
- **LIR** -- so that nothing downstream needs to know the program was ever structured code.
- **A lowering** -- so that a class of knowledge stops being needed, by restating it in the next
  layer's vocabulary.
- **A backend** -- so that one target's spelling never becomes a fact any other consumer must know.

Every rule in `architecture/` is a consequence of one of these five. If you can reach the same rule
from the motivation, you understand it; if you can only reach it by quoting the rule, you do not.

## Write what each layer must say before writing any shape

The motivations above say why a layer exists. They do not say what a layer owes the construct in
front of you, and that is what to write down first: for this construct, in each layer's own
vocabulary, what that layer must state and what it must not. Write one for every layer the change
crosses, before naming a single field.

Two things fall out of having written them, and neither is reachable without.

**The lowering between two layers then has exactly one job, and it fits in a sentence.** If the
sentence needs an "and", the two contracts are not written yet -- and the work will get divided
between the layers by whichever side is easier to edit rather than by which one owns it.

**A fact appearing in both contracts is in the wrong one.** A layer states a fact because it is the
highest one that can; a lower layer restating it is two authorities for one answer.

Worked shape, from dynamic dispatch. A semantic layer must state that a call is dynamically bound
(never inferable -- a call whose receiver's exact type is known is still dynamically bound, and
whether it can be devirtualized is an optimization), the value it is made on, and which question is
being asked. It must not state a position, an ordering, or a table: a position means nothing except
against a chosen layout, and choosing one is what the layer below is for. The execution layer states
where to look. So the lowering has one job -- turn an identity into a coordinate -- and that
sentence names the whole change before any field exists.

## Survey the component and the stage, not the answer

Every design here has prior art, and the survey is not optional. What decides whether reading it was
worth anything is which question it answered.

"What does the other compiler produce" is answered the same way by everyone and transfers almost
nothing: of course a C++ compiler builds a dispatch table. The question that transfers is **which
component computes it, and at which stage**. Clang assigns dispatch positions in a vtable-layout
query sitting beside its record-layout query, consulted at code generation. A Java compiler computes
none at all: it emits a symbolic reference and its runtime assigns positions when the class links.
The split is not a matter of taste -- it tracks exactly one thing, when the class hierarchy is fixed
-- and it is invisible to a survey that stopped at the data structure.

That is the failure worth naming, because nothing downstream reports it: a survey answering only the
first question lets you build the conventional structure in the wrong component. The tests pass, the
shape looks like the textbook, and the layer that should have owned the decision never sees it.

## The one question: read the consumers

Before arguing about a shape, find everything that reads it and look at what each one does in the
first line after reading. A shape is right when its readers use it as it stands; every deviation is
paid for again at each of them.

| The first thing a reader does | What it means             | Do                                    |
| ----------------------------- | ------------------------- | ------------------------------------- |
| Inverts it                    | Stored the wrong way      | Store the direction they ask for      |
| Branches on it                | Two shapes, not one       | Find the shape that covers both       |
| Derives something from it     | The producer under-stated | Carry the fact from where it is known |
| Nothing -- no reader          | It should not exist       | Delete it                             |

This beats any rule for one reason: it is greppable. A rule has no call sites and can only be
invoked; consumers can be listed, and the list settles the argument.

## Five searchable smells

Each is the question above, pre-applied to a shape that recurs. Each has been a real defect here.

**1. A `default:` or fallback arm that throws.** Whatever the arm cannot name, some upstream
producer knew and did not write down. Do not add the missing case -- carry the fact.

**2. A `bool` or predicate that selects between two output shapes.** Two shapes means every
downstream consumer handles two cases, and the case a consumer forgets is a defect the other case
hides. Find the one shape that covers both.

Its tell at the producer is a conditional where a value is _built_ -- a ternary deciding whether a
field or an argument is there at all. That reads as a construction detail and is not one: it says
the type being built covers two shapes, each of whose fields means something on only one of them. So
the fix is never at the construction site. Split the type, and the branch goes with it, along with
the discriminator every consumer was reaching for to tell the two apart.

**3. A pass that runs before the real work to compute a property.** The property is a decision, and
it is being made at the layer that has the least information about it. Move it to the layer that
already held the inputs, or discover it was never a decision at all.

**4. An `optional<T>` field, or an `if (empty)` branch.** Ask whether "absent" is genuinely
reachable. Usually the thing always exists and only its _name_, _contents_, or _exposure_ is
optional -- three different questions that an `optional` on the whole thing has collapsed into one.

**5. Two alternatives of a closed set that no consumer tells apart.** They are one alternative. Each
instance of this had a decision record behind it, and each record argued from a distinction nothing
was using -- so the argument is never the thing to check. Three readings answer it, cheapest first,
and only the last is an argument:

- _Identical member lists._ A tell, and a noisy one: three separate operations here each hold one
  expression id. Never conclude from this alone.
- _The layer below merges them._ Two arms of one lowering constructing the same node underneath.
  Close to proof -- the merge already happened and only the names upstream survived -- and invisible
  otherwise, because both layers compile and both read as deliberate.
- _Every consumer emits the same thing._ The deciding reading. Line the arms up across both backends
  and the dump; where the output differs only in a name, or not at all, the alternatives were never
  distinct.

The fix is to collapse them and let whatever the consumers were really reading -- usually a type, or
a pair of types -- say it. What made each split look necessary was that it turned an unhandled case
into a build break; that property is kept by having each backend refuse what it cannot realize,
rather than by naming the cases in the IR.

## An optimization belongs to the layer that owns it

A distinction that exists only to avoid emitting work is an optimization, and a layer that encodes
one has taken a decision away from the layer whose job it is. The tell is a shape carrying "there is
nothing here" as structure rather than as content: an `optional` that is absent exactly when a list
would have been empty, a body synthesized only when it would have had statements, an arm skipped
because what it emits does nothing. Each one buys emitted work not done, and each one is paid for by
every consumer above the optimizer, in a branch and in a case its own vocabulary now has to admit.

**The reason to keep the uniform shape is not that the saving is small.** It is that the upper
layers are where generality lives: what they can express is the compiler's coverage, and every
special case admitted to save work narrows it. The saving is available in full one layer down, taken
by a pass that needs no one else to know it happened. Simplicity upstream is what makes the upper
layers general; the optimizer is what makes the output small. Those are two jobs, and the trade
between them is not a trade -- it is a division.

**This holds whether or not the optimizer is switched on.** Lyra's execution backend currently runs
only the passes that make a suspending body executable and nothing that removes dead work, so a body
emitted empty stays empty in the module. That is a fact about today's pipeline, not about where the
decision belongs. A shape defended by "the optimizer removes it anyway" is defended by a fact that
can change; a shape defended by "removing it is not this layer's decision" is not.

The sibling of `north_star.md` invariant 3, which says correctness is independent of optimization.
This says uniformity is too.

## Falsifying a proposed shape

Four checks, all cheap:

- **Could a mechanical LLVM IR backend translate this without deciding anything?** If a consumer
  needs an `if` to work out what a node means, the node is under-specified. This is the sharpest
  check available and it applies well beyond backends.
- **Does the empty case fall out of the general case?** Write the loop for N and check that N=0
  needs no branch. If it does need one, the data model is carrying two cases through code that
  should carry one.
- **Is this the same rule something here already follows?** A class's behaviors extend its base's
  exactly as its members do. Where one axis of a rule already exists, the second is expressed the
  same way at every layer it crosses -- if the first keeps a two-part logical coordinate and
  flattens it at the point of use, so does the second. Two shapes for one rule is what "the code is
  getting messy" is made of, and it is invisible from inside either axis: each is locally
  reasonable, and no reader of one is looking at the other.
- **How much machinery does this shape need, next to its neighbors?** A memo, a recursion, an extra
  "unknown" state threaded through several returns -- each is evidence about the shape, not about
  the problem. The mechanical form of the check is to try placing the work inside a contract some
  existing step already states. Dispatch positions counted across a whole lineage needed a recursive
  descent, a memo keyed by class, and an absent state carried through three signatures; the same
  positions named as an ordinal within the declaration that introduced them fit unchanged into a
  step whose stated contract is that it reads one declaration and waits on nothing. **A shape that
  fits a contract already written is usually right; one that keeps needing another mechanism to say
  where something lives is usually answering a question it created.**

## Do not mint a rule from a fix

The strongest temptation after fixing something is to write down the rule that would have prevented
it. Resist it. A fix is one derivation that came out a particular way; a rule is that derivation
with its reasoning removed, and the reasoning is the part that decides whether it applies next time.

This has already cost real work here. A round of fixes ended with the rule "whoever mints an
identity must fill the record in the same step". It reads well and it is wrong: it denies the reason
a declare-then-define pool exists at all, which is that some entities must be nameable before they
are complete. Applied to the next problem it produced a field with no reader. The defect the rule
was generalized from had a narrower cause -- identities were being minted for constructs no pass
would ever fill -- and that cause, stated as itself, fixes the problem without forbidding anything
sound.

So when a fix suggests a rule, write down two things instead: what the cause actually was, in that
problem's own terms, and which question exposed it. The question is what transfers.

## When the derivation contradicts the record

`decisions/` entries record why a choice was made, often with numbers behind them. Re-derive anyway,
then compare. If the derivation would reverse a decision, quote the decision's stated rationale and
argue against that rationale explicitly. Silent reversal is the failure; disagreement is not.
