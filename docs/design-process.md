# Design process

How to arrive at a design in this codebase, and how to falsify one. Comment rules live in
`code-comments.md`; doc-writing rules live in `style.md`.

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

## Four searchable smells

Each is the question above, pre-applied to a shape that recurs. Each has been a real defect here.

**1. A `default:` or fallback arm that throws.** Whatever the arm cannot name, some upstream
producer knew and did not write down. Do not add the missing case -- carry the fact.

**2. A `bool` or predicate that selects between two output shapes.** Two shapes means every
downstream consumer handles two cases, and the case a consumer forgets is a defect the other case
hides. Find the one shape that covers both.

**3. A pass that runs before the real work to compute a property.** The property is a decision, and
it is being made at the layer that has the least information about it. Move it to the layer that
already held the inputs, or discover it was never a decision at all.

**4. An `optional<T>` field, or an `if (empty)` branch.** Ask whether "absent" is genuinely
reachable. Usually the thing always exists and only its _name_, _contents_, or _exposure_ is
optional -- three different questions that an `optional` on the whole thing has collapsed into one.

## Falsifying a proposed shape

Two checks, both cheap:

- **Could a mechanical LLVM IR backend translate this without deciding anything?** If a consumer
  needs an `if` to work out what a node means, the node is under-specified. This is the sharpest
  check available and it applies well beyond backends.
- **Does the empty case fall out of the general case?** Write the loop for N and check that N=0
  needs no branch. If it does need one, the data model is carrying two cases through code that
  should carry one.

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
