# Shape comes from types, contents come from expressions

Date: 2026-09-01. Status: accepted.

## Why this decision matters

A lowering repeatedly meets operands that look like they need a number before the program runs: a
replication's multiplier, an assignment pattern's count, an array pattern's index key. The reflex is
to ask whether the operand is constant and to reach for its value. That reflex produced a compiler
that aborted on `{Dw-1{1'b0}}` -- the idiom LRM 11.4.12.1's own worked example uses for
parameterized code -- because the lowering pattern-matched the operand for an integer literal and
the front end had left arithmetic as arithmetic.

The repair that suggests itself is to evaluate the operand instead of matching it. That is worse: it
is constant folding, which [conversion-folding](conversion-folding.md) already assigns to the
optimizer, and it puts a second evaluator in the pipeline whose answer can differ from the one the
front end used to type the construct.

Both are answers to a question SystemVerilog never asks. The language does not classify operands by
whether they are constant. It classifies them by whether they describe a value's **shape** or its
**contents**, and it puts every shape in a type.

## The axis that decides everything: shape or contents

```systemverilog
logic [3:0][7:0] p;
p = '{2: 8'hAA, default: 8'h00};
```

Nothing in that pattern says "four". The element count is `p`'s type. The source never writes a
shape, because the type already carries it.

| The operand                           | What it describes                    | What the lowering does          |
| ------------------------------------- | ------------------------------------ | ------------------------------- |
| `{4{w}}` multiplier, packed result    | shape: the result's width            | hands it to the repeat          |
| `{n{"ab"}}` multiplier, string result | contents: a string has no width      | hands it to the repeat          |
| `'{N{items}}` count, array target     | how many elements the value has      | hands it to the repeat          |
| `'{N{items}}` count, structure target | shape the member list already states | nothing: reads the member count |
| `'{2: v}` key                         | a designator, like a member name     | carries the position it names   |
| every value written in a pattern      | contents                             | lowers the expression it is     |

The two replications are not "constant multiplier" and "non-constant multiplier". They are "the
result type fixes the size" and "it does not", which is why LRM 11.4.12.2 can allow a run-time
multiplier over a string and 11.4.12.1 cannot over packed bits. Constancy is a consequence, never
the criterion -- and nothing in the third column asks about it, because a repeat takes a count
whether or not the count is settled.

Only the structure row reads a number, and it reads it from the type. A structure's members differ
in type, so there is no repeat for the target to carry out and the items land in member positions
directly; the multiplier states the number the member list already states.

## The decisions

```text
S1. An operand is lowered as the expression it is, whatever its value turns out to be. Nothing in
    the lowering asks whether an expression is constant, and nothing evaluates one; a value fixed
    before the program runs reaches the target as a literal operand and is folded there.

S2. A number a lowering needs before the program runs is read from a type, never recovered from an
    expression. A dimension's element count, a packed value's width, a structure's member count are
    all the type saying what shape it has.

S3. A pattern key is a designator, not an operand. It names a position the way a structure pattern's
    key names a member, so what HIR carries is the position. There is no value to fail to obtain:
    the front end accepts no program whose key designates nothing.

S4. Where the language itself asks whether something is fixed before the program runs, the front
    end's answer is read and not recomputed. LRM 20.7's dimension index is the case: Syntax 20-9
    makes it an ordinary expression, and whether it has a value is what decides whether the query is
    an elaboration-time constant at all.
```

## Rejected alternatives

- **Pattern-match the operand for an integer literal.** The shape that produced the abort. It reads
  the front end's output as though a constant always arrives pre-reduced, which is true of a
  parameter reference and false of arithmetic over one.

- **Evaluate the operand where the lowering wants a number.** Folding under another name: an
  evaluation Lyra starts, over an expression the front end already typed, through the front end's
  own evaluator. It also has to choose the scope to evaluate in, which is a guess the binding did
  not have to make.

- **Carry the value on the node, read from what the front end settled.** No evaluation, and still
  wrong: the node then states a shape that its own type states, and the two can be told apart only
  by trusting them to agree. Every consumer that reached for the number turned out to want either an
  operand it could pass through or a count the type already had.

- **Split replication into a constant-multiplier node and a run-time one.** Follows from the
  rejected framing rather than from the language. The two forms differ in whether the result type
  fixes a size, and a lowering reads that from the type it already has.

## Consequences

- Nothing in the lowering asks whether an expression is constant. A multiplier and a pattern count
  are ordinary operands; the target folds them where it can.
- A replication is one node, and which operation it means follows the operand family, the way a
  concatenation's does.
- `{Dw-1{1'b0}}`, `{32-P{1'b1}}` and every other arithmetic multiplier work because nothing looks at
  the multiplier's form.
- A struct member's default value (LRM 7.2.2) remains the one place the lowering evaluates. It is
  not an operand of a program but a value a type carries, and the front end does not produce one --
  which makes it a question about the default-value model rather than about this one.

## Cross-references

- [conversion-folding](conversion-folding.md) -- constant folding belongs to the optimizer; this
  entry is why the question does not arise in the first place.
- `../architecture/hir.md` -- HIR is source-faithful, which is what makes an operand stay an
  expression.
