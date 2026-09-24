# A select names a position in the value it reaches

Date: 2026-09-23. Status: accepted.

Supersedes decisions 2 and 3 of [selector-coordinate-resolution](selector-coordinate-resolution.md)
and the rejected alternative there that resolved a coordinate at lowering; the select clause of
[packed-shape-belongs-to-the-type](packed-shape-belongs-to-the-type.md) D2 and of
[unpacked-range-belongs-to-type](unpacked-range-belongs-to-type.md); the slice clause of
[queue-operators](queue-operators.md) decision 1; and [conversion-folding](conversion-folding.md),
together with S1 of
[shape-from-types-contents-from-expressions](shape-from-types-contents-from-expressions.md) as far
as it forbids evaluating an operation over constants.

## Why this decision matters

A part-select whose bounds are literals reached the run as its two bounds and its form, each a
four-state value, plus the receiver's declared shape; the run recovered where the selected bits
start with four-state subtraction, a conversion into a canonical offset domain, and a multiply, on
every evaluation. Measured on the representative compute block: about 2,100 instructions a select
and 43% of the block. A conversion of a constant was rebuilt every time control reached it as well
-- `acc + 1` converted its `1` twice per evaluation -- because nothing folded it.

Both are one fact: what the source fixed before the program runs reached the run as values to work
out again.

## The model

```
SV:   opcode = int'(val[2:0]);        // logic [31:0] val
      c = w[i +: 4];                  // logic [0:31] w
      d = a[k];                       // int a[1:8]
      acc = val + 1;

MIR:  slice(val, 0, 3)                // position 0, three bits
      slice(w, 28 - to_position(i), 4)
      element(a, to_position(k) - 1)
      val + 1'(logic [31:0])          // one constant of the unit
```

A select states where it lands in the selected value's own numbering -- a packed value's bits from
the least significant, an unpacked array's elements from the left -- and how many parts a fixed-size
run takes. The declared range, its direction, and which part-select form the source wrote are read
at HIR to MIR, which is the last layer that knows them.

## The decisions

**P1. A select's operands are a position and, for a fixed-size run, a count.** Every family reads
the position the same way: an integral value whose x or z bit, or whose magnitude past every value a
design can declare, names no position -- which LRM 11.5.1 and 7.4.5 answer the same way as an index
out of range. The count is a number the select's own result type fixes (LRM 11.5.1: a part-select's
width is constant), so it reaches the call as a machine count. A queue's slice is bounded by two
positions instead, because `$` lets the running program move them (LRM 7.10.1), and the queue clamps
them.

**P2. Translating a declared index into a position is HIR to MIR's, and it is arithmetic in a
position type.** Where the declaration numbers from the index's own zero -- `[N-1:0]`, `[0:N-1]`, a
dynamic array, a queue -- the index is the position as it stands. Otherwise the index is brought to
a 64-bit signed four-state position, where no shift a declaration can ask for wraps it and an
unknown index stays unknown, and the declaration's origin, direction and element width are applied
there as ordinary MIR arithmetic.

**P3. An integral operation whose operands are all constants of the unit is a constant of the
unit.** The builder that states an operation asks whether it folds, the way LLVM's `IRBuilder` asks
its `ConstantFolder` (`llvm/IR/IRBuilder.h`), and the answer is computed by the value library the
run itself uses, so a folded operation and an unfolded one cannot disagree. An operation that can
fail while running is not folded, since whether it fails is a question for a run that reaches it.

**P4. What a deferred write freezes is where the part is, never how many parts it takes.** A count
is a number its type fixes, so it travels beside a descent step's coordinates rather than among
them, and nothing snapshots it.

## What established systems do

- **Verilator** (its width-selection pass, `V3WidthSel.cpp`, read 2026-09-23) replaces every
  `SELPLUS`, `SELMINUS`, `SELEXTRACT` and `SELBIT` with `AstSel(from, lsb, width)` during width
  resolution, before any code is generated: the declared range is applied to the index as AST
  subtraction (`newSubNeg`), and `V3Const` folds it where the index is constant. `AstSel`'s own
  comment: "Always const width".
- **CIRCT** `comb.extract` (circt.llvm.org/docs/Dialects/Comb, read 2026-09-23) takes the low bit as
  an attribute and the width from its result type; declared ranges do not exist in the dialect.
- **LLVM** `IRBuilder` (`/usr/include/llvm-20/llvm/IR/IRBuilder.h`, read 2026-09-23) defaults its
  folder to `ConstantFolder`, and `CreateAdd`, `CreateTrunc` and the rest return a constant when
  every operand is one.
- **slang** evaluates a range select's bounds and translates them per evaluation
  (`RangeSelectExpression::evalRange`), which is right for a constant evaluator that runs once at
  elaboration. The condition that differs is that ours runs on every simulation event.

The field agrees on where the decision lives: the component that still knows the declaration turns a
coordinate into an offset once, and what reaches execution is an offset and a width.

## The records this reverses, and why

[selector-coordinate-resolution](selector-coordinate-resolution.md) rejected resolving a coordinate
at lowering for three stated reasons. "Every caller must remember its coordinate system and
pre-rebase": there is one caller, the select's own lowering, and it holds the static type that is
the coordinate system. "The rebase is synthesized as an operator on the selector's own narrow,
possibly four-state type": P2 does it in the position type, which is exactly the wide, x-aware
domain decision 2 asked for, entered once. "A select node carries a storage computation rather than
a semantic selection": a position in the value's own numbering is not a storage layout, and a
declared range with a direction and a part-select form is the source language reaching MIR, which
`mir.md`'s Purpose places at HIR to MIR. Its requirement set had no run-time cost in it; the
measurement above is what adds one.

[unpacked-range-belongs-to-type](unpacked-range-belongs-to-type.md) named one requirement for the
range operand: staying correct if unpacked dimensions become values supplied at construction. P2
meets it -- the range's endpoint then becomes an operand of the arithmetic rather than a constant,
and the arithmetic stops folding.

[queue-operators](queue-operators.md) resolved `base +/- (w - 1)` in the queue "in the wide int64
domain, never synthesized at lowering". The concern is wrapping, which the position type answers.

[conversion-folding](conversion-folding.md) and S1 of
[shape-from-types-contents-from-expressions](shape-from-types-contents-from-expressions.md) forbade
a lowering from evaluating an operand, for three reasons. Pattern-matching an operand for a literal
aborted on arithmetic: P3 matches nothing, and builds the operation whenever it cannot evaluate it.
A second evaluator could disagree with the front end: P3 uses the value library that runs the
program, and the type every constant takes is the one the front end already gave the node. Folding
belongs to the target, which folds for free:
[a-constant-is-stated-not-computed](a-constant-is-stated-not-computed.md) measured that it does not,
at three optimization levels. S2 of that record stands and P1 follows it: every count a select needs
comes from a type.

## Rejected alternatives

- **Keep the coordinate in MIR and resolve it in machine integers in the runtime.** The unpacked
  family already did this and it is cheap. It leaves the declared range, its direction and the
  part-select form in MIR as an opaque payload, keeps a per-evaluation resolution of a fact the
  compiler settled, and keeps four spellings of the rule -- one per family -- where P2 has one.

- **A position as a machine integer beside a known flag.** Exact, but a position composed from
  several indices, or a position the program computes, needs the flag threaded through every step,
  where the four-state value carries unknown through ordinary arithmetic.

- **Restate a literal operand in a deferred write's body instead of freezing it.** It made the one
  failure the change met go away by matching operands for literals, the shape S1's own history
  rejects; P4 removes the need.

## Consequences

- Measured 2026-09-23 by instruction count under callgrind, same work, against `212505b0`: the
  representative compute block 1,200,065,357 -> 829,304,434 (-30.9%); scalar arithmetic
  7,380,008,890 -> 6,408,004,232 (-13.2%). Rates the same day: 3,237 -> 4,711 table passes per
  second (24x off Verilator, from 35x), and 6,684,983 -> 6,804,700 iterations per second.
- The runtime's packed select is one extraction by position; the four-state rebase, the slice form
  and every select's shape or range operand are gone from both backends.
- `$rose` and `$fell` read bit zero of the value's own numbering, which is the least significant bit
  whatever range the value was declared with (LRM 16.9.3).
- Whether a packed value's width belongs in the emitted code, or should become a value supplied
  while the program runs, is still open, and this settles neither. A position computed from a
  declared range that arrives while running is ordinary MIR arithmetic already, so it would keep
  working and stop folding. The count is read from the result type, so it assumes what every MIR
  integral type already assumes -- a width fixed per specialization -- and would become an operand
  together with the types if widths ever stop being fixed.
- A memory file still states addresses in the declared coordinates (LRM 21.4), so reading one is the
  one place the runtime applies a declared range, and it does so to name a position.

## Cross-references

- LRM 7.4.5, 7.4.6, 7.10.1, 11.5.1, 11.8.1.
- `../architecture/mir.md` -- what a select states.
- [a-constant-is-stated-not-computed](a-constant-is-stated-not-computed.md) -- the pool a folded
  constant lands in.
