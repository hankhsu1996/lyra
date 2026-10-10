# A real destination widens a shortreal operation

Date: 2026-10-03 Status: accepted

## The question the standard leaves open

```systemverilog
shortreal a = 1.0 + 1.0 / 8192.0;
real direct = a * a;

shortreal product = a * a;
real staged = product;
```

The exact product is `1 + 2^-12 + 2^-26`, and the last term does not fit the narrower format. So
`direct` is `1.000244140625` if the multiplication is carried out in the narrower format and widened
afterwards, and `1.0002441555261612` if the `real` destination widens the operands first. `staged`
is `1.000244140625` either way, because storing into a `shortreal` rounds.

The standard supports both readings and rules on neither.

- **The operands decide.** LRM 11.3.1: if any operand is `real` the result is `real`, "otherwise, if
  any operand ... is `shortreal`, the result is `shortreal`". LRM 11.8.1: "Expression type depends
  only on the operands. It does not depend on the left-hand side (if any)."
- **The destination decides.** LRM 11.6: the number of bits of an expression "is determined by the
  operands and the context", and for an addition "the bit length of the largest operand, including
  the left-hand side of an assignment, shall be used". LRM 11.8.2 then propagates "the type and size
  of the expression ... back down to the context-determined operands".

What no clause says is whether `real` against `shortreal` is a difference of type or of size. Read
as a type, 11.8.1's first rule governs and the destination has no say. Read as a size, 11.8.1 says
nothing about it -- that rule holds of signedness on either reading, since the left-hand side gives
an integral expression its width and never its sign -- and 11.6 governs. Table 11-21 is written over
bit lengths and has no entry for the real family.

The front end has the destination decide: it gives `direct` the wider value, `1.0002441555261612`,
and `staged` the rounded one.

The question was put to the front end's maintainer
([slang #1976](https://github.com/MikePopoloski/slang/issues/1976)). His answer: the standard is not
explicit for reals, nothing in it says the bit-length rules stop at them, and on that view the wider
value is right; on the view that those rules do not apply to reals, the narrower one is. The issue
was closed on the agreement that 11.8.1 does not settle which view holds.

## Decision

**An arithmetic operation whose operands are all `shortreal` is carried out in the format of the
assignment it stands in: `real` where the destination is `real`, `shortreal` where it is
`shortreal`.** This is the destination-decides reading, and it is what the front end elaborates.

The reason is which layer owns the answer. The type of an operator is a fact the front end resolves,
and it hands the expression over with that reading already applied: the operator is typed `real` and
each operand stands under a conversion to `real`. That is the same shape it gives `real * int`,
where the conversions are required by 11.8.2 on every reading. So nothing downstream can tell the
two apart, and a lowering that took the other reading would have to undo conversions it cannot
distinguish from ones the standard demands.
[front-end-semantic-boundary](front-end-semantic-boundary.md) D1 states the rule this is a case of:
a fact the front end resolved is translated, never recomputed.

The other reading would also split one expression in two. The front end folds a constant expression
during elaboration by its own reading, so `localparam real direct = a * a` is the wider value before
any lowering runs. A lowering that rounded the same product over variables would give the expression
one meaning folded and another run.

## What the corpus asserts

A conformance case asserts only what the text decides, so no case asserts the value of `direct`. The
case on real-family result types checks the two things both readings agree on: a `shortreal`
intermediate is rounded before the next operator reads it when the destination is `shortreal`, and a
product stored in a `shortreal` is still the rounded value after a `real` is assigned from it.

A check on `direct` expecting the narrower value is the operands-decide reading written as a
requirement. It was in the corpus, recorded against both paths as a wrong answer, and it could never
be cleared, because the answer it called wrong is one the standard permits.

## Rejected alternatives

- **Take the operands-decide reading in the lowering.** It needs the operand types the front end has
  already replaced, and it cannot separate this case from `real * int`.
- **Change the front end.** Its behaviour is deliberate and has clauses on its side. Carrying a fork
  change for a question the standard leaves open would make Lyra differ from its own front end on an
  elaboration answer every other consumer of that front end sees.
- **Keep the check and record the answer as a defect.** A defect has a right answer the tool is
  missing. This has none, so the record would stand forever and report a failure nobody can fix.

## Consequences

- `real r = a * a` over `shortreal` operands gives the wider result. A design that needs the
  narrower product in a `real` writes the intermediate into a `shortreal` first, which every reading
  rounds.
- The front end's compatibility mode, which Lyra turns on by default, does not change this: the
  wider value is what a default run produces.
- Should the standard or the front end settle the question the other way, the lowering changes
  nothing and the case gains the `direct` check.
