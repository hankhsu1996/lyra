# A choice the source lists stands side by side

Date: 2026-10-20. Status: accepted.

## The requirement

> A choice the source writes as a list of alternatives is stated, in MIR and in every target, as
> deep for thirty thousand alternatives as for two, at the cost a hand-written version has.

A case statement's items, the labels of one item, a generate case's items and the bodies a generate
loop chooses among by its index are lists in the grammar: nothing nests one item inside another, and
the front end bounds none of them. MIR stated each as an `if` whose `else` held the rest, one level
per item, so every consumer that walks a body by its nesting and the C++ target's parser recursed
once per item. A case of 30,000 items ended Lyra by stack overflow on the execution backend and
ended clang with an exhausted stack on the C++ backend; a case of 10,000 passed.

## Decisions

**D1. MIR has the switch clang and rustc have.** A machine integer, the values each case is taken
at, every value at most once, and a default body: clang's `SwitchStmt` with its stacked `case`
labels, rustc's `SwitchInt`. A case ends at the end of its body, so the order of the cases means
nothing. Both targets translate it as it is: C++ writes a `switch`, LIR a switch terminator that
LLVM's `switch` is.

**D2. A loop and a block may carry a label a `break` names.** A Rust label names a loop or a block,
and its THIR states the scope every `break` leaves. MIR's labeled break, which a `foreach` already
used, names a block as well as a loop, and every `break` names what it leaves, the source's and the
ones a lowering makes alike. C++'s own `break` leaves the nearest `switch`, so a target that writes
MIR's switch as its own needs the loop a break leaves stated rather than worked out from what stands
around it; the C++ backend writes a named break as a `goto` to a label after the statement. A loop
or a block carries a label only once a break names it, as a Rust loop carries one only where the
source wrote it, so a loop nothing leaves early reads as the loop it is.

**D3. HIR-to-MIR chooses the form from the labels.** Where the keyword is `case`, no qualifier asks
about the items together (LRM 12.5.3), and every label is a value of the subject's type known before
the run -- asked by folding the questions a run would ask of it, whether it holds an x or z bit and
what integer it reads as -- an item is taken where the subject, holding no x or z bit, equals one of
its labels (LRM 12.5). So the items are a switch over the subject's value inside a block each item
leaves by a break naming it, entered only where the subject holds no x or z bit, with the default
after the switch. A value two items name is the first item's. Every other case -- `casez`, `casex`
(LRM 12.5.1), a label holding x or z, a label the run computes, `case inside` (LRM 12.5.4), a
pattern case, a qualifier -- is a block whose arms are tried one after another: each label is a step
that runs while none before it held, and a selected arm leaves the block by a break naming it. A
label is asked what it is by lowering it into steps the statement never holds, which are then
dropped; the asking stops at the first label that is not a value known before the run, and the
search then lowers every label again where it tries it. A generate case takes the same two forms,
and a generate loop's choice among its bodies is a switch over its index.

**D4. An if-else-if stays nested, except under a qualifier asserting uniqueness.** It is nested in
the grammar, each `if` in the `else` of the one before, as clang's `IfStmt` and rustc's `If` nest
it, and the front end bounds how deeply a source may write one. A `unique` or `unique0` series
settles every arm's answer ahead of selecting one (LRM 12.4.2), so it selects among the answers it
kept by the search of D3.

**D5. How deeply a body's blocks nest is reported beside what a unit holds.** It is no count of
things held -- two units together nest no deeper than the deeper of them -- so the growth test takes
the deepest over every unit, and a design whose nesting follows its size rather than the depth the
source wrote is found there.

## Survey

- clang's `SwitchStmt` (`clang/include/clang/AST/Stmt.h`) holds an integral condition and a body
  whose `case` labels are siblings. Its parser reads a run of stacked `case` labels in a loop
  because recursing on them "can cause us to run out of stack space in our recursive descent parser"
  (`ParseCaseStatement`, `clang/lib/Parse/ParseStmt.cpp`). Its `IfStmt` holds an `else` that may be
  another `IfStmt`.
- rustc's MIR `SwitchInt` (`compiler/rustc_middle/src/mir/syntax.rs`) dispatches on an integer, each
  target value at most once. Its THIR `Match` holds a list of arms, each a pattern with an optional
  guard, which MIR building lowers into a control-flow graph; its `Break` names the scope it leaves,
  a loop or a labeled block (`compiler/rustc_middle/src/thir.rs`).

## Rejected

- **A switch whose cases hold a list of arbitrary conditions tried in turn.** No peer language of
  MIR has it, so a target spelling it has to invent the state that runs it -- the C++ backend wrote
  two flag variables -- which is a backend deciding an operation MIR never stated.
- **Keeping the nested `if` and growing the stack.** Lyra's stack would be enough, but the C++
  target would still be handed thirty thousand selection statements one inside another, which clang
  refused even with its own stack growth.

## Consequences

- A case of 30,000 constant items written in a function builds and runs on both backends. In an
  `initial` block, which is a coroutine, what limits it is the coroutine split's own cost, which
  grows with the square of the body's blocks on both backends.
- A long search, where the labels are not values known before the run, is correct at any length and
  costs one test per item it passes, as the linear search LRM 12.5 states.
- An operation over many operands the source lists -- a concatenation, a set membership, the
  arguments of `$display` on the execution backend -- is still stated one level per operand; it is a
  fold, not a choice, and this record does not reach it.
