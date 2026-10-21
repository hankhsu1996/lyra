# A list the source writes stands side by side

Date: 2026-10-20. Status: accepted.

## The requirement

> A list the source writes -- the alternatives of a choice, or the operands of an operation -- is
> stated, in MIR and in every target, as deep for thirty thousand items as for two, at the cost a
> hand-written version has.

A case statement's items, the labels of one item, a generate case's items and the bodies a generate
loop chooses among by its index are lists in the grammar: nothing nests one item inside another, and
the front end bounds none of them. So are the operands of a concatenation, the members of a set, the
arguments of a call, the clauses of a condition joined by `&&&`, and what the compiler lists for
itself over a structure's members to compare, stream, measure or print one. MIR stated each as an
`if` whose `else` held the rest, or as an operation whose first operand held the rest, one level per
item, so every consumer that walks a body by its nesting and the C++ target's parser recursed once
per item. A case of 30,000 items, a concatenation of 30,000 operands and a structure of 30,000
members each ended Lyra by stack overflow; a case of 10,000 passed.

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
search then lowers every label again where it tries it. A generate case takes the same two forms, a
generate loop's choice among its bodies is a switch over its index, and so is an instance array's
choice of how each element is built, where something written elsewhere makes its elements differ.

**D4. An if-else-if stays nested, except under a qualifier asserting uniqueness.** It is nested in
the grammar, each `if` in the `else` of the one before, as clang's `IfStmt` and rustc's `If` nest
it, and the front end bounds how deeply a source may write one. A `unique` or `unique0` series
settles every arm's answer ahead of selecting one (LRM 12.4.2), so it selects among the answers it
kept by the search of D3. The same holds of an operator chain the source nests -- `a || b || c`,
`a + b + c` -- which is as deep as the source wrote it.

**D5. An operation over a list is stated as a person writes it by hand, in one of three shapes
chosen by what it is.**

- A join ([concatenation-realization](concatenation-realization.md) decision 4): of bits, a local of
  the joined type and one write of each part into the bits it takes; of characters, one call over
  one list of strings; of queue elements, a queue the steps append each part to.
- A fold that evaluates every operand -- the members of a set, which LRM 11.4.13 calls "effectively
  the OR reduction of all the comparisons", a structure's equality, which is unknown where no pair
  of members is unequal and one is unknown (LRM 11.4.5), and the sum of a structure's members'
  widths or bit counts -- is a local holding the operator's identity and one statement per operand
  folding it in, `r = r || t`; the four-state logical operators already give both clauses' x rule.
- A search that stops at the first condition settling it -- the tests of a pattern (LRM 12.6), whose
  later tests may read a member only its tag makes live, the clauses of a condition joined by `&&&`
  (LRM 12.6.2), and the conjuncts of an assertion's position -- is steps that each try one condition
  while the answer is open, whether or not a condition can be unknown. What the compiler writes for
  a structure's own whole-value methods asks one member per statement, returning at the first that
  settles the answer.

What the steps leave to run when a call that has many arguments departs is a table MIR-to-LIR builds
by a loop, each scope's entry continuing to the one around it, as clang pops its cleanup stack and
rustc walks its drop tree.

**D6. How deep a body nests is reported beside what a unit holds.** It counts each block one level
inside the block holding it and each expression as many levels inside its block as it stands on. It
is no count of things held -- two units together nest no deeper than the deeper of them -- so the
growth test takes the deepest over every unit, and a design whose nesting follows its size rather
than the depth the source wrote is found there.

## Survey

- clang's `SwitchStmt` (`clang/include/clang/AST/Stmt.h`) holds an integral condition and a body
  whose `case` labels are siblings. Its parser reads a run of stacked `case` labels in a loop
  because recursing on them "can cause us to run out of stack space in our recursive descent parser"
  (`ParseCaseStatement`, `clang/lib/Parse/ParseStmt.cpp`). Its `IfStmt` holds an `else` that may be
  another `IfStmt`. Its `CallExpr` and `InitListExpr` hold their operands as one array, and it
  refuses a fold expression whose expansion is deeper than the bracket depth, because a fold builds
  one binary node per element (`TransformCXXFoldExpr`, `clang/lib/Sema/TreeTransform.h`). Its
  cleanups are a stack popped by a loop (`PopCleanupBlocks`, `clang/lib/CodeGen/CGCleanup.cpp`).
- rustc's MIR `SwitchInt` (`compiler/rustc_middle/src/mir/syntax.rs`) dispatches on an integer, each
  target value at most once. Its THIR `Match` holds a list of arms, each a pattern with an optional
  guard, which MIR building lowers into a control-flow graph; its `Break` names the scope it leaves,
  a loop or a labeled block (`compiler/rustc_middle/src/thir.rs`). A call's arguments and an
  aggregate's operands are one list (`Box<[ExprId]>`), lowered by one loop over it, and the drops an
  unwind runs are a flat table walked by a loop (`DropTree`,
  `compiler/rustc_mir_build/src/builder/scope.rs`).
- Both compilers realize a list of operands built into one value by storing each into its place:
  clang's `AggExprEmitter::VisitInitListExpr` (`clang/lib/CodeGen/CGExprAgg.cpp`) and rustc's
  `Rvalue::Aggregate` (`compiler/rustc_codegen_ssa/src/mir/rvalue.rs`) each loop over the operands
  and store one per field, a bit-field into its bits. LLVM's `APInt` writes many parts by
  `insertBits` into a value sized up front (`llvm/ADT/APInt.h`).
- abseil's `StrCat` of five or more pieces converts each to a view and hands `CatPieces` one list,
  which sums the sizes, allocates once and copies each piece once (`absl/strings/str_cat.cc`).
  CPython 3.12 compiles `f"{a}{b}{c}"` to one `BUILD_STRING 3`, `[a, *b, c]` to `BUILD_LIST` then
  `LIST_EXTEND` and `LIST_APPEND`, and `a in (x, y, z)` to the tuple and one `CONTAINS_OP`.

## Rejected

- **A fold as the reduction of the truths joined**, one join of the truths and one reduction of the
  joined bits. It is flat, but it builds a value as wide as the list only to reduce it, and it leans
  on a join that is itself one write per part; the accumulated fold is the same statements without
  the vector.
- **A switch whose cases hold a list of arbitrary conditions tried in turn.** No peer language of
  MIR has it, so a target spelling it has to invent the state that runs it -- the C++ backend wrote
  two flag variables -- which is a backend deciding an operation MIR never stated.
- **Keeping the nested forms and growing the stack.** Lyra's stack would be enough, but the C++
  target would still be handed thirty thousand selection statements, or a call thirty thousand calls
  deep, one inside another, which clang refused even with its own stack growth.
- **Nesting the list as a balanced tree.** A fold of thirty thousand would stand fifteen deep rather
  than thirty thousand, but the depth would still follow the list, and the order a search takes
  would be harder to read off it; the steps above are as deep for any length.

## Consequences

- A case of 30,000 constant items, a concatenation of 30,000 operands of bits, strings or queue
  elements, a set of 30,000 members and a call of 30,000 arguments build and run on both backends in
  a function. In an `initial` block, which is a coroutine, what limits each is the coroutine split's
  own cost, which grows with the square of the body's blocks on both backends.
- A long search, where the labels are not values known before the run, is correct at any length and
  costs one test per item it passes, as the linear search LRM 12.5 states.
- A join writes each part once, where the chain it replaced made a value as wide as everything
  joined so far at every step.
