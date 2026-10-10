# A write names a place or a join of lvalues

Date: 2026-10-09 Status: accepted

## Context

IEEE 1800-2023 A.8.5 defines what may stand on the left of a write recursively and in four forms: a
select of a variable or a net, a concatenation of lvalues, an assignment pattern of lvalues, and a
streaming concatenation. A.6.2 puts an assignment operator on any of them, Table 10-1 puts a
concatenation or a nested one on the left of a continuous assignment, 23.3.3 makes a port connection
a continuous assignment and names a concatenation as what an output connects to, 13.5 makes an
`output` or `inout` actual anything a procedural assignment may have on its left, and 10.6 gives
`assign` and `force` a concatenation of variables.

The lowering modelled what a write names as one place. A concatenation had a second path of its own,
for a blocking or nonblocking assignment in a procedure and nowhere else, and a stream a third, for
an assignment statement alone. Every other kind of write asked for one place and refused a join, and
an assignment operator on a concatenation was reported as a compiler bug with a citation of the
clause that makes it legal. That refusal was what stopped sixteen of the seventeen designs of the
benchmark suite the project builds.

## Requirement

Whatever a write names, each destination it is made of receives its share of the one value written,
by that same kind of write, with the value evaluated once and every destination located once.

## How the field does it

Two concepts meet in the construct. Every language surveyed has the first, and only some the second.

**Destructuring assignment.** The left side is a recursive shape whose leaves are places, the right
side is evaluated once, and each leaf takes its part. Rust names the left side an _assignee
expression_ -- "place expressions, underscores, tuples of assignee expressions, slices, structs" --
and its reference says the form "always decomposes into sequential assignments to place expressions,
which may be considered the more fundamental case"; `rustc_ast_lowering`'s `lower_expr_assign`
rewrites `(a, b) = t` to `{ let (l1, l2) = t; a = l1; b = l2; }` where the AST becomes HIR,
returning early for an ordinary left side. Python's language reference (7.2) defines a `target_list`
the same way and its compiler emits one unpack and one store per target. Go's specification makes it
"two phases": the index operands on the left and the expressions on the right are all evaluated,
then the assignments are carried out in order. All three refuse an assignment operator on such a
left side.

**An lvalue that is not one address.** Something with a load and a store that the rest of the
compiler uses without asking its kind. Clang's code generator has one `LValue` with kinds -- simple,
bit-field, vector element, a swizzle naming scattered components of a vector -- and
`EmitLoadOfLValue` and `EmitStoreThroughLValue` dispatch on the kind, so a compound assignment is
load, operate, store over any of them. Loading a swizzle gathers its components and storing scatters
them, which is what the two directions of a join are.

The SystemVerilog implementations use the second for the first:

| Implementation        | Where the join is made                                                                     | Where it is taken apart                                                                                                                                                                                                                 | Assignment operator                                                                         |
| --------------------- | ------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------- |
| slang's evaluator     | an `LValue` is a path or a concatenation of `LValue`s, packed or unpacked, recursive       | `store` divides the value among the elements, most significant first; `load` joins them                                                                                                                                                 | the lvalue is located, the right side reads it through `load`, then `store`                 |
| CIRCT (Moore dialect) | a `concat_ref` operation at import                                                         | one pass before the core lowering flattens nested ones, extracts each leaf's bits from the source, and clones the original assignment per leaf -- one template instantiated for the continuous, blocking, nonblocking and delayed forms | not handled there                                                                           |
| Verilator             | a concatenation node on the left                                                           | constant folding splits it into one assignment per side "of same flavor as old one", through a temporary when the right side is impure or reads a target; a pin and a task's output become assignments first                            | rewritten to `l = l op r` by cloning the left side, with a special case for an impure index |
| Icarus                | every l-value is a chain of parts and a concatenation is the chain of its operands' chains | never: the code generator walks the chain                                                                                                                                                                                               | the operator stays on the statement with the whole chain                                    |

What is common: the leaves are found once, a nested packed join comes out flat, the first operand is
most significant, the join is gone before the layer where a write has one destination, and one piece
of code serves every kind of write. Lyra's conditions do not differ: HIR-to-MIR is its desugaring
layer, and a port connection is already a continuous assignment here. The one thing SystemVerilog
has that Rust, Python and Go refuse is the operator form, which 11.4.12 makes meaningful by calling
the join "a packed vector of bits", and which slang's `load` covers.

## Decision

**D1. In the lowering, what a write names is an lvalue: one place, or a join of lvalues.** A join is
recursive, as the grammar is, and has a kind that says how a value is cut among its members and how
what they hold is joined:

- _packed_ -- a concatenation, or an assignment pattern of an integral type. The members share the
  bits of one vector, the first member most significant, each as many as it is wide.
- _unpacked_ -- an assignment pattern of a structure or a fixed-size array. Member `i` takes member
  or element `i`.
- _streamed_ -- a streaming concatenation. The value is read as a stream of bits, the order of its
  blocks is reversed where the operator says so, and the members are filled from the most
  significant end.

The names are slang's, which is the one implementation that names this role for this language.

**D2. Every kind of write asks the lvalue for its shares and makes its own kind of write to each.**
A share is one place and the value it takes. An assignment stores each; a nonblocking one freezes
them all into one update, so a control on it is read once; a continuous assignment attaches a driver
to each place that is a net and stores each share on every evaluation; an output actual stores them
when the call returns; a takeover is begun on each place and one evaluation of the source drives
each its share, ending when no place says that evaluation still owns it. No kind of write asks which
form the source wrote. A value written to a join is held once before it is cut.

**D3. A write that reads first settles every place once and reads what they hold together.** An
assignment operator, an increment and an `inout` actual take the join's value from its members --
their bits end to end, or the aggregate they are the members of -- with each member's index
evaluated once for the read and the write (LRM 11.4.1).

**D4. Every place of a join is located before the first is written.** In `{i, a[i]} = v` the second
member is the element `i` named when the statement was reached. LRM 10.4.2 fixes that for a
nonblocking assignment and the standard says nothing for a blocking one; Go's two phases and slang's
evaluator both locate first, and it makes the two forms name the same places.

**D5. MIR carries none of this.** What reaches it is a local holding the value, reads of parts of
it, and writes to one place each. No peer language has a place that is several places, and a backend
that met one would have to decide the cut.

**D6. A write to one place stays the store it was.** An assignment whose left side is one place is
lowered as before, with no local and no block; only a join becomes a sequence of steps. This is
rustc's early return for an ordinary left side.

## Rejected

- **A join as a target in MIR, distributed by each backend.** It fails MIR's purpose, since no
  language MIR is a peer of has such a place, and it puts the cut in two places that must agree.
- **Cloning the left side to read it** (Verilator's `l = l op r`). An expression named twice is
  evaluated twice, which is the defect LRM 11.4.1 rules out and the one this project has met
  repeatedly; settling once and loading is the form that cannot make it.
- **A flat list of parts as the only shape** (Icarus). It is the same model for the packed kind,
  where nesting is associative. An assignment pattern whose member is itself a pattern is not
  associative, so the recursion is kept and the packed kind flattens by it.
- **Keeping the join a special case of the assignment statement.** That is what was there: each
  further position is then a refusal, and each refusal a design that does not build.

## Consequences

- A concatenation, an assignment pattern and a stream are each a target in every position the
  language lets a write stand.
- The procedure-only path for a concatenation, the statement-only path for a stream, and the
  refusals in the one-place lowering are gone. A `ref` actual, a method's receiver and a wait still
  ask for one place, and a join arriving there is something the front end refuses.
- A blocking assignment to a concatenation whose later member's index an earlier member changes now
  writes the element named when the statement was reached, where it wrote the one named after the
  earlier member's store.
- A takeover on part of a variable or of a net is refused inside a join as it is outside one: a
  takeover is state on whole storage, and LRM 10.6.2's constant part-select of a net is not carried.
- A dynamically sized value in a stream target is refused as it is anywhere a stream meets one,
  since no type yet names a bit count the program fixes.
- A seed argument that is not a variable is refused with the clause that requires one (LRM 20.14.1),
  where it reached an internal error.
