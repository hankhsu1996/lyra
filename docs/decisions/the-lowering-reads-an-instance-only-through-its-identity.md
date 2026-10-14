# The Lowering Reads An Instance Only Through Its Identity

## Date

2026-10-13

## Status

Accepted. Revises [read-set-inference](read-set-inference.md),
[a-part-is-the-select-the-source-wrote](a-part-is-the-select-the-source-wrote.md) D2 to D4, and
where [front-end-semantic-boundary](front-end-semantic-boundary.md) D2 and
[event-control-unification](event-control-unification.md) take a read set from.

## Requirement

Every instance of a design element, and every block of a generate loop, that one identity names
lowers to the same thing, because the lowering reads nothing of it that the identity does not hold
-- not because a check afterwards finds the copies alike.

## Facts it rests on

1. The front end elaborates every instance into a tree of its own and folds constants into it as it
   binds: a select's result range from the base it settled, an operator's type handed up from an
   operand, which branch a constant condition leaves dead, which reads its flow analysis keeps. It
   records which parameters an evaluation read where the language required a constant, and keeps
   beside each constant it folded what that fold read. A type settled from such folds carries none
   of it, so where it binds something that reads a type as a fact -- the type `type(...)` declares
   (LRM 6.23), an argument a system subroutine reads for its type alone (LRM 20.6, 20.7), the part a
   port names (LRM 23.2.2.1) -- it records what every fold inside that expression read, and it
   charges the values a site hands a class specialization to the instance writing the site.
2. Two instances whose identities are equal therefore hold trees of one shape -- a shape differs
   only where a required constant or a declared type does, both of which the identity holds -- but
   those opportunistic answers differ between them. Lowering one of them for both is right only if
   the lowering never reads those answers.
3. The standard fixes an operation's width and signedness (LRM 11.6.1, 11.8.1) and an unpacked
   result's element count (11.4.11, 7.4.5), never the range of an operation's result; it lists a
   procedure's implicit sensitivity as the longest static prefixes the text reads (9.2.2.2.1,
   11.5.3), with no exception for a branch a constant decides or a select past its value's end; and
   it defines a select past its value's end by its value, whether its index is constant or not
   (11.5.1, 7.4.5).
4. A reduction of the field: rustc shrank an instance's identity to the generic parameters a body
   uses by analysing the body, and removed it (rust#133883) because that analysis could not be kept
   right; what replaces it keys code by the layout of its data and passes the rest as a hidden
   argument. Build systems narrow a key to what an action read by recording the reads as it runs
   (ccache's manifest, Bazel's discovered inputs), never by predicting them.

## Decision

**D1. The lowering reads from an instance's tree only what every instance of its identity shares.**
That is the expression tree as written, the type a declaration states, a type the front end read as
a fact, and a constant the language requires, each where the front end recorded what it read; the
identity holds the values of the parameters so recorded. Everything else it needs it works out from
those.

**D2. What an operation yields is typed by its width, its signedness and its element count, numbered
from zero.** That covers an operator, a conversion, an assignment, a part-select, a concatenation
and its kin, and a system subroutine's result. A name, a member, an element, a user subroutine's
result and a pattern keep the type their declaration states, since a select or a key is read against
its range. One function decides which an expression is, and nothing in the lowering interns an
expression's front-end type past it. A query (LRM 20.6.1, 20.7) is not such a reader: it reads the
type the front end settled for its operand, which the front end recorded where the query is bound,
so one expression has one answer wherever a query on it stands.

**D3. What a node reads and writes is collected from its text.** Each name is marked read or written
by where it stands, and states the longest static prefix the source wrote. Nothing is split into bit
runs, merged by value, or dropped where a constant index falls outside its value. Where a prefix
lands, and taking what a procedure writes out of what it reads (9.2.2.2.1 b), are answered where the
prefix is evaluated.

**D4. Lowering every instance and comparing stays a test.** It is what finds a read D1 missed, and
no build relies on it.

## Limits

- A read only an unreachable path holds wakes the procedure that holds it (fact 3 lists it).
- A select past its value's end, in a net lvalue or a port expression, is still refused per instance
  where the front end settled no position for it.
- A query whose type a parameter settled makes that parameter an argument, whatever the query reads
  of the type, so `$bits` of a part-select a loop index places splits the blocks although its answer
  does not.

## Rejected

- **Record every evaluation the front end makes, and key on all of it.** Complete, but a part-select
  base the front end folds only to name a range the lowering renumbers would split every block of a
  loop by its index again.
- **Answer a query on an operation from the type numbered from zero.** The front end answers the
  same query from its own type wherever a constant is required, so `$left` of one part-select would
  be 7 in running code and the settled bound in a declaration's range: two answers in one program.
- **Find the parameters a type operator's expression names by their spelling.** It sees only a
  `type(...)` written at the top of a declaration, and a query in a range reads the same settled
  type without one.
- **Find, before lowering, the queries the lowering will answer itself, and charge their types.** A
  walk predicting what the lowering reads is the analysis this record rejects in fact 4; the front
  end binds every query wherever it stands, so it records the reads there.
- **Record the provenance of every fact the front end settles, and read facts through one
  accessor.** A key holding the constant settled at each place needs it; a key holding parameters'
  values needs only which parameters something relied on, which the front end's record already
  answers.
- **Keep the front end's flow analysis and patch the drop of an out-of-range read.** The bit runs,
  their count and the merge by value would still differ between instances.
