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
   result's element count (11.4.11, 7.4.5), never the range of an operation's result; an unbased
   unsized literal is as wide as its context and sets every bit (5.7.1), so it has no range either,
   and the front end gives it its context's type whole; it lists a procedure's implicit sensitivity
   as the longest static prefixes the text reads (9.2.2.2.1, 11.5.3), with no exception for a branch
   a constant decides or a select past its value's end; and it defines a select past its value's end
   by its value, whether its index is constant or not (11.5.1, 7.4.5).
4. A reduction of the field: rustc shrank an instance's identity to the generic parameters a body
   uses by analysing the body, and removed it (rust#133883) because that analysis could not be kept
   right; what replaces it keys code by the layout of its data and passes the rest as a hidden
   argument. Build systems narrow a key to what an action read by recording the reads as it runs
   (ccache's manifest, Bazel's discovered inputs), never by predicting them.
5. The field lowers an application from its definition and its arguments and nothing of where it
   stands: rustc's `Instance` is `{def, args}` (`rustc_middle/src/ty/instance.rs`) and a body is the
   definition's MIR instantiated with `args`; clang finds a class template specialization by its
   arguments and keeps the point of instantiation only as a location, [temp.point] making a program
   whose points of instantiation mean different things ill-formed. slang shares an elaborated body
   between instances agreeing on definition, parameter values and the instances their interface
   ports connect to (`InstanceCacheKey`). Where SystemVerilog differs is that a body reaches out of
   its instance -- a name searched upward (LRM 23.8), a disable naming a block outside (LRM 9.6.2),
   a class an enclosing module declares and hands down as a type (LRM 6.20.3) -- so where it lands
   is one of its arguments.
6. The lowering runs under several namings, and the one specializations are compared in names an
   instance by where it stands among those its key reaches; no function of a specialization's name
   gives that name. So a target the front end resolved the body's text to is named through the
   stage's naming, from the instance, which is identity.
7. rustc puts a closure's environment into its identity: `ClosureArgsParts` holds the parent's
   arguments, the closure kind, its signature and the tuple of its captured variables' types
   (`rustc_type_ir/src/ty_kind/closure.rs`), and code generation reads those rather than working
   them out again. slang shares no elaborated body that has an upward name at all
   (`source/ast/ElabVisitors.h`). An outside review of this design held that a key one walk works
   out and a lowering that asks the front end again are two authorities on one meaning, and that a
   missing fact must be an error rather than a fallback to the representative instance.

## Decision

**D1. The lowering reads from an instance's tree only what every instance of its identity shares.**
That is the expression tree as written, the type a declaration states, a type the front end read as
a fact, and a constant the language requires, each where the front end recorded what it read; the
identity holds the values of the parameters so recorded. Everything else it needs it works out from
those.

**D2. What has no range of its own is typed by its width, its signedness and its element count,
numbered from zero.** That covers an operator, a conversion, an assignment, a part-select, a
concatenation and its kin, a system subroutine's result, and an unbased unsized literal, which takes
its context's type even where that context is a part-select an index places. A name, a member, an
element, a user subroutine's result and a pattern keep the type their declaration states, since a
select or a key is read against its range. One function decides which an expression is, and nothing
in the lowering interns an expression's front-end type past it. A query (LRM 20.6.1, 20.7) is not
such a reader: it reads the type the front end settled for its operand, which the front end recorded
where the query is bound, so one expression has one answer wherever a query on it stands.

**D3. What a node reads and writes is collected from its text.** Each name is marked read or written
by where it stands, and states the longest static prefix the source wrote. Nothing is split into bit
runs, merged by value, or dropped where a constant index falls outside its value. Where a prefix
lands, and taking what a procedure writes out of what it reads (9.2.2.2.1 b), are answered where the
prefix is evaluated.

**D4. Lowering every instance and comparing stays a test.** It is what finds a read D1 missed, and
no build relies on it. Each instance is lowered on its own, from its own body, and what it comes out
as is held equal to its specialization; the lowering never holds two bodies at once.

**D5. An instance is its body and its environment, and the lowering reads the instance through
nothing else.** The environment is everything the body's text reaches outside itself: where each
name it writes lands once it leaves the instance, where it starts reaching each class or covergroup
another instance declares, the constant each parameter of another instance it reads settled to or
that each object of that instance holds it, what the parent bound to each interface port, and which
of its own parameters it is handed. It is worked out once per instance, by the stage that works keys
out, and what the body reaches outside itself comes from one walk of the body that visits everything
the lowering translates -- a disable's target, a sequence reached by a hierarchical name, the values
an instantiation assigns a child's parameters, a loop's scheme and a name a view defines with an
expression included. The key renders it, so instances of one key have one environment; the lowering
is handed it, takes every out-of-body fact from it, and treats a fact it asks for and does not find
as this compiler's defect. A name whose declaration and the head of its path stand in no other
instance never leaves the instance and is not looked up. This is what rustc does for a closure,
whose captured environment is a typed part of its arguments (fact 7): identity discovery and
translation do not each ask the front end what the text means. A top a value was given to elsewhere
has nothing to hand it that value (LRM 23.3.1), so it compiles the value in and its key says so;
every other top is built as an instance left to its defaults is.

**D6. Identity from the front end; content from what the other unit published.** The lowering names
any instance, block or declaration the front end resolved the body's text to through the stage's
naming (fact 6). A default a connection takes is told by the connection resolving to the port's own
initializer, and its value is the child's published default.

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
