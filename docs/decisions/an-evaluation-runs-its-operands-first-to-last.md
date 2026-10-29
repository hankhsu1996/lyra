# An evaluation runs its operands first to last

Date: 2026-10-28. Status: accepted.

## The requirement

> The order in which one evaluation's operands run is decided once, above every target, and every
> target runs that order whatever its own language leaves unordered.

The standard leaves the order open. The order of a call's argument expressions is undefined (LRM
13.5). An assignment's target and value are evaluated in an undefined order where no timing control
separates them (LRM 10.4.1), and where one does, the value first. A nonblocking assignment's are
evaluated "at the same time" (LRM 10.4.2). And "the ordering of assignment operations relative to
any other operation within an expression is undefined" (LRM 11.4.2), whose example gives three
answers for one expression. So the standard lets each tool answer differently, and does not ask that
one tool answer the same way twice.

That one program gives one answer on every target is therefore Lyra's own requirement, chosen as
part of the product: a design keeps its answer when the backend or the host compiler changes, a
failure seen on one reproduces on the other, and a choice the standard leaves open is one a case can
pin. The alternative -- any order the standard allows, with only undefined behaviour in the emitted
C++ removed -- was weighed and not taken. MIR listed a node's operands and stated nothing about the
order they ran in. The execution backend ran a write's target before its value and read a variable
when the operator using it ran, so `x + f()` with `f` writing `x` read what `f` had written; the C++
backend wrote each node as one C++ expression, whose operands C++ leaves unsequenced
([intro.execution]) and whose arguments it runs in an order of its own ([expr.call]), so the host
compiler chose: GCC ran two calls of one operator, and two arguments of one call, in the reverse of
clang's order. Where one of those operands read what another wrote, the C++ was also undefined
behaviour, not only a different answer.

## Decisions

**D1. A node's operands run first to last, as MIR lists them; an assignment's value runs before its
target, and a call's arguments before its callee and the object it is entered on.** Operands left to
right and a write's value before its place are Rust's order (rustc's builder, `ExprKind::Assign`:
"we evaluate assignments right-to-left"), and the one a write with an intra-assignment timing
control takes anyway (LRM 10.4.1), so every write -- blocking, delayed, nonblocking -- is one order.
A compound write's value reads its own target, whose index is evaluated once (LRM 11.4.1: `a[i]+=2`
is "same as `a[i] = a[i] +2`"), so there the target's operands run first. A read takes what its
storage holds at the moment it runs, not when the operator using it does. The object of a call and
the target of a write often stand for storage that ends with the full-expression -- a write in
progress, a part one designates -- and such a thing can be formed only where it is used, so it runs
last, after everything that could be held.

**D2. The conversion into the legalized phase leaves no node with two operands a target could run in
the other order to another answer, and the verification refuses a unit that does.** The test is the
shape of the evaluation, never whether two operands conflict, and it runs both ways: an operand
needs a temporary where what runs after it could change what it evaluates to, and where its side
effects could change what a later one evaluates to. A node has side effects unless it is a constant,
a read, a reference formed, an operation over values, a construction or the making of an execution
(D4), made of nodes without; a write, a suspension and every other call have them. What a node
evaluates to can be changed by the side effects of what runs beside it where it reads storage other
than the frame's own locals, or is a call with side effects, a write, a suspension, a choice or a
run of steps; a read of a local of the frame whose address nothing takes -- which every temporary
the conversion declares is -- can be changed only by what beside it mutates that local, by writing,
moving out of, lending or handing it to a call as storage. A local's address is taken by a borrow,
an address-of, a call binding a reference to it, and a call answering a reference through which it
can be written, as rustc's `borrowed_locals` gathers them; what a box a local holds owns counts as
that local. Taking ahead holds the operand in a temporary, as rustc's builder takes each operand
into one "so that we capture the value of `x` at this time"; it is read back as a copy where a copy
holds its value entire, as rustc consumes a `Copy` place by `Operand::Copy`, and moved where a copy
would be a second owner or nothing holds one -- a box, a payload built for the call that reads it.
Which of the two is a fact of the operand's type, never of what made it. Otherwise -- a closure,
whose captures' references were handed out where it was made, a value standing for storage
elsewhere, a run of machine values built where it is kept -- what inside it has to run first is
taken ahead in its place, held against what runs after the whole of it, and the rest is formed where
it stands. A value a borrow refers to, or an operation is entered on, that the evaluation makes is
made first in a temporary the borrow or the operation then reaches, so a view answered into it --
the C string of a string -- lasts as long as the evaluation's temporaries, as a C++ temporary lasts
to the end of its full-expression. What each node does is asked once, and each node's operands are
held against what runs after them in one pass from the last back, which the verification repeats
over the form. A call a suspension waits on stands at the top of its statement, so where one of its
arguments has to be taken ahead, the statement runs after the steps declaring the temporary: inside
a block of its own, or, where the statement declares a local, just before it in its own block, since
a block around a declaration would end the name it declares. A target is then free to run what
remains in any order its language gives: C++ needs nothing of its own, and the execution backend has
no order to choose.

**D3. Every call has side effects, whatever it calls.** No fact about what a library entry does
decides an order: a call of the library has side effects as much as a call of the program's own code
does, so reading a variable, which is a call, is held in a temporary ahead of a later operand that
runs any call. What decides is the shape of the evaluation, never a statement about what each entry
reads or changes, which would be a contract per entry checked by nothing, where a wrong one reorders
silently. What an entry answers -- a mutable reference into what it is entered on -- is another kind
of fact: it is part of the entry's signature, which every target reads to make the call, so a wrong
one fails there; it is read here only to know that what the call is entered on can be written
through the answer, which makes that local's address taken.

**D4. Three kinds of node have no side effects by what they are, each for a reason of the
runtime's.** A construction of a value the program holds -- an integral, a string, a structure --
makes storage of its own out of its operands, and what its parts do are its operands; so does a
construction of a value of the library, since what joins the engine -- a wait, a report, a
cancellation -- does so through an entry's call, never a constructor's. A call answering an
execution allocates its frame, binds its arguments and records its handle, and runs none of its
body, which starts only once it is driven: both backends make an execution stopped before its first
statement, and its promise registers nothing with the engine when it is made. An allocation touches
nothing, since an object is ended when its last share is let go of, which is a write or a move,
never when storage is next asked for; running out of memory ends the program whichever operand ran
first.

## What the field does

- **rustc** defines the order in the language and makes it explicit in building MIR: each operand is
  lowered into a temporary in the order it runs
  (`compiler/rustc_mir_build/src/builder/expr/as_operand.rs`, `as_operand`), and an assignment's
  value is built before its place (`builder/expr/stmt.rs`, `ExprKind::Assign`). A method's receiver
  runs first there, taken by reference, which its borrow checker keeps alive for the call.
- **C++** fixes some orders and leaves the rest: an assignment's right operand before its left
  ([expr.ass]/1), a call's postfix-expression before its arguments and the arguments indeterminately
  ([expr.call]/8), a braced list left to right ([dcl.init.list]/4), and the operands of most
  operators unsequenced ([intro.execution]/10). clang and GCC choose differently.
- **Nim** defines left-to-right order and compiles through C; its C-level IR makes no temporaries,
  and its maintainer's answer to the two C compilers' disagreement is that the code generator above
  it introduces them (nim-lang/nimony#792).

Where Lyra differs: MIR is a tree that a backend writes as nested expressions, not a control-flow
graph in which every operand is already a statement, so only an operand a later one could change is
taken apart -- the partial A-normal form that names a result where a later operand runs code -- and
the rest keep the shape the source wrote. And MIR has no borrow checker to keep a receiver formed
first alive while the arguments run, which is why the receiver runs last.

## Rejected

- **Each backend running the order itself** -- the C++ render writing temporaries or a sequencing
  helper. A render translates one node and makes no value of its own, and two backends each
  realizing the order is the agreement nothing holds in step.
- **Any order the standard allows, with only undefined behaviour removed.** The emitted C++ would
  barely change -- a built-in operator whose operands read and write one object written as a call,
  whose arguments C++ sequences -- but one design would answer differently by backend and by host
  compiler, which the product does not accept.
- **Stating on each library entry what it reads and changes**, as LLVM's `memory(none)` and
  `memory(read)` do, so a read beside a read keeps its shape. It is a contract per entry, more than
  a thousand of them, that nothing checks, and a wrong one reorders silently.
- **Asking whether two operands conflict**: a summary per node of what it may change and read, held
  against what runs after it, so an operand is taken ahead only where the two could be told apart.
  It took ahead no fewer operands than the shape does -- over the conformance corpus the shape's
  rule left a tenth fewer locals in the emitted C++, and less of it -- and it was a second account
  of each node's effects, which the verification computed again.
- **Taking every operand into a temporary.** A constant, a temporary, and an operand followed only
  by ones without side effects cannot be changed by what runs after it, which the shape of the
  evaluation shows; holding them too would give up the source's shape for nothing.
- **Leaving an operand that cannot be held where it stands.** It leaves its order to the target,
  which is the defect; what inside it has to run first is taken ahead instead.
- **The receiver first** (Rust's order). With no borrow checker, holding a receiver formed first
  across arguments that may end what it designates leaves it pointing at storage that has ended.
- **A write's target before its value** (Java's order). It holds a target formed before a value that
  may resize or reallocate what the target lies in, and it is the reverse of the order a timing
  control already imposes.

## Consequences

- `x + f()` where `f` writes `x` reads `x` before the call on both backends, and `h.m(f())` where
  `f` moves the handle `h` is made on the object `h` names after `f`.
- A nonblocking assignment's value is frozen before its target's coordinates.
- No library entry states what it does to memory, and a read of a variable beside any call is held
  in a temporary.
- Since an answer is now held in a local of the type MIR states, that type is the one the runtime
  answers: a wait's watched place is a type of its own rather than an untyped address, and a DPI
  buffer's data is a pointer to its chunks rather than to the buffer.
- The execution backend moves a value out of a local, which takes the local's end with it, as
  rustc's drop elaboration leaves nothing to drop in a place moved out of.
- A reference a temporary holds across a later operand is safe for the reason any reference a call
  binds is: storage the program can let go of while it is held is leased when the reference is
  formed, and the lease is kept to the end of the full-expression, where the temporaries end too.
- The rewrite after this one, deciding where each value lies, keeps the order it is handed; the
  final check proves only the form, so that rewrite states the obligation itself.
- `lyra dump mir` shows a unit as the rewrites leave it, unchecked, so what a refused unit holds can
  be read.

## Cross-references

- `a-rewrite-of-mir-says-whether-a-program-needs-it.md` -- the conversion this states the ordering
  half of.
- `a-value-too-large-for-a-frame-is-held-through-a-box.md` -- the rewrite after it, deciding where
  each value lies.
